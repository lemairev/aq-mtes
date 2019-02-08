server <- function(input, output, session) {

  # read data ---------------------------------------------------------------
  # prevair concentrations ----
  pol_data <- reactive({
    req(input$date, input$pol, input$stat)
    
    # ifile (date_real take into accound lead)
    tmp <- file_tbl %>% 
      filter(date_real == input$date & stat == input$stat & pol == input$pol) %>% 
      dplyr::select(name, lead) # %>% pull
    
    # return raster & lead time
    list(r = raster(as.character(tmp[,"name"])), lead = as.numeric(tmp[, "lead"]))
  })
  
  # monitoring stations concentrations ----
  pol_data_ts <- reactive({
    req(input$date, input$pol)
    
    ifile <- file_tbl_ts %>% 
      #filter(date == input$date & pol == input$pol) %>% 
      filter(date == "2018-12-21" & pol == input$pol) %>% 
      dplyr::select(name) %>% 
      pull()
    
    # read, clean accent & build  
    read_csv(ifile) %>% 
      mutate_at(
        .vars = vars(c("station_localid","network_name", "station_name")),
        .funs = iconv, from = "latin1", to = "UTF-8"
      ) %>% 
      filter(samplingpoint_y > 20) # rm domtom
  })
  
  # emissions ----
  emi_data <- reactive({
    req(input$emi_agg)
    
    # return the selected commune if one else return agg list
    if (input$emi_agg == "com") {
      validate(
        need(!is.null(city()$com_code), message = err_msg$city_aggr)
      )
      dat <- filter(emi_data_com, geo == city()$com_code)
    } else {
      dat <- switch(input$emi_agg, 
        "fr"  = emi_data_agg$fr,
        "reg" = emi_data_agg$reg,
        "dep" = emi_data_agg$dep
      )
    }
    dat
  })
  
  # episodes ----------------------------------------------------------------
  episode_filter <- reactive({
    req(input$epi_dept)
    
    # return episode if all else return selected dept
    if (input$epi_dept == "Tous") { return(episode) } 
    
    filter(episode, dept == input$epi_dept)
  }) 
  
  # find out which city ----
  city <- eventReactive(input$map_click, {
    
    # init the value ==> i.e the box is here 
    if (is.null(input$map_click)) {
      return(tibble(com_name = ""))
    }
    
    # define pt clicked & turn it as a geometry simple feature (pt) 
    pt_click <- c(input$map_click$lng, input$map_click$lat) 
    pt_click_sf <- st_sfc(st_point(pt_click), crs = 4326) #3857)  
    
    # compute dist between commune db & clicked pt ==> return closest commune
    pt_dist  <- st_distance(commune_sf, pt_click_sf)
    pt_close <- commune_name[which.min(pt_dist),]

    # test if city is in Fr or not
    over_fr <- st_intersects(pt_click_sf, st_as_sf(shp_frm), sparse = FALSE)
    
    # if not in fr
    if (!as.vector(over_fr)) {
      tibble(com_name = "hors de France")
    } else {
      pt_close
    }
  }, ignoreNULL = FALSE)

  # top row indicator -------------------------------------------------------
  # date & simulation informations
  output$dateBox <- renderInfoBox({
    # label 
    lab <- list(
      analyse   = "Carte des concentrations optimisées (simulations et observations)", 
      prevision = "Carte des concentrations prédites (simulations et statistiques)"
    )
    # box define
    infoBox(
      title    = tags$p(input$date, style = "font-size: 125%;"), 
      subtitle = ifelse(pol_data()$lead == -1, lab$analyse, lab$prevision),
      icon     = icon("calendar-check"),
      color    = "blue"
    )
  })

  # which city are you monitoring
  output$cityBox <- renderInfoBox({
    infoBox(
      title    = tags$p(city()$com_name, style = "font-size: 125%;"), 
      subtitle = "Pour choisir une ville, cliquez sur la carte",
      icon     = icon("home"),
      color    = "aqua"
    )
  })
  
  # concentration at the selected city (add annual value could be cool)
  output$concBox <- renderInfoBox({
    req(city(), input$map_click) 

    # use the real clicked place (i.e not the closest city)
    pt_click <- matrix(c(input$map_click$lng, input$map_click$lat), ncol = 2) 
    pt_conc  <- raster::extract(pol_data()$r, pt_click, method = "bilinear")
    
    # can vary the icon if pol > threshold (such as oms)
    infoBox(
      value = tags$div(style = "font-size: 110%; font-style: classic;", 
        HTML(paste(round(pt_conc, digits = 0), "µg.m<sup>-3</sup> de", 
                   pol_param_lab[input$pol]))), 
      title =  tags$p(style = "font-size: 125%;", "Concentrations estimées"),
      icon  = icon("cloudsmith"),
      color = "blue"
    )
  })

  # leaflet map -------------------------------------------------------------
  # define reactive palette 
  colorpal <- reactive({
    req(input$pol, input$stat)
    brks <- prevair[[input$stat]][[input$pol]]
    colorBin(prevair$color, domain = range(), bins = brks, pretty = FALSE)
  })

  # base map (i.e "not reactive") 
  output$map <- renderLeaflet({
    lim <- bbox(pol_data()$r) # def lim for zoom
    leaflet(options = leafletOptions(zoomControl = FALSE)) %>% 
      addTiles() %>%
      # add & move where I want the zoom
      htmlwidgets::onRender(
      "function(el, x) {
        L.control.zoom({ position: 'bottomright' }).addTo(this)
      }") %>%  
      fitBounds(lim[1], lim[2], lim[3], lim[4]) %>% 
      addEasyButton(
        easyButton(
          id = "geoloc", icon  = "fa-crosshairs", title = "Locate Me",
          onClick = JS("function(btn, map){ 
                          map.locate({setView: true, enableHighAccuracy: true }); 
                          Shiny.onInputChange('geo_action', 'clicked');
                       }"),
          position = "bottomright"
        )
      ) %>% 
      addSearchOSM(options = searchFeaturesOptions(position = "topleft"))
  })
  
  # add another way with geoloca to select the city
  # observeEvent(input$geo_action, {
  #   str(input$geo_action)
  # })
  
  # observe input & update map
  observe({
    # assign the pal
    pal <- colorpal()
    
    # unique lonlat id for station 
    selcol <- c("samplingpoint_x", "samplingpoint_y", "station_localid", 
                "network_name", "station_altitude", "station_name")
    sta_df <- pol_data_ts() %>% 
      dplyr::select(!!selcol) %>% 
      distinct() 

    # create the label (for the hover)
    hover <- sprintf(
      "<strong>%s</strong><br/>%s",
      sta_df$station_name, sta_df$network_name
    ) %>% lapply(htmltools::HTML)

    # add the map
    leafletProxy("map", data = pol_data()$r) %>%
      clearImages() %>%
      clearControls() %>% 
      # add raster concentrations # !!! reprojected to match leaflet crs 
      addRasterImage(pol_data()$r, colors = pal, opacity = .8,
                     attribution = "Air pollution data © PrevAir") %>% 
      addLegend("bottomleft", pal = pal, values = prevair[[input$stat]][[input$pol]], 
                group = "legend", title = HTML("<p>µg.m<sup>-3</sup></p>"), opacity = 1) %>%
      # add country contour
      addPolygons(data = shp_frm, stroke = TRUE, color = "black", 
                  fillColor = "transparent", weight = 1) %>% 
      # add stations just to be clicked (can't compared daily, hourly val) 
      addCircles(data = sta_df, layerId = ~station_localid, lng = ~samplingpoint_x,
        lat = ~samplingpoint_y, color = "black", fillColor = "transparent",
        stroke = TRUE, weight = 3, radius = 300, label = hover, 
        highlightOptions = highlightOptions(color = "red", weight = 3, bringToFront = TRUE),
        labelOptions = labelOptions(style = list("font-weight" = "normal", padding = "3px 8px"),
          textsize = "12px", direction = "auto")
      )
  })
  
  # observe city click & add marker
  observeEvent(input$map_click, {
    # add pt clicked & name of the city if selected
    leafletProxy("map") %>%
    addMarkers("map", lng = input$map_click$lng, lat = input$map_click$lat, 
               label = city()$com_name)
  })
     
  # timeseries  -------------------------------------------------------------
  output$tseries <- renderPlot({
    validate(
      need(input$map_shape_click$id, message = err_msg$timeserie)
    )
    
    # safer than using lng, lat (that could be a little bit different and thus 
    # filter would return an empty df)
    dat <- pol_data_ts() %>% 
      filter(station_localid == input$map_shape_click$id) 
    
    # prevent when changing pol that station no longer exists 
    validate(
      need(nrow(dat) != 0, message = err_msg$timeserie)
    )
    
    ggplot(data = dat, aes(x = value_datetime_begin, y = value_numeric)) +
        geom_line(color = "grey", lwd = .3) +
        geom_point(aes(fill = cut(value_numeric, 
                                  breaks = prevair[[input$stat]][[input$pol]], 
                                  right = TRUE, include.lowest = TRUE)),
          cex = 3, color = "black", stroke = .3, pch = 21) +
        scale_fill_manual(values = prevair$color, drop = FALSE) + 
        labs(
          title    = paste0("Concentrations de ", pol_param_lab[input$pol]),
          subtitle = paste0("Station: ", dat$station_name), 
          x        = "date/hours",
          y        = "concentrations",
          fill     = expression(µ*g/m^{3}),
          caption  = paste0("Données © ", dat$network_name) 
        ) +
        scale_x_datetime(date_labels = "%d/%m\n%H:%M") +
        guides(fill = guide_legend(reverse = TRUE)) # reverse ldg order
  })
  
  # emissions histo --------------------------------------------------------
  output$histo <- renderPlot({
    # map click check
    validate(
      need(input$map_click, message = err_msg$map_click)
    )
    
    # city check (if map click make sure that it'is in France)
    if (!is.null(input$map_click)) {
      validate(
        need(!is.null(city()$com_code), message = err_msg$city_check)
      )
    }
    
    # filter (current quarter & city selected) & plot
    # if using sqrt trans ==> should perform the sum over pol before 
    # otherwise ggplot apply tr to each pol and stack them 
    # other alternative no coord flip and use coord_trans(sqrt) but ugly
    dat <- filter(emi_data_com, geo == city()$com_code & 
                    time == lubridate::quarter(input$date)) %>% 
                    group_by(snap) %>% 
                    summarise(emis = sum(emis)) 
    # plot
    ggplot(data = dat, aes(x = factor(snap), y = emis)) + 
      geom_col() +
      scale_y_continuous(trans = "sqrt", labels = scientific, 
                         expand = expand_scale(mult = c(0, .1))) +
      scale_x_discrete(
        breaks = emi_data_lab$ACT$identifiant,
        labels = str_wrap(emi_data_lab$ACT$nom_secteur, width = 24)) +
      coord_flip() +
      labs(
        title    = paste0("Quelles émissions dominent à ", str_to_title(city()$com_name), "?"),
        subtitle = paste0("Emissions du trimestre n°", lubridate::quarter(input$date)),
        x = NULL, y = "emission values (kg)", caption = "Données de l'INS"
      ) +
      theme(legend.position = "bottom", legend.justification = "center")
  })
  
  # go to emissions tab 
  observeEvent(input$to_emis, {
    updateTabItems(session, "tabs", "m_emis")
  })
  

  # summary -----------------------------------------------------------------
  output$summary <- renderUI({
    # map click check
    validate(
      need(input$map_click, message = err_msg$map_click)
    )
    
    # city check (no message)
    if (!is.null(input$map_click)) {
      validate(need(!is.null(city()$com_code), message = ""))
    }
    
    # use the real clicked place (i.e not the closest city)
    pt_click <- matrix(c(input$map_click$lng, input$map_click$lat), ncol = 2) 
    pt_conc  <- raster::extract(pol_data()$r, pt_click, method = "bilinear")
      
    # filter (current quarter & city selected) & plot
    snap_ind <- filter(emi_data_com, geo == city()$com_code & 
                    time == lubridate::quarter(input$date)) %>%  
                    group_by(snap) %>% # regroup by snap
                    summarise(emis = sum(emis)) %>% # agg all pol
                    top_n(n = 1) # extract max
    
    snap_name <- filter(emi_data_lab$ACT, identifiant == snap_ind[["snap"]])

    # text
    tags$div(
      # conc
      tags$h4("Concentrations estimées"),
      tags$p(
        HTML(
          paste0(
            "Les concentrations estimées de ", str_replace(input$pol, "PM25", "PM2.5"),
            " valent ", round(pt_conc, digits = 0), " µg.m<sup>-3</sup> le ", input$date,"."
          )
        )
      ),
    tags$br(),
    # emis
    tags$h4("Sources locales d'émissions"),
      tags$p(
        paste0(
          "La principale source de pollution de ", 
          str_to_title(city()$com_name), " provient du secteur : ", 
          str_to_lower(snap_name[["nom_secteur"]]), " (", 
          format(x = round(snap_ind[["emis"]], digits = 0), big.mark = " "),
          " kg d'émissions de polluants atmosphériques pour le trimestre n°", 
          lubridate::quarter(input$date), ")."
        )
      )
    )
  })
  
  # explore emission --------------------------------------------------------
  # sel options: start with the commune selected
  output$emi_opt <- renderUI({
    # add commune to the choice as the name of the selected commune
    agg_type_loc <- c(agg_type, "com")
    names(agg_type_loc) <- c(names(agg_type), "Commune") # dflt
    # if city selected update with the name of the city instead of commune
    if (isTruthy(city()[["com_name"]])) {
      names(agg_type_loc)[4] <- str_to_title(city()[["com_name"]]) #update 
    }
    # spatial choice
    selectInput("emi_agg", "Sélectionnez la zone des émissions", 
                choices = agg_type_loc, selected = agg_type_loc[4])
  })
  
  # if other than france or commune please select more precisely
  output$emi_opt_fin <- renderUI({
    conditionalPanel(
      condition = "input.emi_agg == 'dep' | input.emi_agg == 'reg'",
      # dflt selection = dep or region where the user is
      selectInput("emi_agg_fin", "Affinez la sélection", 
        choices = sort(unique(emi_data()[[paste0(input$emi_agg,"_name")]])), 
        multiple = FALSE, selected = city()[[paste0(input$emi_agg,"_name")]])
    )
  })
  
  # pollutant explanations
  output$emi_expl <- renderTable({
    tibble("Acronyme" = names(emi_param$pol), "Nom" = emi_param$pol_name)
  }, striped = TRUE, spacing = 'xs') 
  
  # options for emis per quarter figures
  output$emi_time_opt <- renderUI({
    tagList(
      pickerInput("emi_spe", label = "Sélectionnez au moins un polluant", 
                  choices = emi_param$pol, selected = emi_param$pol, 
                  options = list(`actions-box` = TRUE), multiple = TRUE,
                  choicesOpt = list(subtext = paste0("(", emi_param$pol_name, ")"))),
      actionButton("emi_spe_go", "Actualisez moi", 
                   icon = icon("bar-chart"))
    )
  })
  
  # new filtering (if reg, rep... return the selected one)
  emi_data_fin <- reactive({
    req(input$emi_agg)
    # all quarter & all pol considered 
    # only need to filter if region or dept
    if (input$emi_agg %in% c("com", "fr")) {
      emi_data()
    } else if (input$emi_agg == "dep") {
      req(input$emi_agg_fin)
      filter(emi_data(), dep_name == input$emi_agg_fin)
    } else if (input$emi_agg == "reg") {
      req(input$emi_agg_fin)
      filter(emi_data(), reg_name == input$emi_agg_fin)
    }
  })
  
  # abs importance of each pol emis per snap (pol focus)
  output$emi_pol_abs <- renderPlot({
    req(input$emi_agg)

    ggplot(data = emi_data_fin(), aes(x = factor(snap), y = emis, 
                                      fill = factor(pol, levels = emi_param$pol[neworder]))) +
      geom_col() +
      scale_y_continuous(
        trans = "identity", expand = expand_scale(mult = c(0, .05)),
        labels = scientific) +
      scale_x_discrete(
        breaks = emi_data_lab$ACT$identifiant,
        labels = str_wrap(emi_data_lab$ACT$nom_secteur, width = 24)) +
      coord_flip() +
      scale_fill_manual(values = specie_color, name = "espèces", 
                        labels = names(emi_param$pol[neworder])) +
      theme(plot.margin = margin(5, 5, 15, 5, "pt")) +
      labs(x = NULL, y = "emissions (kg)")
  })
  # rel importance of each pol emis per snap (pol focus)
  output$emi_pol_rel <- renderPlot({
    req(input$emi_agg)

    ggplot(data = emi_data_fin(), aes(x = factor(snap), y = emis,
                                      fill = factor(pol, levels = emi_param$pol[neworder]))) +
      geom_col(position = "fill") +
      scale_y_continuous(expand = expand_scale(mult = c(0, 0)), labels = percent) +
      coord_flip() +
      scale_fill_manual(values = specie_color, name = "espèces", 
                        labels = names(emi_param$pol[neworder])) +
      theme(axis.title.y = element_blank(), axis.text.y = element_blank(),
        legend.position = "none", plot.margin = margin(5, 25, 15, 10, "pt")) +
      labs(x = NULL, y = "importance relative (%)", fill = "espèces")
  })
   
  # abs importance of each pol emis per quarter (time focus)
  output$emi_time_abs <- renderPlot({
    input$emi_spe_go # wait action otherwise exe at each change...boring
    
    # filter on species (isolate to wait the action button)
    dat <- emi_data_fin() # dep on input_agg (thus no isolate)
    dat <- isolate(filter(dat, pol %in% input$emi_spe)) 
    
    # security (if no species selected) 
    if (nrow(dat) == 0) { return() }
    
    # plot
    ggplot(data = dat, aes(x = factor(snap), y = emis, fill = factor(time))) +
      geom_col() +
      labs(title = NULL, x = NULL, y = "émissions (kg)", fill = "trimestre") +
      scale_x_discrete(
        breaks = emi_data_lab$ACT$identifiant,
        labels = str_wrap(emi_data_lab$ACT$nom_secteur, width = 24)) +
      scale_y_continuous(
        expand = expand_scale(mult = c(0, .05)),
        trans = "identity", labels = scientific) + 
      theme(plot.margin = margin(5, 5, 15, 5, "pt")) +
      coord_flip()
  })
  
  # rel importance of each pol emis per quarter (time focus)
  output$emi_time_rel <- renderPlot({
    input$emi_spe_go

    # filter on species (isolate to wait the action button)
    dat <- emi_data_fin() # dep on input_agg (thus no isolate)
    dat <- isolate(filter(dat, pol %in% input$emi_spe)) 
    
    # security (if no species selected)
    if (nrow(dat) == 0) { return() }
    
    # percent (title = "relative importance of emission per semester")
    ggplot(data = dat, aes(x = factor(snap), y = emis, fill = factor(time))) +
      geom_col(position = "fill") +
      labs(title = NULL, x = NULL, 
           y = "part des émission selon le trimestre", fill  = "trimestre") +
      scale_y_continuous(labels = scales::percent_format(),
                         breaks = 1:4,
                         expand = expand_scale(mult = c(0, 0))) +
      coord_flip() +
      theme(axis.title.y = element_blank(), axis.text.y = element_blank(),
        legend.position = "none", plot.margin = margin(5, 25, 15, 10, "pt"))
  })  
  
  # explore episode ---------------------------------------------------------
  # infobox 
    output$epi_info <- renderInfoBox({
    # part of the title
    dept <- ifelse(input$epi_dept == "Tous", "France", str_to_title(input$epi_dept))
    # nb of episodes
    nb_epi <- episode_filter() %>% distinct(date, dept, seuil, pol) %>% count() 
    # define the box
    infoBox(
      value    = tags$p(nb_epi, style = "font-size: 150%;"),
      title    = tags$p(paste0("dépassements de seuil depuis avril 2015 en ", 
                               dept), style = "font-size: 120%;"),
      icon     = icon("map-pin"),
      color    = "light-blue",
      width    = NULL,
      fill     = TRUE
    )
  })
  
  # episode timeline (count one episode per dept)
  output$epi_time <- renderPlot({
    distinct(episode_filter(), date, dept, seuil, pol) %>% 
    ggplot(aes(x = date, y = factor(pol, levels = rev(c("PM10", "O3", "SO2", "NO2"))))) +
      geom_point() + #aes(text = seuil)
      scale_x_date(date_breaks = "4 month", labels = scales::date_format("%m-%Y")) + 
      labs(title = NULL, x = NULL, y = NULL) +
      theme(plot.margin = margin(5, 25, 15, 25, "pt"))
  })
  
  # occurence episodes par seuil & polluant
  output$epi_count <- renderPlot({
    # count
    episode_filter() %>% 
      distinct(date, dept, seuil, pol) %>% 
      count(seuil, pol) %>% 
      # plot
      ggplot(aes(x = fct_reorder(pol, n), y = n, fill = seuil)) +  
        geom_col() + 
        scale_y_sqrt(expand = expand_scale(mult = c(0, .1))) + 
        coord_flip() +
        scale_fill_brewer(type = "qual", palette = "Set2") + 
        labs(title = NULL, y = "nombre d'episodes", x = "polluant",
          caption = NULL, fill = "seuil réglementaire dépassé") +
        theme(legend.position = "bottom") +
        guides(fill = guide_legend(title.position = "bottom", title.hjust = 0.5))
  })
  
  # nb episodes par poll fct des annees
  output$epi_year <- renderPlot({
    # count
    episode_filter() %>%
      distinct(date, year, dept, seuil, pol) %>% 
      count(year, pol) %>% 
      # plot
      ggplot(aes(x = year, y = n , fill = fct_reorder(pol, n, .desc = T))) + 
      geom_col(position = "dodge") + 
      scale_y_sqrt(expand = expand_scale(mult = c(0, .1))) + 
      labs(title = NULL, x = "année", y = "nombre d'épisodes", fill = "polluant") + 
      scale_fill_brewer(type = "qual", palette = "Set2")
  })
  
  # map du nb episodes de pm10 du dept / nb total 
  output$epi_map <- renderPlot({
    req(input$epi_year, input$epi_pol)
    
    # filter episode according to user input & add to the map
    dat <- episode %>%
      distinct(date, dep_code, pol, year) 

    # if all year no filter on year
    if (input$epi_year == "Toutes") {
      dat <- filter(dat, pol == input$epi_pol) %>% 
        count(dep_code, pol)
    } else {
      dat <- filter(dat, year == input$epi_year & pol == input$epi_pol) %>% 
        count(dep_code, pol, year)
    }

    # should evolve at least the upper part
    brks <- c(0, 3, 5, 8, 12, 17, 25, 30, 40, +Inf)
    dat  <- mutate(dat, n_cut = cut(n, breaks = brks, right = TRUE)) %>% 
      left_join(x = shp_dep, by = c("code_insee" = "dep_code")) # add to the map

    # warning!!! breaks hardcoded (not satisfy with the cut_interval and so on)
    pal <- RColorBrewer::brewer.pal(name = "Blues", n = length(brks) - 1) 
    plot(dat["n_cut"], main = NULL, key.pos = 4, axes = F, key.width = lcm(3.2), 
         key.length = .5, lwd = .5, pal = pal)
  })
}


