ui <- dashboardPage(
  # header ------------------------------------------------------------------
  dashboardHeader(
    title      = "La qualité de l'air près de chez soi",
    titleWidth = 380
  ),
  
  # side bar ----------------------------------------------------------------
  dashboardSidebar(
    sidebarMenu(id = "tabs",
      menuItem("Ma qualité de l'air", tabName = "m_dash", icon = icon("eye")),
      menuItem("Explorez", icon = icon("search"), startExpanded = TRUE,
        menuSubItem("Emissions de polluants", tabName = "m_emis", icon = icon("angle-right")), 
        menuSubItem("Episodes de pollution", tabName = "m_epis", icon = icon("angle-right"))  
      ),
      menuItem("Préoccupation", tabName = "m_social", icon = icon("twitter")),
      menuItem("A propos", tabName = "m_about", icon = icon("question"))
    )
  ),

  # body --------------------------------------------------------------------
  dashboardBody(
    tabItems(
      # dashboard -----------------------------------------------------------
      tabItem(tabName = "m_dash",
        # some usefull information for the user 
        fluidRow(
          infoBoxOutput("dateBox"), # add the date & info forecast...
          infoBoxOutput("cityBox"), # add the name of the selected city
          infoBoxOutput("concBox")  # add concentrations value of the sel city
        ),
        # boxes need to be put in a row (or column)
        fluidRow(
          # custom css style, pal css fixer & change color of error message
          shiny::includeCSS('www/style.css'),
          tags$style(".bootstrap-select .dropdown-menu li a span.text {width: 100%;}"),
          tags$head(
            tags$style(HTML("
              .shiny-output-error-validation {
                color: grey;
                font-size: 125%;
              }
            "))
          ),
          # map concentrations & stations
          box(
            title = "Concentrations estimées (modélisation numérique Prev'Air)", 
            status = "primary", solidHeader = TRUE, collapsible = TRUE,
            # options
            column(width = 12,
              column(width = 4, 
                dateInput("date", "Date", value = "2018-12-23",#pol_param$date[1], 
                          language = "fr", weekstart = 1)),
              column(width = 4, 
                selectInput("pol", "Polluant", choices = pol_param$pol, 
                            selected = "PM25")),
              column(width = 4, 
                selectInput("stat", "Donnée", selected = "MOYJ",
                            choices  = c("Maximum" = "MAXJ", "Moyenne" = "MOYJ")))
            ),
            # map
            leafletOutput("map"),
            uiOutput("colors")
          ),
          
          # timeseries 
          box( 
            title = "Concentrations relevées aux stations (AASQA)", 
            status = "primary", solidHeader = TRUE, collapsible = TRUE,
            plotOutput("tseries")
          )
        ),
        fluidRow(
          column(width = 4, 
            # summary
            box(
              title  = "Résumé", status = NULL, solidHeader = F, 
              collapsible = T, width = NULL,
              htmlOutput("summary")
            )
          ),
          column(width = 8,
            # histo
            box(
              title  = "Sources locales d'émissions (Inventaire National)", 
              status = "warning", solidHeader = T, collapsible = T, width = NULL,
              # plot
              plotOutput("histo"),
              # go to emission explore 
              actionButton("to_emis", strong("Pour en découvrir plus, cliquez-moi!"), 
                icon = icon("search"), width = "100%", 
                style = "color: #fff; background-color: #ffa200; border-color: #fff")
            ) 
          )
        )
      ), 
      
      # explore emission ----------------------------------------------------
      tabItem(tabName = "m_emis",
        fluidRow(
          # plot box
          column(width = 9,
            box(
              title = "Les sources d'émissions de polluants par espèces",
              status = "primary", solidHeader = TRUE, collapsible = TRUE, width = NULL,
              column(width = 8, plotOutput(outputId = "emi_pol_abs")),
              column(width = 4, plotOutput(outputId = "emi_pol_rel"))
            )
          ),
           # options box
          column(width = 3,
            box(
              title = "Options globales, valables pour les deux figures", 
              status = "warning", solidHeader = F, collapsible = T, width = NULL,
              uiOutput("emi_opt"),
              uiOutput("emi_opt_fin")
            ),
            box(
              title = "Noms des polluants",
              status = "warning", solidHeader = F, collapsible = T, 
              width = NULL, collapsed = T,
              tableOutput("emi_expl")
            )
          )
        ),
        fluidRow(
          # options box
          column(width = 3,
            box(
              title = "Options: sources d'émissions",
              status = "primary", solidHeader = F, collapsible = T, width = NULL,
              uiOutput("emi_time_opt")
            )
          ),
          # plot box
          column(width = 9, 
            box(
              title = "Importance des sources d'émissions de polluants par trimetre",
              status = "primary", solidHeader = T, collapsible = T, width = NULL,
              column(width = 8, plotOutput(outputId = "emi_time_abs")),
              column(width = 4, plotOutput(outputId = "emi_time_rel"))
            )
          )
        )
      ),

      # explore episodes ----------------------------------------------------
      tabItem(tabName = "m_epis",
        # options & box info
        fluidRow(
          column(7,
            valueBoxOutput("epi_info", width = NULL)    
          ),
          column(5,
            box(
              title = "Sélectionnez un/tous les départements", status = "primary",
              solidHeader = F, collapsible = T, width = NULL,
              selectInput('epi_dept', NULL, multiple = FALSE,
                choices = c("Tous", sort(unique(episode$dept))), selected = "Tous")
            )
          )
        ),
        fluidRow(
          # episode timeline
          column(8, 
            box(
              title  = "Episodes de pollution fonction du temps (par département sélectionné)",
              status = "primary", solidHeader = TRUE, collapsible = TRUE, width = NULL,
              plotOutput("epi_time")
            )
          ),
          column(4, 
            box(
              title = "Nombre d'épisodes par polluant et département depuis avril 2015", 
              status = "primary", solidHeader = T, collapsible = T, width = NULL,
              plotOutput("epi_count")
            )
          ) # column 
        ), # fluidrow
        fluidRow(
          box(
            title = "Nombre d'épisodes par polluant, année et département depuis avril 2015", 
            status = "primary", solidHeader = T, collapsible = T, width = 6,
            plotOutput("epi_year")
          ),
          
          box(
            title = "Nombre d'épisodes par département", status = "warning",
            solidHeader = TRUE, collapsible = TRUE, width = 6,
            
            div(id = "mult_dbl", 
              selectInput("epi_pol", "Polluant", selected = "PM10",
                          choices = c("PM10", "NO2", "O3", "SO2"))
            ),
            div(id = "mult_dbl",
              selectInput('epi_year', "Année", selected = "Toutes",
                          choices = c("Toutes", sort(unique(lubridate::year(episode$date)))))
            ),
            plotOutput("epi_map")
          )
        )
      ),
      
      # social -------------------------------------------------------------
      tabItem(tabName = "m_social",
        includeHTML("social.html")
      ),
      
      # about --------------------------------------------------------------
      tabItem(tabName = "m_about",
         includeMarkdown("about.md")
      )
    ) # tabItems close
  )
)
