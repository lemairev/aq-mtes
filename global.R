# load lib
library(tidyverse)
library(raster)
library(ncdf4)
library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(leaflet)
library(leaflet.extras)
library(sf)
library(scales)

# ggplot theme
theme_set(theme_light(base_size = 12))

# prevair concentrations --------------------------------------------------
# find avail files & extract parameters (date, pol, lead)
newcol <- c("bname", "qual", "date", "stat_lead", "pol", "ename", "type")
file_tbl <- tibble(name = dir("data/prevair/", "PREVAIR", full.names = T)) %>%
  # split name into multiple columns to filter
  tidyr::separate(name, sep = "\\.", into = newcol, remove = FALSE) %>%
  # split stat_lead into two columns
  tidyr::extract(stat_lead, "([A-Z]*)([0-9]*)", into = c("stat", "lead"),
          convert = TRUE, remove = TRUE) %>%
  # combine lead time and qual (i.e analyse = J-1, previ = J+0...3)
  mutate( 
    date = as.Date(date, "%Y%m%d"), # conv1ert from char to date
    lead = ifelse(is.na(lead), yes = -1, no = lead), # 2 col in 1
    date_real = case_when(
      lead %in% c(-1, 0) ~ date,
      lead %in% 1:3      ~ date + lead  
    )
  ) %>%
  dplyr::select(-c("bname", "ename", "qual", "type")) # drop useless var
# list of the parameters
pol_param <- map(file_tbl, .f = unique)
names(pol_param$pol) <- c("NO2", "O3", "PM10", "PM2.5") # fix PM25 PM2.5 for select
pol_param_lab <- c("NO2" = "NO2","O3" = "O3", "PM10" = "PM10", "PM25" = "PM2.5")

# monitoring stations concentrations --------------------------------------
# read data
indir <- "data/real-time/"
file_tbl_ts <- tibble(name = dir(indir, "FR", full.names = T)) %>% 
  separate(name, c("co", "pol", "dt_begin", "dt_end", "ext"), 
           sep = "_|\\.", remove = F) %>% 
  mutate(date = as.Date(dt_begin, format = "%Y-%m-%d-%Hh")) %>% 
  dplyr::select(-ext)

# emissions ---------------------------------------------------------------
indir <- "data/processed/"
# commune levels
commune_name <- read_rds(paste0(indir, "commune.Rds")) 
emi_data_com <- data.table::fread(paste0(indir, "emission_all_addfrac.csv")) 
emi_data_lab <- read_rds(paste0(indir, "emission_code_addfrac.Rds")) 
# aggregated levels: fr, dept, region
agg_type <- c('France' = "fr", 'Région' = "reg", 'Département' = "dep")
emi_data_agg <- map(agg_type, ~ read_rds(paste0(indir, "emission_all_agg_", .x, "_addfrac.Rds"))) %>% 
  set_names(agg_type)
# extract param (species avail...)
emi_param <- sapply(emi_data_agg[["fr"]][,1:3], unique)
names(emi_param$pol) <- emi_param$pol 
names(emi_param$pol)[c(7, 8, 10)] <- c("PM10-2.5", "PM2.5", "PMcoarse") # fix lab
emi_param$pol_name <- c("Méthane", "Monoxyde de Carbone", 
                        "Composé Organique Volatil Non Méthanique", 
                        "Protoxydes d'azote", "Ammoniac", "Oxydes d'azote", 
                        "Particules (diamètre 2.5 µm < d < 10 µm)", 
                        "Particules fines (diamètre < 2.5 µm)", 
                        "Oxydes de soufre", 
                        "Poussières (diamètre > 10 µm")
# neworder so that pm are all one
neworder <- c(1:6, 9, 8, 7, 10)
specie_color <- c("#66C2A5", "#8DA0CB", "#E78AC3", "#A6D854", "#FFD92F", 
                  "#E5C494", "#B3B3B3", "#C77575", "#B86161", "#9C3E3E")


# quick tr to sf object for finding the closest commune 
commune_sf <- dplyr::select(commune_name, com_x_lonlat, com_y_lonlat) %>% 
  st_as_sf(coords = c("com_x_lonlat","com_y_lonlat"), crs = 4326) 

# episodes ----------------------------------------------------------------
episode <- read_rds("data/processed/episode_list.Rds") %>% 
    mutate(year = lubridate::year(date)) %>%  # add year col
    dplyr::filter(!reg_code %in% c(paste0("0", 1:4), "06")) %>% #rm domtom
    left_join(distinct(commune_name[,c("dep_code","dep_name")]), 
              by = c(dept = "dep_name")) %>% 
    distinct # add code dept 

# french maps --------------------------------------------------------------
# simplified geometry, no domtom (better speed)
shp_frm <- rgdal::readOGR("data/fr_simple/", "fr_contour_lonlat_simplified") 
shp_dep <- st_read("data/dep_simple/", "departements-20180101_simplified") %>% 
  mutate_if(.predicate = base::is.factor, .funs = as.character) # factor to char

# utilities ---------------------------------------------------------------
# define error message
err_msg <- list(
  timeserie  = "Sélectionnez une station de mesure de la qualité de l'air en cliquant sur les cercles sur la ca)rte pour découvrir l'évolution des concentrations mesurées!",
  map_click  = "Sélectionnez une ville en France en cliquant sur la carte pour découvrir la concentration estimée ainsi que les sources de pollution les plus importantes!",
  city_check = "Sélectionnez une ville en France en cliquant sur la carte pour découvrir les sources de pollution majoritaires!",
  city_aggr  = "Sélectionnez une ville en France pour découvrir les sources de pollution ou sélectionnez une autre zone (France, région ou département)!"
)
# define prevair colorscale & breaks for PrevAir
# i.e removed the repetition of 1st color & thus the intermediate breaks
prevair <- list(
 MOYJ = list(
   NO2  = c(0, 28, 42, 56,  70, 100, +Inf),
   O3   = c(0, 48, 72, 96, 120, 150, +Inf),
   PM10 = c(0, 20, 30, 40,  50,  80, +Inf),
   PM25 = c(0, 14, 21, 28,  35,  60, +Inf)
 ),
 MAXJ = list(
   NO2  = c(0, 80, 120, 160, 200, 400, +Inf),
   O3   = c(0, 72, 108, 144, 180, 240, +Inf),
   PM10 = c(0, 32,  48,  64,  80, 125, +Inf),
   PM25 = c(0, 24,  36,  48,  60,  90, +Inf)
 ),
 color = c("#00d6a8", "#67fe00", "#f6ff00", "#ffb200", "#ff0000", "#8d0000")
)


