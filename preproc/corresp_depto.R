# script to build table of name correspondance 
# try to ensure common format between all my data
# dept level
library(tidyverse)
library(readxl)

# ==> do fr metropolitan or all? now I have episode for all but emis for frm
# if time later ==> I should dl emissions for domtom 

# read shape & process ----------------------------------------------------
fr_df <- sf::st_read("data/map/", layer = "communes") %>% 
  as_tibble() %>% # as df
  select(c(dep_code = "INSEE_Dépa", dep_name = "Nom_Départ")) %>% # sel dept
  mutate_if(.predicate = is.factor, .funs = as.character) %>% 
  distinct %>%  drop_na

# read depto & tidy it  ---------------------------------------------------
indir <- "data/lcsqa/"
# histo 
ifile <- paste0(indir, "episodes_15042015_14092017.xlsx")
histo <- read_xlsx(ifile, col_names = TRUE, skip = 2) %>% 
  select(dept = "Département") 
# actu
ifile <- paste0(indir, "episodes_23092017_07122018.csv")
actu  <- read_delim(ifile, col_names = TRUE, delim = ";", 
                    locale = locale("fr", encoding = "Latin3")) %>% 
  select(dept = "Département")

# bind, distinct & then split
both  <- bind_rows(histo, actu) %>% 
  distinct %>% drop_na %>% 
  # remove white space between same name cause split with space next
  mutate(
    dept = str_replace(dept, "TERRITOIRE DE BELFORT", "TERRITOIRE-DE-BELFORT"),
    dept = str_replace(dept, "LA REUNION", "LA-REUNION")
  ) %>% 
  # split dept (one by line) & unique
  separate_rows(dept, sep = "[:blank:]{1,20}", convert = TRUE) %>% distinct

# merge depto episode & emissions -----------------------------------------
dep_merge <- full_join(fr_df, both, by = c("dep_name" = "dept"))
# ! NA for domtom cause not in the shapefile: later include domtom

write_rds(dep_merge, path = "data/processed/corresp_depto.Rds")
