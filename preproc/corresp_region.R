# script to build table of name correspondance
# try to ensure common format between all my data
# region level
library(tidyverse)
library(readxl)

# adapted from: https://data.hypotheses.org/564
norm_name <- function(text) {
  text <- gsub("['`^~\"]", " ", text)
  text <- iconv(text, to = "ASCII//TRANSLIT//IGNORE")
  text <- gsub("['`^~\"]", " ", text)
  text <- gsub("-", " ", text) # replace "-" by " "
  text <- tolower(text)
  return(text)
}

# ref table corresp -------------------------------------------------------
# clean name & remove "'" from names norm
newcol <- c("code_new", "name_new", "code_old", "name_old", 
            "name_new_norm", "name_old_norm")
reg_old_new  <- read_csv2("data/map/anciennes-nouvelles-regions.csv") %>%
  set_names(newcol) %>% 
  mutate(
    name_old_norm = norm_name(name_old_norm),
    name_new_norm = norm_name(name_new_norm)
  ) 

# episodes ----------------------------------------------------------------
indir <- "data/lcsqa/"

# histo ==> old region
ifile <- paste0(indir, "episodes_15042015_14092017.xlsx")
histo <- read_xlsx(ifile, col_names = TRUE, skip = 2) %>% 
  select(emi_histo_name = "Région") %>%  
  distinct %>% drop_na %>% 
  mutate(name_old_norm = norm_name(emi_histo_name))

# actu  ==> new region
ifile <- paste0(indir, "episodes_23092017_07122018.csv")
actu  <- read_delim(ifile, col_names = TRUE, delim = ";", 
                    locale = locale("fr", encoding = "Latin3")) %>% 
  select(emi_actu_name = "Région") %>% 
  distinct %>% drop_na %>% 
  mutate(name_new_norm = norm_name(emi_actu_name)) 

# merge -------------------------------------------------------------------
reg_all_name <- reg_old_new %>% 
  left_join(histo, by = "name_old_norm") %>% 
  left_join(actu, by = "name_new_norm")  

write_rds(reg_all_name, path = "data/processed/corresp_region.Rds")

