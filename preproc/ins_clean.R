# load lib
library(tidyverse)

# path 
indir <- "data/ins/"
oudir <- "data/processed/"

# read & clean
emi_read <- function(ifile_list) {
  ifile_list %>% 
    map_dfr(read_csv2, skip = 20, guess_max = 2e6) %>% 
    select(-identifiant) %>%  # drop id == nb of line
    filter(code_secteur != "Toutes activités") %>% # rm sum of all snap
    set_names(c("pol", "geo", "time", "snap", "emis")) %>% # rename
    mutate_at(c("snap", "emis"), as.numeric) # from chr to num
} 

# read & process
files_pm <- dir(indir, pattern = "PM|TSP", full.names = T)
files_other <- setdiff(dir(indir, "data", full.names = T), files_pm)

emis_df_other <- emi_read(files_other) # read other species
emis_df <- emi_read(files_pm) # read pm
emis_df_nox <- filter(emis_df, pol == "NOX") # nox out (faster processing for pm)

# create additionnal fraction (verified by sum ==> ok)
# warnings ==> could have some error of 10e-11 when reconstructing 
# PM10 or TSP from diff value ==> filter if < 0 ==> 0 
emis_df  <- filter(emis_df, pol != "NOX") %>% # only pm
  mutate(pol = recode(pol, "PM2,5" = "PM25")) %>% # rename 
  spread(pol, value = emis, fill = 0) %>% # spread 0 instead of NA
  mutate( 
    TSP_only  = TSP - PM10, # create only frac pm > pm10
    PM10_only = PM10 - PM25 # create frac pm10 > me > pm2.5
  ) %>% 
  select(-c("TSP", "PM10")) %>% 
  gather(pol, emis, 4:6) %>% # tidy
  mutate(emis = ifelse(emis < 0, 0, emis))# rounding error ==> set 0

# all species together (other nox & pm reworked)
emis_df <- bind_rows(emis_df, emis_df_nox, emis_df_other) 

# save as csv (read faster than rds)
write_csv(emis_df, path = paste0(oudir, "emission_all_addfrac.csv")) # save

# emiss by species
emis_df <- split(emis_df, f = as_factor(emis_df$pol)) # split by pol
walk2(
  .x = emis_df, 
  .y = names(emis_df), 
  .f = ~ write_rds(.x, paste0(oudir, "emission_", .y, "_addfrac.Rds"))
) # save each species 

# read info and store them 
ifile   <- dir(indir, "info", full.names = T)
info_df <- ifile %>% 
  map(read_csv2, skip = 7, guess_max = 1e6) %>% # read all csv
  set_names(str_split(ifile, "_|\\.", simplify = T)[,3]) %>% # name the df
  write_rds(paste0(oudir, "emission_code_addfrac.Rds")) # save


# graph to ensure quality of emis (show small error when rounding) --------
# x11()
# mutate(emis_df, TSPP = PM10_only + TSP_only + PM25) %>% 
#   filter(TSP != TSPP) %>% 
#   mutate(diffTSP = TSP - TSPP) %>% 
#   ggplot(aes(x = diffTSP)) +
#   geom_histogram()
# 
# 
# x11()
# mutate(emis_df, PPM10 = PM10_only + PM25) %>% 
#   filter(PM10 != PPM10) %>% 
#   mutate(diff = PM10 - PPM10) %>% 
#   ggplot(aes(x = diff)) +
#   geom_histogram()
