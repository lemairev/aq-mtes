# stream aq data from eea (gather all eu data)
# in france: aasqa ==> lcsqa ==> eea for the reporting

# load lib
library(tidyverse)

# path
oudir <- "data/eea/tseries/real-time/"

# build the request
country   <- "FR" 
pollutant <- c("NO2", "CO", "PM10", "PM2.5", "SO2", "O3")       
url_base  <- "http://discomap.eea.europa.eu/map/fme/latest/"   
url_toget <- paste0(url_base, country, "_", pollutant, ".csv")

# download data 
for (i in seq_along(url_toget)) {
  print(paste0("processing: ", pollutant[i]))
  
  # grab the data 
  url_content <- read_csv(url_toget[i])

  # extract min & max time for date_begin 
  time_range  <- url_content %>% 
    pull("value_datetime_begin") %>% 
    range() %>% 
    map_chr(format, "%Y-%m-%d-%Hh") %>% 
    str_c(collapse = "_")

  # write the result 
  bname <- basename(url_toget[i]) %>% 
    str_remove("\\.csv") %>%
    str_remove("\\.") # if PM2.5 to be consistent with other src of data
  
  write_csv(url_content, path = paste0(oudir, bname, "_", time_range, ".csv"))
}

# later use purrr or // version of that script for fast dl
# url_content <- map(url_toget, read_csv) 

