# lib load
library(tidyverse)

# 2. See trends (2004 ==> nowadays) 
# 4. comparing aq and cc (such as twitter) 

# use + to add multiple terms (not supported in R package, working on it!)
# see gtrends docs: https://support.google.com/trends/answer/4359582?hl=fr
# misspelling... !!! https://support.google.com/trends/answer/4359550?hl=fr&ref_topic=4365530
 
# read data ---------------------------------------------------------------
# request gtrends aq only
time_trends  <- read_csv("data/gtrends/trends-2004-aq.csv", 
                        skip = 2, col_names = T) 
info_request <- names(time_trends)[2] # extract request
time_trends  <- set_names(time_trends, c("month", "hits")) %>% 
  mutate(month_new = lubridate::ymd(month, truncated = 1))

# request gtrends aq cc
time_trends_aqcc  <- read_csv("data/gtrends/trends-2004-ccaq.csv", 
                        skip = 2, col_names = T) 
info_request_aqcc <- names(time_trends_aqcc)[2:3] # extract request
time_trends_aqcc  <- set_names(time_trends_aqcc, c("month", "aq", "cc")) %>% 
  mutate(month = lubridate::ymd(month, truncated = 1)) %>% 
  gather(key = type, value = hits, aq:cc) # == gather(type, hits, -month)

# figures -----------------------------------------------------------------
theme_set(theme_classic())

p <- ggplot(data = time_trends, aes(x = month_new, y = as.numeric(hits), group = 1)) +
     geom_line() + 
     scale_x_date(date_labels = "%Y-%m") +
     theme(
       legend.title = element_blank(),
       legend.position = "bottom",
       legend.text = element_text(size = 12)
     ) + 
     labs(
       title = "Tendance de recherche Google sur la qualité de l'air",
       x  = NULL,
       y  = "Intérêt relatif"
     ) + geom_smooth()
ggsave(p, filename = "figs/trends.png", width = 8, height = 5)


p <- time_trends_aqcc %>% 
  mutate(type = ifelse(type == "aq", "Qualité de l'air", "Changement climatique")) %>%     ggplot(aes(x = month, y = as.numeric(hits), color = type, group = type)) +
     geom_line() + 
     scale_x_date(date_labels = "%Y-%m") +
     theme(
       legend.title = element_blank(),
       legend.position = "bottom",
       legend.direction = "vertical",
       legend.text = element_text(size = 12)
     ) + 
     labs(
       title = "Comparaison des fréquences de recherche Google",
       subtitle = "Portant sur la qualité de l'air & le changement climatique",
       x  = NULL,
       y  = "Intérêt relatif"
     ) 

ggsave(p, filename = "figs/trends_aqcc.png", width = 8, height = 5)
x11() ; p
