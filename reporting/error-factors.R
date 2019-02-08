# lib load
library(tidyverse)

# influence of misspellings & quote for exact combination of words

# read data ---------------------------------------------------------------
# request gtrends misspellings
trend_missp  <- read_csv("data/gtrends/trends-2004-aq_misspelling-influence_atm.csv", 
                  skip = 2, col_names = T)  %>% 
  rename_at(vars(contains("France")), 
            funs(str_remove(., pattern = ":.*$"))) %>% #rm all after ":"
  rename(month = Mois) %>% 
  mutate(month = lubridate::ymd(month, truncated = 1)) %>% 
  gather(type, hits, -month)
            

# request gtrends aq cc
trend_quote <- read_csv("data/gtrends/trends-2004-aq_quote-influence.csv", 
                        skip = 2, col_names = T) %>%  
  rename_at(vars(contains("France")), 
            funs(str_remove(., pattern = ":.*$"))) %>% #rm all after ":"
  rename(month = Mois) %>% 
  mutate(month = lubridate::ymd(month, truncated = 1)) %>% 
  gather(type, hits, -month)

# figures -----------------------------------------------------------------
theme_set(theme_classic())

# misspelling influence
p <- ggplot(data = trend_missp, aes(x = month, y = as.numeric(hits), 
                                    color = type, group = type)) +
     geom_line() + 
     scale_x_date(date_labels = "%Y-%m") +
     theme(
       legend.title = element_blank(),
       legend.direction = "vertical",
       legend.position = c(.8,.7),
       legend.text = element_text(size = 12)
     ) + 
     labs(
       title = 'Comparaison de l\'influence du "misspelling"',
       x = NULL,
       y  = "Intérêt relatif",
       color = NULL
     ) 

ggsave(plot = p, filename = "figs/misspelling.png", scale = 1, width = 8, height = 5)
# x11() ; p 

# quote influence
p <- ggplot(data = trend_quote, 
            aes(x = month, y = as.numeric(hits), 
                color = type, group = type)) +
     geom_line() + 
     scale_x_date(date_labels = "%Y-%m") +
     theme(
       legend.position = "bottom",
       legend.direction = "vertical",
       legend.text = element_text(size = 12)
     ) + 
     labs(
       title = 'Comparaison de l\'influence des " " sur la recherche',
       x  = NULL,
       y  = "Intérêt relatif",
       color = NULL
     ) 

ggsave(plot = p, filename = "figs/quote.png", scale = 1, width = 8, height = 5)
# x11() ; p
