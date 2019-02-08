# lib load
library(gtrendsR)
library(tidyverse)

#3. See if linking search nb & pollution episode is possible!

# use + to add multiple terms (not supported in R package, working on it!)
# see gtrends docs: https://support.google.com/trends/answer/4359582?hl=fr
# misspelling... !!! https://support.google.com/trends/answer/4359550?hl=fr&ref_topic=4365530

# read data ---------------------------------------------------------------
# episodes
episode <- read_rds("data/processed/episode_list.Rds") %>% 
  dplyr::filter(!reg_code %in% c(paste0("0", 1:4), "06"))

period  <- range(episode$date)

# request of google trends
time_trends  <- read_csv("data/gtrends/trends-2015-2018-aq.csv", 
                        skip = 2, col_names = T) 

info_request <- names(time_trends)[2] # extract request
time_trends  <- set_names(time_trends, c("weeks", "hits")) # rename

# figures -----------------------------------------------------------------
theme_set(theme_classic())

p <- ggplot(data = time_trends, aes(x = weeks, y = as.numeric(hits))) +
     geom_vline(data = episode, aes(xintercept = date, color = pol), 
                alpha = .3, lwd = 1) +
    # scale_colour_manual(values = c("#AD7575", "#B875BA", "#7788A8", "#659993")) +
     geom_line() + 
     theme(
       legend.title = element_blank(),
       legend.position = "bottom",
       legend.text = element_text(size = 12)
     ) + 
     labs(
       title = "Recherche Google sur la pollution atmosphérique & pics de pollution",
       x  = NULL,
       y  = "Intérêt relatif"
     ) +
    guides(colour = guide_legend(override.aes = list(alpha = 1)))

ggsave(plot = p, filename = "figs/episodes.png", width = 8, height = 5)

