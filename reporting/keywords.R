# lib load
library(gtrendsR)
library(tidyverse)

#1. which keywords to look for aq 
# info: https://support.google.com/trends/answer/4359582?hl=fr
# !!! "'" = " " in google ==> thus use space
# !!! use "..." to have only a group of word

# It is worth noting that over the past decade search engines have grown increasingly more sophisticated with spell correction of the organic search results, and in September of 2014 both Google and Bing broadened out their spelling handling on exact match to include variant matching of closely related terms. That sort of shift minimizes soem of the direct economic benefit people would get from cheaper cost per click ads in the past.

# make a request ----------------------------------------------------------
# options 
opt <- list(
  keyw = c("pollution atmosphérique", "pollution de l air", 
           "qualité de l air", "épisode de pollution"),
  geo  = "FR",  # focus on france
  time = "all", # from 2004 (following period ==> tstep of the answer change)
  chan = "web"  # could be youtube...
)
# request
ifile <- "data/gtrends/trends-2004-aq_term-influence.csv"
if (!file.exists(ifile)) {
  trends <- gtrends(opt$keyw, gprop = opt$chan, geo = opt$geo, time = opt$time)
  trends_time <- trends$interest_over_time
  write_csv(trends_time, path = ifile)
} else {
  trends_time <- read_csv(ifile)
}

# figures -----------------------------------------------------------------
theme_set(theme_classic())
p <- ggplot(data = trends_time, aes(x = date, y = as.numeric(hits), group = keyword, col = keyword)) +
     geom_line() + 
     scale_x_date(date_labels = "%Y-%m") +
     theme(
       legend.title = element_blank(),
       legend.direction = "vertical",
       legend.position = c(.8,.7),
       legend.text = element_text(size = 12)
     ) + 
     labs(
       title = 'Quels mots pour rechercher la qualité de l\'air?',
       x  = NULL,
       y  = "Intérêt relatif",
       color = NULL
     ) 

x11() ; p
ggsave(plot = p, filename = "figs/keywords.png", width = 8, height = 5)
