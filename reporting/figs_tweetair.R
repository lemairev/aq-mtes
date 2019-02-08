# load lib
library(rtweet)
library(tidyverse)
library(tidytext)
library(syuzhet) # get_sentiment(fr )


theme_set(theme_light())

# custom functions
clean_tweet <- function(text) {
  if (is.null(text)) stop("need text arg")
  if (!is.character(text)) stop("need chr")
  # need to keep the accent for the sentiment analysis
  # text <- iconv(text, to = "ASCII//TRANSLIT//IGNORE")   # rm accent
  # text <- gsub("['`^~\"]", " ", text)  # rm accent
  text <- gsub("\\n", " ", text)  # rm newline
  text <- gsub("(http?s[^ ]*)|(www\\.[^ ]*)", " ", text)  # rm http
  text <- str_to_lower(text, locale = "fr") # to min
  text <- str_squish(text) # rm space
  return(text)
}

rm_accent <- function(text) {
  # need to keep the accent for the sentiment analysis
  text <- iconv(text, to = "ASCII//TRANSLIT//IGNORE")   # rm accent
  text <- gsub("['`^~\"]", " ", text)  # rm accent
}

extr_hashtags_mentions <- function(text, type = c("hashtags", "mentions")) {
  if (is.null(type)) stop("type needed")
  patrn <- switch(type, hashtags = "#[[:alnum:]_]*", mentions = "@[[:alnum:]_]*")
  str_extract_all(text, pattern = patrn, simplify = TRUE)
}

# https://www.earthdatascience.org/courses/earth-analytics/get-data-using-apis/text-mine-colorado-flood-tweets-science-r/

# path
indir <- "data/tweet/"
oudir <- "tweetair/"

# read data ---------------------------------------------------------------
# stop words & commune information
stopwordfr <- stopwords::stopwords('fr')

# reformat to have a list of all names (dept region..)
communesfr <- read_rds("data/processed/commune.Rds") %>% 
  select(reg_name, dep_name, com_name) %>% # extract name
  unlist %>% # together in one vector
  str_to_lower %>% # reformat
  str_replace_all("-", " ") %>% # reformat
  unique # unique ==> ready to compare with twitter

# all the tweets (aq & cc)
all_tweets <- map_df(dir(indir, full.names = T), read_twitter_csv)
aq <- map_df(dir(indir, pattern = "aq", full.names = T), read_twitter_csv)
# process data -------------------------------------------------------------
# clean tweets
all_tweets_text <- all_tweets %>% 
  mutate(
    text_clean = clean_tweet(text),
    description_clean  = clean_tweet(description),
    location_clean = clean_tweet(rm_accent(location))
  ) 

# extract localisation information
# see geo_coords, coords_coords (need api google maps) 
# country, bbox_coords, place_full_name, **location**
# here may have fr or something: quoted_description, quoted_name

# first find localisation word to create the matching list 
# use distinct to avoid double counting of the same author
location <- all_tweets_text %>% 
  mutate(location = rm_accent(location_clean)) %>% 
  distinct(user_id, location_clean) %>% 
  unnest_tokens(word, location_clean) %>% 
  count(word, sort = T) %>% 
  filter(!word %in% stopwordfr) %>%
  distinct 

# match with my db of fr names (not exhaustif but ok)
location_fr <- left_join(tibble(word = c(communesfr, 'france')), 
                         y = location, by = "word") %>% drop_na 

# now i can recode my var geo
pattrn <- str_c(location_fr$word, collapse = "|")
all_tweets_text <- all_tweets_text %>% 
  mutate(
    geo = ifelse(str_detect(location_clean, pattern = pattrn), 
                 yes = "fr", no = "other") 
  ) 

count(all_tweets_text, geo)
 #%>% 
  # ggplot(aes(x = fct_reorder(geo, n), y = n )) +
  # geom_col() + 
  # coord_flip()


top_user <- all_tweets_text %>% 
  count(screen_name, query, sort = TRUE) %>% 
  filter(n >= 10) 

library(ggwordcloud)

query_uniq <- unique(top_user$query)

top_user %>%
  mutate(query_simple = case_when(
    query == "#changementclimatique" ~ "changement climatique",
    query == "#rechauffementclimatique" ~ "changement climatique",
    query == "#climatechange" ~ "changement climatique",
    query == "#airquality" ~ "qualité de l'air",
    query == "#qualitedelair" ~ "qualité de l'air",
    query == "#airpollution" ~ "qualité de l'air"
  )) %>% 
  ggplot(aes(label = screen_name,  size = n, color = query_simple)) +
  geom_text_wordcloud() +
  theme_minimal() -> p

# en rouge cc
# en bleu qa
ggsave(p, filename = "figs/top_user.png", width = 8, height = 5)

top_user_info <- left_join(top_user, y = select(all_tweets_text, c("screen_name","text", "source")))
top_user_info %>% 
  count(query, screen_name)

# give me the location
# tibble(text = all_tweets_text$location_clean, 
#        line = 1:length(all_tweets_text$location_clean)) %>% 
#   unnest_tokens(word, text) %>% 
#   count(word, sort = T) %>% 
#   filter(!word  %in% stopwordfr) %>%
#   distinct %>% 
#   filter(n >= 50) %>%  
#   ggplot(aes(fct_reorder(word,n), n)) + 
#   geom_col() + 
#   coord_flip()

# attention need to take first distinct user to avoir double counting location
# explore data ------------------------------------------------------------
### idea 
# => see connexion between hastags
# => see connexion between users
# => see freq of words/hastags
# => compare cc & aq

# compare that & query
count(all_tweets, query, sort = T)  %>% 
  ggplot(aes(x = fct_reorder(query, n),  y = n)) +
  geom_col() +
  labs(
    y = "occurence",
    x = "hashtags",
    title = "Classement des hashtags les plus populaires"
  ) + coord_flip() -> p1

# extract all hastags (see example after to map)
extr_hashtags_mentions(rm_accent(all_tweets_text$text_clean), type = "hashtags") %>% 
  as.tibble() %>%  
  gather() %>% 
  filter(value != "") %>%  
  count(value, sort = TRUE) %>%
  filter(n >= 150) %>% 
  ggplot(aes(x = fct_reorder(value, n),  y = n)) +
  geom_col() +
  coord_flip() +
  labs(
    y = "occurence",
    x = "hashtags",
    title = "Classement des hashtags les plus populaires dans les tweets collectés"
  ) -> p2

p3 <- cowplot::plot_grid(p1, p2, ncol = 1, labels = "auto")
ggsave(p3, filename = "figs/hashtags.png", width = 8, height = 7)

# extract all mention and find the occurence
mention <-  str_extract_all(all_tweets_text$text_clean, "@[[:alnum:]_]*", simplify = T) %>% 
  as.tibble() %>%  
  gather() %>% 
  filter(value != "") %>%  
  count(value, sort = TRUE) 

filter(mention, n >= 23) %>% 
  ggplot(aes(x = fct_reorder(value, n),  y = n)) +
  geom_col() +
  coord_flip() +
  labs(
    y = "occurence",
    x = "mentions",
    title = "Classement des comptes les plus mentionnés parmi les tweets collectés"
  ) -> p

ggsave(p, filename = "figs/mentions.png", width = 8, height = 5)

# need to clean the text more
tibble(text = rm_accent(all_tweets_text$text_clean), line = 1:length(all_tweets_text$text_clean)) %>% 
  unnest_tokens(word, text) %>% 
  count(word, sort = TRUE) %>% 
  filter(!word  %in% c(stopwordfr, "a", "via", "c'est", "plus", "contre", "si", "amp", "ca", "faire")) %>%
  filter(n >= 250) %>% 
  ggplot(aes(x = fct_reorder(word, n), y = n)) + 
  geom_col() +
  coord_flip() +
  labs(
    y = "occurence",
    x = "mots phares",
    title = "Classement des mots les plus fréquents parmi les tweets collectés"
  ) -> p
ggsave(p, filename = "figs/words.png", width = 8, height = 5)
