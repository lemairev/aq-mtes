# Renviron not working force look into my lib
.libPaths(c(.libPaths(), "~/bin/Rlibs-3.4.3/"))

# load lib
library(rtweet)

# read the twitter auth ---------------------------------------------------------------------
Sys.setenv(TWITTER_PAT = 'rtweet_token.rds')

# param search ------------------------------------------------------------------------------
# limitations: i don't have ilestencoretemps eg 
keywords   <- list(
	aq = paste0("#", c("airquality", "airpollution", "pollutiondelair", "qualitedelair", "atmosphericpollution", "pollutionatmospherique")),
	cc = paste0("#", c("climatechange", "globalwarming", "changementclimatique", "rechauffementclimatique"))
)
tweet_args <- list(n = 18000, lang = "fr")
since_new  <- numeric(length(keywords))

# collect tweet per category ---------------------------------------------------------------- 
for (i in seq_along(keywords)) {	
	# info
	print(names(keywords)[i])
	
	# collect	
	if(!file.exists("since.Rds")) {
		tweets <- search_tweets2(keywords[[i]], n = tweet_args$n, lang = tweet_args$lang, 
		  											 include_rts = FALSE)
	} else {
		since  <- readRDS("since.Rds")
		tweets <- search_tweets2(keywords[[i]], n = tweet_args$n, lang = tweet_args$lang, 
													   include_rts = FALSE, since_id = since[i])
	}

	# return something? update since id	& save tweets 
	if(nrow(tweets) > 0) {
		since_new[i] <- since_id(tweets)
		ofile <- paste0("tweets_", names(keywords)[i], "_", format(Sys.time(), "%Y-%m-%d_%H"), ".csv") 

		write_as_csv(tweets, file_name = paste0("data/", ofile)) 
	} else {
		stop(paste("error when trying to collect, date:", format(Sys.time(), "%Y-%m-%d_%H")))
	}	
}
# save since 
saveRDS(since_new, file = "since.Rds")

