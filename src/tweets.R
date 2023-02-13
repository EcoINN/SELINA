#' ---
#' Title: "Get tweets"
#' Author: EcoINN
#' Date: "February 2023"
#' Output: The raw df of tweets
#' ---
#' 

get_tweets <- function(json_file, keywords, country, start_date, end_date, datdir) {
  tokens <- fromJSON(json_file)
  bearer_token <- tokens$Bearer
  
  # Build a query
  query <- build_query(query = keywords, 
                       country = country,
                       is_retweet = FALSE,
                       has_media = TRUE,
                       has_images = NULL,
                       has_videos = NULL,
                       has_geo = NULL)
  
  # Get tweets
  tweets <-  get_all_tweets(query = query,
                            start_tweets = start_date,
                            end_tweets = end_date,
                            data_path = datdir,
                            n = Inf,
                            bearer_token = bearer_token)
  
  return(tweets)
}