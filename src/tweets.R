#' Tweets
#' 
#' A function to get tweets
#' 
#' @author EcoINN
#' @date "February 2023"
#' @return tweets dataset (csv format)



#' Get Tweets
#'
#' @param json_file A character string specifying the path to the JSON file containing the bearer token.
#' @param keywords A character vector of keywords to include in the Twitter search query.
#' @param country A character string specifying the country to focus the Twitter search on.
#' @param start_date A date string (YYYY-MM-DD) specifying the start of the time period for the Twitter search.
#' @param end_date A date string (YYYY-MM-DD) specifying the end of the time period for the Twitter search.
#' @param datdir A character string specifying the directory to save the collected tweets.
#' @return A dataframe of collected tweets.
#' @examples
#' \dontrun{
#'   tweets_df = get_tweets(
#'       json_file = "path/to/your/json_file.json",
#'       keywords = c("climate change", "global warming"),
#'       country = "Malta",
#'       start_date = "2021-01-01",
#'       end_date = "2022-12-31",
#'       datdir = "path/to/save/tweets"
#'   )
#' }


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