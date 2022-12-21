#' ---
#' Title: "JSON Functions"
#' Author: EcoINN
#' Date: "November 2022"
#' Output: json functions
#' ---


# This function makes the API call
connect_twitter <- function(jsonfile){
  tokens <- fromJSON(file = jsonfile) # Call JSON file
  bearer_token <- tokens$Bearer # Set bearer token
}


# This function creates a df
import_json <- function(json_file){
  # import json 
  file_json <- stream_in(file(json_file))
  # create new df 
  tweet_data <- data.frame(geo = file_json$geo,
                           date = file_json$created_at,
                           tweet_text = file_json$text,
                           source_id = file_json$source
                           #url = file_json$entities$urls
                           #hashtag = file_json$entities$hashtags
                           )
}