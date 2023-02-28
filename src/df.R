#' ---
#' Title: "Tweets preprocessing"
#' Author: EcoINN
#' Date: "February 2023"
#' Output: data frame
#' ---
#' 


process_tweets <- function(tweets_df) {
  # Creates a df with the necessary information for this study
  #
  # Args:
  #    tweets_df: A df containing the tweets 
  #
  # Returns: 
  #    A df with only unique tweets 
  #
  # Check for necessary columns
  req_cols <- c("text", "lang", "entities.urls", "author_id", "created_at",
                "geo.coordinates.coordinates")
  missing_cols <- setdiff(req_cols, colnames(tweets_df))
  if (length(missing_cols) > 0) {
    stop(paste0("Missing required columns: ", paste(missing_cols, 
                                                    collapse = ", ")))
  }
  print("This may take a couple of minutes...")
  
  tweets <- tweets_df %>%
    unnest_wider(geo.coordinates.coordinates, names_sep = '.') %>%
    unnest_wider(entities.urls) %>%
    select(author_id, text, lang, created_at, 
           longitude = geo.coordinates.coordinates.1, 
           latitude = geo.coordinates.coordinates.2, 
           url, expanded_url, display_url, media_key) %>%
    distinct(text, .keep_all=TRUE) %>%
    mutate(url1 = stringr::str_extract(text, "(https?://t\\.co/[^[:space:]]+)")) %>%
    mutate(text = gsub("(https?://t\\.co/[^[:space:]]+)", "", text)) %>%
    filter(lang == "en") %>%
    mutate(text = gsub("&amp;", "and", text)) %>%
    select(-url1) %>%
    mutate(hashtags = sapply(str_extract_all(text, "#\\w+"), 
                             function(x) str_c(unlist(x), collapse = " "))) %>%
    mutate(hashtags = ifelse(hashtags == "", NA , hashtags))
  
  return(tweets)
}


remove_rows <- function(df, words) {
  # Create a function to remove rows containing certain words in a dataframe
  #
  # Args:
  #    df: A df containing the tweets 
  #    words: The words to find within the text
  #
  # Returns: 
  #    A modified df
  #
  # Create a logical vector indicating which rows contain the words to remove
  rows_to_remove <- grepl(paste(words, collapse = "|"), 
                          df$text, 
                          ignore.case = TRUE)
  
  # Remove the rows from the dataframe
  df <- df[!rows_to_remove, ]
  
  # Return the modified dataframe
  return(df)
}
