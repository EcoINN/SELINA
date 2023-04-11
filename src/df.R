#' ---
#' Title: "Tweets processing"
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
    # filter(lang == "en") %>%
    mutate(text = gsub("&amp;", "and", text)) %>%
    select(-url1) %>%
    mutate(hashtags = sapply(str_extract_all(text, "#\\w+"), 
                             function(x) str_c(unlist(x), collapse = " "))) %>%
    mutate(hashtags = ifelse(hashtags == "", NA , hashtags))
  
  return(tweets)
}


clean_text <- function(text) {
  # Cleans and preprocess the text data.
  #
  # Args:
  #    The df column containing the tweets text
  #
  # Returns: 
  #    A df with a clean text
  #
  # Convert text to lowercase
  text <- tolower(text)
  # Remove URLs, mentions, and hashtags
  text <- gsub("http[^[:blank:]]*|@[^[:blank:]]*|#\\S+", "", text)
  # Remove emojis and other Unicode symbols, except spaces
  text <- stri_replace_all_regex(text, "[\\p{So}\\p{Cs}\\p{Cn}]+", "")
  # Remove punctuation and numbers
  text <- gsub("[[:punct:]]|[[:digit:]]", "", text)
  # Remove extra whitespace
  text <- gsub("\\s+", " ", text)
  # Remove leading and trailing whitespace
  text <- str_trim(text)
  return(text)
}


process_urls <- function(df) {
  # Takes a data frame as input, separates multiple URLs in the expanded_url 
  # column into different columns, and filters out the URLs that don't have 
  # the word 'photo' in them.
  #
  # Args:
  #    A df
  #
  # Returns: 
  #    A df with filtered URLs
  #
  # Split the 'expanded_url' column by "," and create new columns
  df <- df %>%
    separate(expanded_url, into = paste0("url_", 1:5), sep = ",", 
             remove = FALSE, extra = "merge", fill = "right") %>%
    select(-expanded_url)
  
  # Create a function to filter out URLs without the word 'photo'
  filter_photo_urls <- function(url_col) {
    ifelse(grepl("photo", url_col, ignore.case = TRUE), url_col, NA)
  }
  
  # Apply the function to each new URL column
  for (col_name in colnames(df)[startsWith(colnames(df), "url_")]) {
    df[[col_name]] <- sapply(df[[col_name]], filter_photo_urls)
  }
  
  return(df)
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
