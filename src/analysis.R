#' ---
#' Title: "Twitter analysis Functions"
#' Author: EcoINN
#' Date: "November 2022"
#' Output: twitter analysis
#' ---
#' 


common_words <- function(df) {
  # This function explores common words found on tweets
  #
  # Args:
  #    df_ft: A cleaned data frame
  #
  # Returns: 
  #    A df with only one column containing all the words
  #
  # Clean and preprocess the data
  tweet_clean <- df %>%
    mutate(tweet_text = gsub("\\s?(f|ht)(tp)(s?)(://)([^\\.]*)[\\.|/](\\S*)", 
                             "", text)) %>%  
    dplyr::select(text) %>%
    unnest_tokens(word, text) %>% 
    anti_join(stop_words) %>%
    filter(!word == "rt")
  
  return(tweet_clean)
}


tweet_bigrams <- function(df) {
  # This function explores common words found on tweets
  #
  # Args:
  #    df_ft: A cleaned data frame
  #
  # Returns: 
  #    A df with the words
  #
  # Remove stop words from the text column and remove RT, http
  tweets_paired <- df %>%
    dplyr::select(text) %>%
    mutate(text = stringr::str_remove_all(text, stop_words$word)) %>%
    mutate(text = stringr::str_replace_all(text, "\\brt\\b|\\bRT\\b", "")) %>%
    mutate(text = stringr::str_replace_all(text, "http://*", "")) %>%
    unnest_tokens(paired_words, text, token = "ngrams", n = 2)
  
  # Count the frequency of paired words
  tweets_paired %>%
    count(paired_words, sort = TRUE)
  
  # Separate words into two columns and count the unique combinations of words
  tweets_separated <- tweets_paired %>%
    separate(paired_words, c("word1", "word2"), sep = " ")
  
  # Count the frequency of word combinations
  word_counts <- tweets_separated %>%
    count(word1, word2, sort = TRUE)
  
  return(word_counts)
}