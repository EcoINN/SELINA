#' ---
#' Title: "JSON Functions"
#' Author: EcoINN
#' Date: "November 2022"
#' Output: Word analysis
#' ---
#' 


# This function explores common words found on tweets
process_df <- function(df, lang = "en") {
  # Extract the URLs from the text column
  df <- df %>% 
    mutate(url = stringr::str_extract(text, "(https?://t\\.co/[^[:space:]]+)")) %>% 
    mutate(text = gsub("(https?://t\\.co/[^[:space:]]+)", "", text))
  
  # Filter the df based on the value of the 'lang' column
  df_ft <- df %>% 
    filter(lang == lang)
  
  # Replace '&amp;' for 'and'
  df_ft$text <- gsub("&amp;", "and", df_ft$text)
  
  # Clean and preprocess the data
  df_text <- df_ft %>% 
    select(text) %>% 
    unnest_tokens(word, text) %>% 
    anti_join(stop_words) 
  
  # remove https
  tweet_clean <- df_ft %>%
    mutate(tweet_text = gsub("\\s?(f|ht)(tp)(s?)(://)([^\\.]*)[\\.|/](\\S*)", 
                             "", text)) %>%  
    dplyr::select(text) %>%
    unnest_tokens(word, text) %>% 
    anti_join(stop_words) %>%
    filter(!word == "rt") # remove all rows that contain "rt" or retweet
  
  return(tweet_clean)
}


# This function plots the most common words
plot_words <- function(tweet_clean, x = x, y=y, title=title) {
  # plot the top 15 words
  tweet_clean %>%
    count(word, sort = TRUE) %>%
    top_n(15) %>%
    mutate(word = reorder(word, n)) %>%
    ggplot(aes(x = word, y = n)) +
    geom_col() +
    xlab(NULL) +
    coord_flip() +
    labs(x = x,
         y = y,
         title = title)
}


# This function performs a paired word analysis
paired_word <- function(tweet_data) {
  # clean up the data by removing stop words
  tweets_paired <- tweet_data %>%
    dplyr::select(tweet_text) %>%
    mutate(tweet_text = removeWords(tweet_text, stop_words$word)) %>%
    mutate(tweet_text = gsub("\\brt\\b|\\bRT\\b", "", tweet_text)) %>%
    mutate(tweet_text = gsub("http://*", "", tweet_text)) %>%
    unnest_tokens(paired_words, tweet_text, token = "ngrams", n = 2)
  
  tweets_paired %>%
    count(paired_words, sort = TRUE)
  
  # separate words into columns and count the unique combinations of words
  tweets_separated <- tweets_paired %>%
    separate(paired_words, c("word1", "word2"), sep = " ")
  
  # new bigram counts:
  word_counts <- tweets_separated %>%
    count(word1, word2, sort = TRUE)
  word_counts
}


# This function plots the result of the paired word analysis
plot_paired_words <- function() {
  word_counts %>%
    filter(n >= 50) %>%
    graph_from_data_frame() %>%
    ggraph(layout = "fr") +
    # geom_edge_link(aes(edge_alpha = n, edge_width = n)) +
    geom_node_point(color = "darkslategray4", size = 3) +
    geom_node_text(aes(label = name), vjust = 1.8, size = 3) +
    labs(title = "Paired word analysis",
         subtitle = "Twitter ",
         x = "", y = "") +
    theme_void()
}