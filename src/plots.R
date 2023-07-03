#' Plots
#' 
#' Set of functions to generate maps and plots
#' 
#' @author EcoINN
#' @date "December 2022"
#' @return map/plots


#' Maps Tweet Locations Using Leaflet
#'
#' @param df A dataframe with long/lat data.
#' @param lat_col The name of the column with latitude data.
#' @param lon_col The name of the column with longitude data.
#' @return A leaflet map of all tweets in the df.
#' @examples
#' \dontrun{
#'   # Assuming df is your DataFrame and "latitude" and "longitude" are your column names
#'   leaflet_map = maps(df, "latitude", "longitude")
#'   print(leaflet_map)
#' }


maps <- function(df, lat_col, lon_col) {
  # Ensure the latitude and longitude columns are correctly recognized
  if (!(lat_col %in% names(df)) | !(lon_col %in% names(df))) {
    stop("Latitude and/or longitude columns not found in the data frame.")
  }
  
  # Remove rows where latitude or longitude is NA
  tweet_locations <- df %>%
    filter(!is.na(df[[lat_col]]), !is.na(df[[lon_col]]))
  
  print(paste("Number of points:", nrow(tweet_locations)))
  
  # Print first few rows of tweet_locations
  print(head(tweet_locations))
  
  # Plot points on top of a leaflet basemap
  site_locations_base <- leaflet(tweet_locations) %>%
    addProviderTiles("CartoDB.Positron") %>%
    addCircleMarkers(
      lng = ~df[[lon_col]], 
      lat = ~df[[lat_col]], 
      popup = ~as.character(df$text),
      radius = 3, stroke = FALSE,
      clusterOptions = markerClusterOptions()
    )
  
  return(site_locations_base)
}



#' Plots Most Common Words
#'
#' @param data A dataframe.
#' @param column A character string specifying the column to analyze.
#' @param num_words An integer specifying the number of most common words to plot. Default is 20.
#' @return A ggplot2 plot.
#' @examples
#' \dontrun{
#'   # Assuming df is your DataFrame and "text" is your text column
#'   common_words_plot = plot_words(df, "text", 10)
#'   print(common_words_plot)
#' }


plot_words <- function(data, column, num_words = 20) {
  # Unnest the tokens in the specified column
  data_unnested <- data %>% 
    unnest_tokens(word, !!sym(column))
  
  # Count the frequency of each word
  word_frequency <- data_unnested %>%
    count(word, sort = TRUE)
  
  # Plot the most common words
  ggplot(word_frequency[1:num_words, ], aes(x = reorder(word, n), y = n)) +
    geom_col(fill = 'steelblue') +
    coord_flip() +
    labs(x = "Words",
         y = "Frequency",
         title = paste("Most common words in '", column, "' column"))
}



#' Plots Most Common Bigrams
#'
#' @param data A dataframe.
#' @param column A character string specifying the column to analyze.
#' @param num_bigrams An integer specifying the number of most common bigrams to plot. Default is 20.
#' @return A ggplot2 plot.
#' @examples
#' \dontrun{
#'   # Assuming df is your DataFrame and "text" is your text column
#'   bigrams_plot = plot_bigrams(df, "text", 10)
#'   print(bigrams_plot)
#' }


plot_bigrams <- function(data, column, num_bigrams = 20) {
  # Unnest the tokens in the specified column
  data_unnested <- data %>% 
    unnest_tokens(output = bigram, input = !!sym(column), token = "ngrams", n = 2)
  
  # Count the frequency of each bigram
  bigram_frequency <- data_unnested %>%
    count(bigram, sort = TRUE)
  
  # Filter out the NAs
  bigram_frequency <- bigram_frequency %>% 
    filter(!is.na(bigram))
  
  # Plot the most common bigrams
  ggplot(bigram_frequency[1:num_bigrams, ], aes(x = reorder(bigram, n), y = n)) +
    geom_col(fill = 'steelblue') +
    coord_flip() +
    labs(x = "Bigrams",
         y = "Frequency",
         title = paste("Most common bigrams in '", column, "' column"))
}



#' Plots Most Common Trigrams
#'
#' @param data A dataframe.
#' @param column A character string specifying the column to analyze.
#' @param num_trigrams An integer specifying the number of most common trigrams to plot. Default is 20.
#' @return A ggplot2 plot.
#' @examples
#' \dontrun{
#'   # Assuming df is your DataFrame and "text" is your text column
#'   trigrams_plot = plot_trigrams(df, "text", 10)
#'   print(trigrams_plot)
#' }


plot_trigrams <- function(data, column, num_trigrams = 20) {
  # Unnest the tokens in the specified column
  data_unnested <- data %>% 
    unnest_tokens(output = trigram, input = !!sym(column), token = "ngrams", n = 3)
  
  # Count the frequency of each trigram
  trigram_frequency <- data_unnested %>%
    count(trigram, sort = TRUE)
  
  # Filter out the NAs
  trigram_frequency <- trigram_frequency %>% 
    filter(!is.na(trigram))
  
  # Plot the most common trigrams
  ggplot(trigram_frequency[1:num_trigrams, ], aes(x = reorder(trigram, n), y = n)) +
    geom_col(fill = 'steelblue') +
    coord_flip() +
    labs(x = "Trigrams",
         y = "Frequency",
         title = paste("Most common trigrams in '", column, "' column"))
}