#' ---
#' Title: "Map function (leaflet)"
#' Author: EcoINN
#' Date: "December 2022"
#' Output: Map function
#' ---


maps <- function(df, lat_col, lon_col) {
  # This function uses leaflet to map tweets
  #
  # Args:
  #    df: A data frame with long/lat data
  #    lat_col: The name of the column with latitude data
  #    lon_col: The name of the column with longitude data
  #
  # Returns: 
  #    A map of all tweets in the df
  
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



plot_words <- function(tweet_clean, x = x, y=y, title=title, no) {
  # This function plots the most common words
  #
  # Args:
  #    tweet_clean: A df with a column containing all the words
  #    x: Title for the x axis
  #    y: Title for the y axis
  #    title: The title for the plot
  #    no: Number of words to show in the plot
  #
  # Returns: 
  #    A plot of the common words
  #
  tweet_clean %>%
    count(word, sort = TRUE) %>%
    top_n(no) %>%
    mutate(word = reorder(word, n)) %>%
    ggplot(aes(x = word, y = n)) +
    geom_col() +
    xlab(NULL) +
    coord_flip() +
    labs(x = x,
         y = y,
         title = title)
}


plot_paired_words <- function(word_counts, title=title, subtitle=subtitle, no) {
  # This function plots the result of the paired word analysis
  #
  # Args:
  #    word_counts: A df with a column containing all the words
  #    title: The title for the plot
  #    no: Number of paired words to show in the plot
  #
  # Returns: 
  #    A plot of the paired words
  #
  word_counts %>%
    filter(n >= no) %>%
    graph_from_data_frame() %>%
    ggraph(layout = "fr") +
    geom_node_point(color = "darkslategray4", size = 3) +
    geom_node_text(aes(label = name), vjust = 1.8, size = 3) +
    labs(title = title,
         subtitle = subtitle,
         x = "", y = "") +
    theme_void()
}


plot_common_words <- function(df, column_name, n = 10) {
  # This function tokenizes the text, counts the frequency of each word, 
  # and plots the top n common words.
  #
  # Args:
  #    df: A dataframe containing the text column to analyze
  #    column_name: The name of the text column to analyze
  #    n: The number of top common words to plot (default is 10)
  #
  
  # Check if the input is a dataframe
  if (!is.data.frame(df)) {
    stop("The input df must be a data frame.")
  }
  
  # Check if the column_name is a character vector
  if (!is.character(column_name) || length(column_name) != 1) {
    stop("The column_name must be a single character string.")
  }
  
  # Check if the n is numeric and greater than 0
  if (!is.numeric(n) || n <= 0) {
    stop("The n value must be a positive number.")
  }
  
  # Tokenize the text
  tokenized_text <- df %>%
    select_(column_name) %>%
    unnest_tokens(word, !!as.name(column_name))
  
  # Count the frequency of each word
  word_counts <- tokenized_text %>%
    count(word, sort = TRUE) %>%
    filter(!word %in% stop_words$word) # Remove stop words
  
  # Plot the top n common words
  ggplot(head(word_counts, n), aes(reorder(word, n), n, fill = word)) +
    geom_col(show.legend = FALSE) +
    coord_flip() +
    labs(x = "Words", y = "Frequency", title = "Top Common Words") +
    theme_minimal()
}