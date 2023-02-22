#' ---
#' Title: "Map function (leaflet)"
#' Author: EcoINN
#' Date: "December 2022"
#' Output: Map function
#' ---


maps <- function(df) {
  # This functions useas leaflet to map tweets
  #
  # Args:
  #    df: A data frame with long/lat data
  #
  # Returns: 
  #    A map of all tweets in the df
  #
  # Remove na values
  tweet_locations <- df %>%
    na.omit()
  head(tweet_locations)
  
  # Plot points on top of a leaflet basemap
  site_locations_base <- leaflet(tweet_locations) %>%
    addProviderTiles("CartoDB.Positron") %>%
    addCircleMarkers(lng = ~longitude, lat = ~latitude, popup = ~text,
                     radius = 3, stroke = FALSE)
  
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