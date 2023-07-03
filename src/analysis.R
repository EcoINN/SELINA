#' Twitter analysis 
#' 
#' Set of functions to carry out the analysis
#' 
#' @author EcoINN
#' @date "November 2022"
#' @return Analysis 



#' Sentiment Analysis
#'
#' @param data A dataframe containing text data.
#' @param column A character string specifying the column of the data to analyze.
#' @param n_words An integer specifying the number of words to display in the plot.
#' @return A ggplot2 object displaying the sentiment analysis results.
#' @examples
#' \dontrun{
#'   sentiment_plot = sentiment_analysis(data = tweets_df, column = "lemmatized_text", n_words = 15)
#'   print(sentiment_plot)
#' }


sentiment_analysis <- function(data, column, n_words = 10, title = column) {
  # Unnest tokens
  words <- data %>% 
    unnest_tokens(output = word, input = !!sym(column))
  
  # Get sentiment
  sentiment <- words %>% 
    inner_join(get_sentiments("bing")) %>% 
    count(word, sentiment, sort = TRUE) %>% 
    ungroup()
  
  # Filter to get only the top n_words for each sentiment
  top_words <- sentiment %>%
    group_by(sentiment) %>%
    top_n(n_words, n) %>%
    ungroup()
  
  # Plot
  ggplot(top_words, aes(x = reorder(word, n), y = n, fill = sentiment)) +
    geom_col(show.legend = FALSE) +
    facet_wrap(~sentiment, scales = "free_y") +
    labs(y = "Contribution to sentiment",
         x = paste("Top words from '", title, "' column")) +
    coord_flip()
}



#' Spatial Analysis
#'
#' @param df A dataframe containing location data.
#' @param lon A character string specifying the column of the data representing longitude.
#' @param lat A character string specifying the column of the data representing latitude.
#' @param nsim An integer specifying the number of simulations for computing the K-function.
#' @return Two plots. The first plot displays the point pattern. The second plot displays the K-function with CSR Envelope.
#' @examples
#' \dontrun{
#'   spatial_analysis(df = tweets_df, lon = "longitude", lat = "latitude", nsim = 99)
#' }


spatial_analysis <- function(df, lon = "longitude", lat = "latitude", nsim = 99){
  # Check if the provided longitude and latitude columns exist in the dataframe
  if (!(lon %in% names(df)) | !(lat %in% names(df))) {
    stop("The provided longitude or latitude column name does not exist in the dataframe.")
  }
  
  # Convert longitude and latitude to numeric
  df[[lon]] <- as.numeric(as.character(df[[lon]]))
  df[[lat]] <- as.numeric(as.character(df[[lat]]))
  
  # Check for NA values in the longitude or latitude columns
  if (any(is.na(df[[lon]])) | any(is.na(df[[lat]]))) {
    stop("The provided longitude or latitude column contains NA values.")
  }
  
  # Check if longitude and latitude values are finite and fall within expected ranges
  if(any(!is.finite(df[[lon]])) | any(!is.finite(df[[lat]])) | 
     any(df[[lon]] < -180 | df[[lon]] > 180) | any(df[[lat]] < -90 | df[[lat]] > 90)) {
    stop("Longitude and latitude values must be finite and fall within valid ranges (-180 to 180 for longitude, -90 to 90 for latitude).")
  }
  
  # Create a bounding box (a window object in spatstat)
  bb <- owin(xrange = range(df[[lon]]), yrange = range(df[[lat]]))
  
  # Convert our spatial points to a ppp object which is required by the spatstat package
  ppp_object <- tryCatch({
    ppp(df[[lon]], df[[lat]], window = bb)
  }, error = function(e) {
    stop("Error converting to ppp object: ", e$message)
  })
  
  # Plot the point pattern
  plot(ppp_object, main = "Point Pattern")
  
  # Compute the K-function
  K <- envelope(ppp_object, fun = Kest, nsim = nsim)
  
  # Plot the result
  plot(K, main = "K-function with CSR Envelope")
}

