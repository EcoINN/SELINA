#' Twitter analysis 
#' 
#' Set of functions to carry out the analysis
#' 
#' @author EcoINN
#' @date "November 2022"
#' @return Analysis 



#' Hotspot Analysis
#'
#' Conducts a hotspot analysis using the Getis-Ord Gi* statistic.
#'
#' @param df A dataframe containing the visit data.
#' @param k K-nearest neighbors (KNN) 
#' @return A dataframe with the original data and an additional column with the Getis-Ord Gi* values.
#' @examples
#' \dontrun{
#'   hotspot_df <- hotspot_analysis(df = malta, lon = "longitude", lat = "latitude")
#' }


hotspot_analysis <- function(df, k, dmin, dmax) {
  
  # Convert DataFrame to sf object
  df <- st_as_sf(df, coords = c("longitude", "latitude"), crs = 4326)
  
  # Transform to a projected CRS (Mercator)
  df_projected <- st_transform(df, 3395)
  
  # Add a new attribute with a constant value
  df_projected$tweet_value <- 1
  
  # Calculate spatial weights with KNN
  nb <- knn2nb(knearneigh(df_projected, k=k))
  
  # Create a listw object
  listw <- nb2listw(nb, style="W")
  
  # Calculate Getis-Ord Gi* statistic
  getis_ord <- localGS(df_projected, listw, dmin = dmin, dmax = dmax, attr = "tweet_value")
  
  # Extract p-values from the result
  p_values <- getis_ord[, "p"]
  
  # Plot the p-values
  plot(df_projected$geometry, col = ifelse(p_values < 0.05, "red", "blue"), main = "Getis-Ord Gi* Statistic")
}



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



# Function for time series analysis
time_series_analysis <- function(df) {
  # Extract wet and dry seasons from the seasonal tweets data
  wet_season <- df[df$season == "Wet", ]
  dry_season <- df[df$season == "Dry", ]
  
  # Convert year and month columns to appropriate data types
  wet_season$year <- as.factor(wet_season$year)
  wet_season$month <- as.factor(wet_season$month)
  dry_season$year <- as.factor(dry_season$year)
  dry_season$month <- as.factor(dry_season$month)
  
  # Calculate monthly tweet counts for wet and dry seasons
  wet_monthly_counts <- aggregate(tweets ~ year + month, data = wet_season, FUN = length)
  dry_monthly_counts <- aggregate(tweets ~ year + month, data = dry_season, FUN = length)
  
  # Create plots for wet and dry seasons
  wet_plot <- ggplot(wet_monthly_counts, aes(x = month, y = tweets, group = year, color = year)) +
    geom_line() +
    labs(x = "Month", y = "Tweet Count", title = "Monthly Tweet Counts - Wet Season") +
    theme_minimal()
  
  dry_plot <- ggplot(dry_monthly_counts, aes(x = month, y = tweets, group = year, color = year)) +
    geom_line() +
    labs(x = "Month", y = "Tweet Count", title = "Monthly Tweet Counts - Dry Season") +
    theme_minimal()
  
  # Create a list to store the results
  results <- list()
  results$wet_season <- wet_season
  results$dry_season <- dry_season
  results$wet_plot <- wet_plot
  results$dry_plot <- dry_plot
  
  return(results)
}



#' Temporal Analysis
#'
#' Analyses the frequency of visits across different temporal scales (daily, weekly, monthly, yearly).
#'
#' @param df A dataframe containing the visit data.
#' @param date_col The column containing the date.
#' @return A list of frequency tables for each temporal scale.
#' @examples
#' \dontrun{
#'   freq_analysis <- temporal_analysis(df = malta)
#'   print(freq_analysis$daily)
#'   print(freq_analysis$weekly)
#'   print(freq_analysis$monthly)
#'   print(freq_analysis$yearly)
#' }


temporal_analysis <- function(df, date_col="created_at"){
  # Ensure the date column is in the correct format
  df[[date_col]] <- as.POSIXct(df[[date_col]], format = "%Y-%m-%dT%H:%M:%S", tz = "UTC")
  
  # Extract the month, year and determine the season
  df$month <- month(df[[date_col]])
  df$year <- year(df[[date_col]])
  df$season <- ifelse(df$month %in% 4:9, "Dry", "Wet")
  
  # Count the number of visits per season
  season_counts <- df %>%
    group_by(year, season) %>%
    summarise(count = n()) %>%
    arrange(year, desc(count))
  
  print(season_counts)
  
  # Generate a bar plot of the number of visits per season
  ggplot(season_counts, aes(x = factor(year), y = count, fill = season)) +
    geom_bar(stat = "identity", position = "dodge") +
    labs(x = "Year", y = "Number of visits", title = "Visits per Season", fill = "Season") +
    theme_minimal()
  
  # Return the count data
  return(season_counts)
}


