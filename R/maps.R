#' ---
#' Title: "JSON Functions"
#' Author: EcoINN
#' Date: "December 2022"
#' Output: Map function
#' ---


# This function 
maps <- function(tweet_data) {
  # cleanup & and filter
  mt_tweets <- tweet_data %>%
    mutate(geo.coordinates = gsub("\\)|c\\(", "", geo.coordinates)) %>%
    separate(geo.coordinates, c("long", "lat"), sep = ", ") %>%
    mutate_at(c("lat", "long"), as.numeric) 
  
  # create basemap of the globe
  world_basemap <- ggplot() +
    borders("world", colour = "gray85", fill = "gray80")
  
  world_basemap
  
  # remove na values
  tweet_locations <- tweet_data %>%
    na.omit()
  
  head(tweet_locations)
  
  # plot the data
  world_basemap +
    geom_point(data = tweet_locations, aes(x = long, y = lat),
               colour = 'purple', alpha = .5) +
    scale_size_continuous(range = c(1, 8),
                          breaks = c(250, 500, 750, 1000)) +
    labs(title = "Tweet Locations")
  
  
  # plot points on top of a leaflet basemap
  site_locations <- leaflet(tweet_locations) %>%
    addTiles() %>%
    addCircleMarkers(lng = ~long, lat = ~lat, popup = ~tweet_text,
                     radius = 3, stroke = FALSE)
}