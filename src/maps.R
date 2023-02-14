#' ---
#' Title: "Map function (leaflet)"
#' Author: EcoINN
#' Date: "December 2022"
#' Output: Map function
#' ---


# This function 
maps <- function(df) {
  # remove na values
  tweet_locations <- df %>%
    na.omit()
  head(tweet_locations)
  
  tw_locations <- tweet_locations %>% rename("long" = "coordinates.1",
                                             "lat" = "coordinates.2")
  # plot points on top of a leaflet basemap
  
  site_locations_base <- leaflet(tw_locations) %>%
    addProviderTiles("CartoDB.Positron") %>%
    addCircleMarkers(lng = ~long, lat = ~lat, popup = ~text,
                     radius = 3, stroke = FALSE)
  
  return(site_locations_base)
}