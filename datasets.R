#' Spatial Join and Aggregation of Social Media Data in Malta
#'
#' This script performs spatial analysis by joining and aggregating data from Twitter, Flickr, and iNaturalist 
#' with a predefined grid over Malta. The process involves transforming all datasets to a common coordinate reference system,
#' performing spatial joins, aggregating counts, and combining the results into a final spatial object. 
#' The result is a grid with aggregated social media data, useful for environmental or social studies.
#'
#' @author EcoStack
#' @date "January 2024"
#' @return A spatial object with aggregated social media data for each grid cell in Malta


# Load necessary libraries
library(sf)         # for handling spatial data
library(dplyr)      # for data manipulation


#' Reading and Preparing Data
#'
#' This section focuses on loading spatial data files for Twitter, Flickr, iNaturalist, and a grid layer. 
#' The datasets are transformed to match the grid's coordinate reference system.
#'
#' Steps:
#' 1. Load spatial data: Read shapefiles for Twitter, Flickr, iNaturalist, and the grid layer.
#' 2. Transform CRS: Ensure all datasets are in the same CRS as the grid layer.
#'
#' @param twitter_shp_path File path to the Twitter data shapefile.
#' @param flickr_shp_path File path to the Flickr data shapefile.
#' @param inaturalist_shp_path File path to the iNaturalist data shapefile.
#' @param grid_shp_path File path to the grid layer shapefile.


# Load the shapefiles
twitter <- st_read("C:/Ecostack/02_Projects/01_Selina/selina_arcgis/output/twitter.shp")
flickr <- st_read("C:/Ecostack/02_Projects/01_Selina/selina/data/Flickr/PUD.shp")
inaturalist <- st_read("C:/Ecostack/02_Projects/01_Selina/selina/data/iNaturalist/iNaturalist.shp")
grid <- st_read("C:/Ecostack/02_Projects/01_Selina/selina_arcgis/output/grid.shp")

# Use the grid's CRS as the target CRS
crs_target <- st_crs(grid) 
twitter_data <- st_transform(twitter, crs_target)
flickr_data <- st_transform(flickr, crs_target)
inaturalist_data <- st_transform(inaturalist, crs_target)

#' Performing Spatial Joins and Aggregating Data
#'
#' This section involves spatially joining the Twitter, Flickr, and iNaturalist data with the grid layer
#' and aggregating the counts for each social media platform within each grid cell.
#'
#' Steps:
#' 1. Spatial join: Join each social media dataset with the grid layer.
#' 2. Aggregate data: Count the number of data points from each platform in each grid cell.
#' 3. Combine aggregated data: Merge the counts from all platforms into a single dataset.
#' 4. Calculate total counts: Sum the counts from all platforms for each grid cell.
#'
#' @return A data frame with aggregated counts for each social media platform and total counts for each grid cell.

# Performing spatial joins
twitter_grid_join <- st_join(grid, twitter_data)
flickr_grid_join <- st_join(grid, flickr_data)
inaturalist_grid_join <- st_join(grid, inaturalist_data)

# Aggregating data
twitter_aggregated <- twitter_grid_join %>%
  group_by(PageNumber) %>%
  summarise(twitter_count = n(), .groups = 'drop') %>%
  st_set_geometry(NULL)

flickr_aggregated <- flickr_grid_join %>%
  group_by(PageNumber) %>%
  summarise(flickr_count = n(), .groups = 'drop') %>%
  st_set_geometry(NULL)

inaturalist_aggregated <- inaturalist_grid_join %>%
  group_by(PageNumber) %>%
  summarise(inaturalist_count = n(), .groups = 'drop') %>%
  st_set_geometry(NULL)

# Combining aggregated data
combined_aggregated <- full_join(twitter_aggregated, flickr_aggregated, by = "PageNumber")
combined_aggregated <- full_join(combined_aggregated, inaturalist_aggregated, by = "PageNumber")

# Replacing NA with 0
combined_aggregated[is.na(combined_aggregated)] <- 0

# Calculating total counts
combined_aggregated <- combined_aggregated %>%
  mutate(total_count = twitter_count + flickr_count + inaturalist_count)

#' Finalizing the Spatial Object
#'
#' This final section merges the combined aggregated data with the original grid layer to create
#' a spatial object that includes both the geometry of the grid and the aggregated social media data.
#'
#' Steps:
#' 1. Merge data: Combine the aggregated social media data with the grid layer.
#' 2. Write to Shapefile: Save the final spatial object as a new Shapefile.
#'
#' @return A spatial object (Shapefile) with the grid geometry and aggregated social media data.

# Merging data with the grid
final_grid <- left_join(grid, combined_aggregated, by = "PageNumber")

# Ensuring the final object is spatial
final_grid <- st_as_sf(final_grid)

# Now you can write the final grid to a new Shapefile
st_write(final_grid, "C:/Ecostack/02_Projects/01_Selina/selina_arcgis/output/final_grid.shp")
