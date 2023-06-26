#' ---
#' Title: "Read json files"
#' Author: EcoINN
#' Date: "February 2023"
#' Output: data frame
#' ---
#' 


rjson <- function(path, pattern) {
  # Reads in all JSON files in a directory that match a specified pattern and returns a data frame
  #
  # Args:
  #    path: A character string specifying the path to the directory containing the JSON files
  #    pattern: A regular expression specifying the pattern to match the JSON file names
  #
  # Returns:
  #    A data frame containing the combined data from all the matching JSON files
  
  # Get a list of all JSON files in the directory that match the specified pattern
  file_names <- list.files(path = path, pattern = pattern, full.names = TRUE)
  
  # Read in each JSON file using jsonlite::stream_in(), and combine them into a single data frame using dplyr::bind_rows()
  data <- tryCatch(
    {
      purrr::map_dfr(file_names, ~ stream_in(file(.x)))
    },
    error = function(e) {
      message("Error reading in JSON files: ", e$message)
      NULL
    }
  )
  
  return(data)
}

