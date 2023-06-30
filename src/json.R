#' Read json files
#' 
#' Import a json file as data frame.
#' 
#' @author EcoINN
#' @date "February 2023"
#' @return data.frame



#' Read JSON Files into a Data Frame
#' 
#' This function reads in all JSON files in a directory that match a specified pattern, 
#' and returns a data frame with the combined data.
#'
#' @param path A character string specifying the path to the directory containing the JSON files.
#' @param pattern A regular expression specifying the pattern to match the JSON file names.
#' @return A data frame containing the combined data from all the matching JSON files.
#' @examples
#' \dontrun{
#'   df <- rjson(path = "./data", pattern = "^data.*$")
#' }


rjson <- function(path, pattern) {
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

