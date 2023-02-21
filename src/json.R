#' ---
#' Title: "Read json files"
#' Author: EcoINN
#' Date: "February 2023"
#' Output: data frame
#' ---
#' 


rjson <- function(path, pattern){
  # Creates a df with the information stored in json files
  #
  # Args:
  #    path: A df containing the tweets
  #    pattern: The regular expression to identify the files
  #
  # Returns: 
  #    A df with all the information 
  #
  files <- dir(path, pattern = pattern)
  data <- files %>%
    map_df(~fromJSON(file.path(path, .), flatten = TRUE))
  
  return(data)
}


