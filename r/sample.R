#' Processing and Sampling Data from Twitter Posts
#'
#' This script processes data from an existing Excel file containing Twitter post classifications. It performs
#' the following tasks:
#' 1. Loads the data from a specified sheet.
#' 2. Filters rows where all specified classification fields are either 0 or NA.
#' 3. Samples 10% of the data where the `image_url1` field is not missing or empty.
#' 4. Adds the sampled data to a new sheet in the same Excel file.
#'
#' The file paths are specified using environment variables for improved security and flexibility.
#'
#' @author "Ecostack Innovations"
#' @date "July 2024"
#' @return The script updates an existing Excel file with a new sheet named "Sample" containing a 10% sample of
#' the filtered data. The file is saved with the new sheet included.
#'
#' @import readxl dplyr openxlsx
#' @export

# Load necessary libraries
library(readxl)   # For reading Excel files
library(dplyr)    # For data manipulation
library(openxlsx) # For writing Excel files

# Load the Tweets data using environment variable
tweets_file <- Sys.getenv("TWEETS_FILE_PATH")

# Read the "Classification" sheet from the existing file
data <- read_excel(tweets_file, sheet = "Classification")

# Specify fields to filter
fields_to_check <- c("Amphibians", "Birds", "Butterflies",
                     "Coastal and Oceanic landform", "Arthropods",
                     "Marine wildlife", "Nature Based Recreation", 
                     "Reptile", "Other Terrestrial animals", 
                     "Terrestrial landscapes", "Terrestrial Plants", "Water")

# Filter rows where the fields are all equal to 0 or NA
filtered_data <- data %>%
  filter(rowSums(select(., all_of(fields_to_check)) == 0 | is.na(select(., all_of(fields_to_check)))) == length(fields_to_check))

# Display the number of observations in the filtered data
print(paste("Number of observations:", nrow(filtered_data)))

# Get 10% of the data
# Randomly select 10% of the data where the image_url1 field is not missing or empty
fd <- data %>%
  filter(!is.na(image_url1) & image_url1 != "")

# Randomly sample 10% of the filtered data
set.seed(123)
sampled_data <- fd %>%
  sample_frac(0.1)

# Display the number of observations in the sampled data
print(paste("Number of observations in the sampled data:", nrow(sampled_data)))

# Load the existing workbook
wb <- loadWorkbook(tweets_file)

# Add the sampled data to a new sheet named "Sample"
addWorksheet(wb, "Sample")
writeData(wb, "Sample", sampled_data)

# Save the workbook with the new sheet
saveWorkbook(wb, tweets_file, overwrite = TRUE)
