#' Enriching Twitter Data with Keyword Counts
#'
#' This script reads data from an Excel file that contains a "Sample" sheet of Twitter post classifications.
#' It also reads a separate Excel file containing keyword groups and their associated keywords. The script
#' performs the following tasks:
#' 1. Reads the data from the "Sample" sheet in the Excel file.
#' 2. Reads the keyword groups and their associated keywords.
#' 3. Counts occurrences of each keyword group in the labels of the data.
#' 4. Adds these counts as new columns in the data.
#' 5. Writes the updated data back to the "Sample" sheet in the same Excel file.
#'
#' The file paths are specified using environment variables for security and flexibility.
#'
#' @author "Ecostack Innovations"
#' @date "July 2024"
#' @return The script updates the "Sample" sheet in the specified Excel file with additional columns
#' for keyword counts. The file is saved with these updates.
#'
#' @import readxl dplyr openxlsx
#' @export

# Load necessary libraries
library(readxl)   # For reading Excel files
library(dplyr)    # For data manipulation
library(openxlsx) # For writing Excel files

# Define file paths using environment variables
input_file_path <- Sys.getenv("INPUT_PATH")
keywords_file_path <- Sys.getenv("KEYWORDS_PATH")

# Step 1: Read the data from the "Sample" sheet in the Excel file
merged_data <- read_excel(input_file_path, sheet = "Sample")

# Step 2: Read the Keywords data
keywords_data <- read_excel(keywords_file_path)

# Convert Keywords data to a named list
keyword_list <- setNames(
  strsplit(keywords_data$Keywords, ", ") %>% lapply(trimws),
  keywords_data$`Keyword groups`
)

# Initialize new columns for keyword counts in the merged_data
for (group in names(keyword_list)) {
  merged_data[[group]] <- 0  # Initialize with 0
}

# Function to count keywords in labels
count_keywords <- function(labels, keywords) {
  sum(sapply(keywords, function(keyword) {
    sum(grepl(keyword, labels, ignore.case = TRUE))
  }))
}

# Add keyword counts to each row
for (i in 1:nrow(merged_data)) {
  labels <- merged_data[i, grep("^label_", names(merged_data))]
  
  if (any(!is.na(labels))) {
    labels_combined <- paste(labels, collapse = ", ")
    
    for (group in names(keyword_list)) {
      merged_data[i, group] <- count_keywords(labels_combined, keyword_list[[group]])
    }
  } else {
    for (group in names(keyword_list)) {
      merged_data[i, group] <- 0
    }
  }
}

# Step 3: Write the updated data back to the "Sample" sheet
wb <- loadWorkbook(input_file_path)

# Write the updated data to the "Sample" sheet without removing it
writeData(wb, sheet = "Sample", merged_data, withFilter = FALSE)

# Save the workbook
saveWorkbook(wb, input_file_path, overwrite = TRUE)
