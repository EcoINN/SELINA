#' Process and Update Tweet Data with Keywords and Filtering
#'
#' This script reads and processes data from two Excel files. It updates tweet data with keyword counts, 
#' filters out labels and scores based on a threshold, and writes the results to a new Excel file with 
#' separate sheets for filtered and classified data.
#'
#' Steps:
#' 1. Load necessary libraries.
#' 2. Define file paths using environment variables.
#' 3. Read the data from the specified Excel files.
#' 4. Process the data to count occurrences of keywords from a predefined list.
#' 5. Filter out labels and scores below a specified threshold.
#' 6. Write the updated data to a new Excel file with separate sheets for filtered and classified data.
#'
#' @author EcoStack Innovations
#' @date July 2024
#' @return An updated Excel file named as specified in the environment variable, containing two sheets: 
#' "Filtered" with data meeting the filtering criteria and "Classification" with original and keyword count data.
#'
#' @details Ensure that the environment variables are correctly set to point to the appropriate file paths.

# Load necessary libraries
library(readxl)       # For reading Excel files
library(dplyr)        # For data manipulation
library(openxlsx)     # For writing Excel files

# Define file paths using environment variables
input_file_path <- Sys.getenv("INPUT_FILE_PATH")  # Path to the resulting Excel file
keywords_file_path <- Sys.getenv("KEYWORDS_FILE_PATH")  # Path to the Keywords Excel file

# Step 1: Read the data from the resulting Excel file
merged_data <- read_excel(input_file_path)

# Step 2: Read the Keywords data
keywords_data <- read_excel(keywords_file_path)

# Convert Keywords data to a named list
keyword_list <- setNames(
  strsplit(keywords_data$Keywords, ", ") %>% lapply(trimws),
  keywords_data$`Keyword groups`
)

# Initialize new columns for keyword counts in the merged_data
for (group in names(keyword_list)) {
  merged_data[[group]] <- NA  # Initialize with NA
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
      merged_data[i, group] <- NA
    }
  }
}

# Step 3: Create a copy of the merged data for filtering
filtered_data <- merged_data

# Loop through each image_url column and remove labels and scores if score is < 0.6
for (i in 1:4) {
  for (j in 1:10) {
    label_col <- paste0("label_", i, "_", j)
    score_col <- paste0("score_", i, "_", j)
    
    # Remove label and score if score is < 0.6
    filtered_data <- filtered_data %>%
      mutate(
        !!label_col := ifelse(get(score_col) < 0.6, NA, get(label_col)),
        !!score_col := ifelse(get(score_col) < 0.6, NA, get(score_col))
      )
  }
}

# Step 4: Write the original data and the filtered data to the same Excel file with different sheets
wb <- loadWorkbook(input_file_path)

# Check if "Filtered" sheet exists and remove it if necessary
if ("Filtered" %in% names(wb)) {
  removeWorksheet(wb, "Filtered")
}

# Add the filtered data to a new sheet named "Filtered"
addWorksheet(wb, "Filtered")
writeData(wb, "Filtered", filtered_data)

# Check if "Classification" sheet exists and remove it if necessary
if ("Classification" %in% names(wb)) {
  removeWorksheet(wb, "Classification")
}

# Add the classified data to a new sheet named "Classification"
addWorksheet(wb, "Classification")
writeData(wb, "Classification", merged_data)

# Save the workbook
saveWorkbook(wb, input_file_path, overwrite = TRUE)
