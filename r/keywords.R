#' Counting Keywords in Twitter Labels
#'
#' This script processes an Excel file containing updated Twitter posts and their associated labels, and counts
#' occurrences of specified keywords from a separate sheet within the same file. The results are stored in new columns
#' within the dataset. This enables further analysis of tweet content based on the presence of defined keywords.
#'
#' @author "Ecostack Innovations"
#' @date "July 2024"
#' @return An Excel file named 'MT_tweets_final.xlsx' containing the original tweet data along with additional columns
#' for keyword counts corresponding to each group. This file will be saved in the specified output directory.

# Load necessary libraries
library(readxl)      # For reading Excel files
library(writexl)     # For writing Excel files
library(dplyr)       # For data manipulation

# Load the Tweets data
tweets_file <- "C:/Ecostack/Projects/01_Selina/selina/output/Tweets/Mt_tweets_updated.xlsx"
tweets_data <- read_excel(tweets_file, sheet = "Tweets")

# Load the Keywords data
keywords_data <- read_excel(tweets_file, sheet = "Keywords")

# Create a named list for keywords
keyword_list <- keywords_data %>%
  deframe()

# Initialize new columns in tweets_data
for (group in names(keyword_list)) {
  if (!(group %in% names(tweets_data))) {
    tweets_data[[group]] <- NA  # Initialize with NA
  }
}

# Function to count keywords in labels
count_keywords <- function(labels, keywords) {
  sum(sapply(keywords, function(keyword) {
    sum(grepl(keyword, labels, ignore.case = TRUE))
  }))
}

# Iterate over each row and count keywords
for (i in 1:nrow(tweets_data)) {
  # Check if at least one label is not NA
  labels <- tweets_data[i, c("label_1", "label_2", "label_3", "label_4")]
  
  if (any(!is.na(labels))) {  # Only process if there's at least one non-NA label
    labels_combined <- paste(labels, collapse = ", ")
    
    for (group in names(keyword_list)) {
      tweets_data[i, group] <- count_keywords(labels_combined, unlist(strsplit(keyword_list[[group]], ", ")))
    }
  } else {
    # Set NA for all keyword groups if no labels are present
    for (group in names(keyword_list)) {
      tweets_data[i, group] <- NA
    }
  }
}

# Save the final data to a new Excel file
write_xlsx(tweets_data, "C:/Ecostack/Projects/01_Selina/selina/output/Tweets/MT_tweets_final.xlsx")
