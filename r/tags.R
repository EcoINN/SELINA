#' Merging and Labeling Image URLs from Twitter Posts
#'
#' This script combines data from multiple sources to enrich Twitter post information with image URLs and associated labels.
#' It reads a list of tweets, extracted image URLs, and corresponding tags from Excel and CSV files. The script merges
#' the data based on tweet URLs and generates labeled image columns for further analysis or reporting.
#' The functionality relies on consistent data structures across the files being processed.
#'
#' @author "Ecostack Innovations"
#' @date "July 2024"
#' @return An Excel file named 'Mt_tweets_labels.xlsx' containing the original tweet data along with additional
#' columns for extracted image URLs and their corresponding labels. This file will be saved in the specified output directory.

# Load necessary libraries
library(readxl)       # For reading Excel files
library(dplyr)        # For data manipulation and joining
library(openxlsx)     # For writing Excel files
library(rlang)        # For advanced programming with R

# Define file paths
input_excel1 <- Sys.getenv("MT_TWEETS_PATH")      # Path to Tweets Excel file
input_csv2 <- Sys.getenv("EXTRACTED_IMAGE_URLS_PATH") # Path to extracted image URLs CSV file
input_excel3 <- Sys.getenv("IMAGE_ANALYSIS_RESULTS_PATH") # Path to image analysis results Excel file
output_excel <- Sys.getenv("OUTPUT_EXCEL_PATH")  # Path for output Excel file

# Step 1: Read the data from the three files
excel1 <- read_excel(input_excel1)
excel2 <- read.csv(input_csv2)
excel3 <- read_excel(input_excel3)

# Step 2: Merge data from excel1 and excel2 based on the matching URLs
# Select relevant columns from excel2 containing image URLs
image_urls <- excel2 %>%
  select(tweet_url, image_url1:image_url8)

# Merge excel1 with image_urls based on the 'tweet_url' column in excel2 and 'url_1' column in excel1
merged_data <- left_join(excel1, image_urls, by = c("url_1" = "tweet_url"))

# Step 3: Merge the data from excel3 to merged_data by matching image URLs
# Create a function to find and merge labels and scores for each image_url
# Arguments:
#   data: The data frame to merge labels into
#   tags: The data frame containing tags (excel3)
#   url_col: The name of the image URL column in the data frame
#   label_cols: The names of the new label columns to be created in the data frame
#   score_cols: The names of the new score columns to be created in the data frame
merge_labels_scores <- function(data, tags, url_col, label_cols, score_cols) {
  # Perform left join between data and tags based on the image URL column
  merged <- left_join(data, tags, by = setNames("image_url", url_col))
  
  # Rename the label and score columns dynamically
  for (i in seq_along(label_cols)) {
    label_col <- paste0("label_", i)
    score_col <- paste0("score_", i)
    merged <- merged %>%
      rename_with(~label_cols[i], all_of(label_col)) %>%
      rename_with(~score_cols[i], all_of(score_col))
  }
  
  return(merged)
}

# Step 4: Loop through each image_url column and merge labels and scores
for (i in 1:8) {  # Assuming up to 8 image URLs columns
  url_col <- paste0("image_url", i)  # Construct the image URL column name
  label_cols <- paste0("label_", i, "_", 1:10)  # Construct the label column names
  score_cols <- paste0("score_", i, "_", 1:10)  # Construct the score column names
  
  # Merge labels and scores for the current image URL column
  merged_data <- merge_labels_scores(merged_data, excel3, url_col, label_cols, score_cols)
}

# Step 5: Reorder columns so each image URL is followed by its intercalated labels and scores
original_columns <- colnames(excel1)
new_columns <- c()
for (i in 1:8) {  # Assuming up to 8 image URLs columns
  new_columns <- c(new_columns, paste0("image_url", i))
  for (j in 1:10) {
    new_columns <- c(new_columns, paste0("label_", i, "_", j), paste0("score_", i, "_", j))
  }
}
column_order <- c(original_columns, new_columns)
merged_data <- merged_data %>% select(all_of(column_order))

# Step 6: Write the updated data to a new Excel file
# Write the merged data to a new Excel file
write.xlsx(merged_data, output_excel, rowNames = FALSE)
