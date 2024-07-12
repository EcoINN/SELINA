# Load necessary libraries
library(readxl)
library(dplyr)
library(openxlsx)
library(rlang)

# Step 1: Read the data from the three files
excel1 <- read_excel("C:/Ecostack/Projects/01_Selina/selina/output/Tweets/Mt_tweets.xlsx")
excel2 <- read.csv("C:/Ecostack/Projects/01_Selina/selina/output/URLS/URLs/extracted_image_urls.csv")
excel3 <- read_excel("C:/Ecostack/Projects/01_Selina/selina/output/URLS/Tags.xlsx")

# Step 2: Merge data from excel1 and excel2 based on the matching URLs
# Select relevant columns from excel2 containing image URLs
image_urls <- excel2 %>%
  select(tweet_url, image_url1:image_url8)

# Merge excel1 with image_urls based on the 'tweet_url' column in excel2 and 'url_1' column in excel1
merged_data <- left_join(excel1, image_urls, by = c("url_1" = "tweet_url"))

# Step 3: Merge the data from excel3 to merged_data by matching image URLs
# Create a function to find and merge labels for each image_url
# Arguments:
#   data: The data frame to merge labels into
#   tags: The data frame containing tags (excel3)
#   url_col: The name of the image URL column in the data frame
#   label_col: The name of the new label column to be created in the data frame
merge_labels <- function(data, tags, url_col, label_col) {
  # Perform left join between data and tags based on the image URL column
  data <- left_join(data, tags, by = setNames("image_url", url_col))
  
  # Rename the 'labels' column to the specified label column name
  data <- data %>%
    rename(!!label_col := labels)
  
  return(data)
}

# Step 4: Loop through each image_url column and merge labels
# Loop through image_url1 to image_url8
for (i in 1:8) {
  url_col <- paste0("image_url", i)
  label_col <- paste0("label_", i)
  # Merge labels for the current image URL column
  merged_data <- merge_labels(merged_data, excel3, url_col, label_col)
}

# Step 5: Write the updated data to a new Excel file
# Write the merged data to a new Excel file
write.xlsx(merged_data, "C:/Ecostack/Projects/01_Selina/selina/output/Tweets/Mt_tweets_updated.xlsx", row.names = FALSE)
