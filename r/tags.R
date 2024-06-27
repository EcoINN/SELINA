# Load required libraries
library(readxl)
library(dplyr)
library(tidyr)
library(openxlsx)

# Read the Excel and CSV files
excel1 <- read_excel("C:/Ecostack/Projects/01_Selina/selina/output/Tweets/Mt_tweets.xlsx")
excel2 <- read.csv("C:/Ecostack/Projects/01_Selina/selina/output/URLS/URLs/extracted_image_urls.csv")
excel3 <- read_excel("C:/Ecostack/Projects/01_Selina/selina/output/URLS/Tags.xlsx")

# Ensure URLs are character type
excel1$url_1 <- as.character(excel1$url_1)
excel2$tweet_url <- as.character(excel2$tweet_url)
excel3$image_url <- as.character(excel3$image_url)

# Match url_1 in excel1 with tweet_url in excel2 and add image_url columns to excel1
merged_data <- excel1 %>%
  left_join(excel2, by = c("url_1" = "tweet_url"))

# Initialize empty columns for labels
for (i in 1:8) {
  merged_data[paste0("label_", i)] <- NA
}

# Match image_url columns with image_url in excel3 and add labels to the corresponding columns
for (i in 1:8) {
  image_col <- paste0("image_url", i)
  label_col <- paste0("label_", i)
  
  if (image_col %in% names(merged_data)) {
    merged_data <- merged_data %>%
      left_join(excel3, by = setNames("image_url", image_col)) %>%
      mutate(!!label_col := coalesce(!!sym(label_col), labels)) %>%
      select(-labels)
  }
}

# Write the final dataframe to a new Excel file
write.xlsx(merged_data, "C:/Ecostack/Projects/01_Selina/selina/output/Tweets/merged_output.xlsx")
