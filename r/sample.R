# Load necessary libraries
library(readxl)
library(dplyr)
library(openxlsx)

# Load the Tweets data
tweets_file <- "C:/Ecostack/Projects/01_Selina/selina/output/Analysis/Mt_tweets_labels.xlsx"

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

# Add the sampled data to a new sheet named "sample"
addWorksheet(wb, "Sample")
writeData(wb, "Sample", sampled_data)

# Save the workbook with the new sheet
saveWorkbook(wb, tweets_file, overwrite = TRUE)
