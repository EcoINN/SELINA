#' Extracting Image URLs from Twitter Posts
#'
#' This script reads a list of Twitter post URLs from an Excel file and extracts the direct URLs of images
#' embedded in each post. The script processes URLs column-wise from the Excel file, scrapes the necessary data
#' from each Twitter page, and outputs a list of image URLs for further use, such as analysis or archiving.
#' Note that the script's functionality depends on the structure of the Twitter webpage at the time of use
#' and is subject to Twitter's terms of service regarding scraping.
#'
#' @author "Ecostack Innovations"
#' @date "April 2024"
#' @return A CSV file named 'tweet_image_urls.csv' containing the original tweet URLs and their corresponding
#' image URLs, if any. This file will be saved in the working directory.


# Load necessary libraries
library(RSelenium)
library(readxl)
library(XML)

# Server
rD <- rsDriver(browser = "firefox")
remDr <- rD$client

# Read Excel file containing URLs
tweets_data <- read_excel("C:/Ecostack/02_Projects/01_Selina/selina/Mt_tweets.xlsx")
tweet_urls <- tweets_data$url_1

# Store image URLs
image_urls <- vector("list", length(tweet_urls))

# Iterate over tweet URLs to extract images
for (i in seq_along(tweet_urls)) {
  # Navigate to the tweet page
  remDr$navigate(tweet_urls[i])
  
  # Wait for the tweet to load and images to be present
  webElem <- remDr$findElements(using = 'css selector', "article img")
  if (length(webElem) > 0) {
    # Click on the first image
    webElem[[1]]$clickElement()
    Sys.sleep(5) # Allow time for the image overlay to open
    
    # Find the image element within the overlay and get the src attribute
    tryCatch({
      image <- remDr$findElement(using = 'css selector', 'img[alt="Image"]') # Adjust the selector as needed
      src <- image$getElementAttribute("src")[[1]]
      image_urls[[i]] <- src
    }, error = function(e) {
      message(paste("Error extracting image for URL", tweet_urls[i], ":", e$message))
      image_urls[[i]] <- NA
    })
  } else {
    image_urls[[i]] <- NA
  }
  
  Sys.sleep(1) # Throttle the requests slightly to avoid being blocked
}

# Stop RSelenium server and close browser
remDr$close()
rD$server$stop()

# Optionally, save the results to a CSV file or return them directly
write.csv(data.frame(tweet_url = tweet_urls, image_url = unlist(image_urls)), "extracted_image_urls.csv", row.names = FALSE)
