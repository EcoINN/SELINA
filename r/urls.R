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
library(xml2)

# Server
rD <- rsDriver(browser = "firefox")
remDr <- rD$client

# Read Excel file containing URLs
tweets_data <- read_excel("C:/Ecostack/02_Projects/01_Selina/selina/Mt_tweets.xlsx")
tweet_urls <- tweets_data$url_1

# Store image URLs
image_urls <- vector("list", length(tweet_urls))

# Function to wait for an element to be present
waitForElements <- function(driver, css_selector, timeout = 5) {
  start_time <- Sys.time()
  elements <- list()
  while (TRUE) {
    elements <- driver$findElements(using = 'css selector', value = css_selector)
    if (length(elements) > 0) break
    if (difftime(Sys.time(), start_time, units = "secs") > timeout) {
      stop("Timeout reached waiting for elements.")
    }
    Sys.sleep(0.5)  # Check every half second
  }
  return(elements)
}

# Iterate over tweet URLs to extract images
for (i in seq_along(tweet_urls)) {
  remDr$navigate(tweet_urls[i])
  
  tryCatch({
    # Use waitForElements to get all image elements
    imgElements <- waitForElements(remDr, 'img[alt="Image"]', timeout = 20)
    
    # Extract the 'src' from each image element
    srcs <- sapply(imgElements, function(element) {
      element$getElementAttribute("src")[[1]]
    })
    
    # Store all image URLs
    image_urls[[i]] <- srcs
    
  }, error = function(e) {
    message(sprintf("Error at URL %s: %s", tweet_urls[i], e$message))
    image_urls[[i]] <- NA
  })
  
  Sys.sleep(2)  # Throttle requests
}

# Flatten the image_urls list
image_urls_flat <- sapply(image_urls, function(urls) {
  if (is.null(urls) || length(urls) == 0) {
    return(NA)
  } else {
    paste(urls, collapse = " ")
  }
})

# Create the data frame
output_df <- data.frame(tweet_url = tweet_urls, image_url = image_urls_flat, stringsAsFactors = FALSE)

# Write to CSV
write.csv(output_df, "extracted_image_urls.csv", row.names = FALSE)

# Stop RSelenium server and close browser
remDr$close()
rD$server$stop()