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
tweets_data <- read_excel("C:/Ecostack/02_Projects/01_Selina/selina/test.xlsx")
tweet_urls <- tweets_data$url_1

# Store image URLs
image_urls <- vector("list", length(tweet_urls))

# Function to wait for an element to be present
waitForElement <- function(driver, css_selector, timeout = 5) {
  start_time <- Sys.time()
  while (TRUE) {
    element <- driver$findElements(using = 'css selector', value = css_selector)
    if (length(element) > 0) return(element[[1]])
    if (difftime(Sys.time(), start_time, units = "secs") > timeout) {
      stop("Timeout reached waiting for element.")
    }
    Sys.sleep(0.5)  # Check every half second
  }
}

# Iterate over tweet URLs to extract images
for (i in seq_along(tweet_urls)) {
  remDr$navigate(tweet_urls[i])
  
  tryCatch({
    imgElement <- waitForElement(remDr, "article img", timeout = 10)
    imgElement$clickElement()
    
    overlay_image <- waitForElement(remDr, "div[aria-label='Image'] img", timeout = 5)  # Update based on actual selector
    src <- overlay_image$getElementAttribute("src")[[1]]
    image_urls[[i]] <- src
    
  }, error = function(e) {
    message(sprintf("Error at URL %s: %s", tweet_urls[i], e$message))
    image_urls[[i]] <- NA
  })
  
  # Attempt to close the overlay, if it's there
  tryCatch({
    close_button <- remDr$findElement(using = 'css selector', 'svg[aria-label="Close"]')
    close_button$click()
  }, error = function(e) {
    # If the close button isn't found, it's okay; just log the message
    message("Close button not found; overlay may have closed automatically or does not exist.")
  })
  
  Sys.sleep(2)  # Throttle requests
}

# Stop RSelenium server and close browser
remDr$close()
rD$server$stop()

# Optionally, save the results to a CSV file or return them directly
write.csv(data.frame(tweet_url = tweet_urls, image_url = unlist(image_urls)), "extracted_image_urls.csv", row.names = FALSE)
