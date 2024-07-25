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
#' @return A CSV file named 'extracted_image_urls.csv' containing the original tweet URLs and their corresponding
#' image URLs, if any. This file will be saved in the working directory.

# Load necessary libraries
library(RSelenium)   # For interacting with the Selenium server
library(readxl)      # For reading Excel files
library(xml2)        # For parsing HTML/XML content
library(wdman)       # For managing the WebDriver

# Function to connect to the running Selenium server
connect_rsDriver <- function() {
  # Specify the path to the GeckoDriver executable using environment variable
  geckodriver <- Sys.getenv("GECKODRIVER_PATH")
  
  # Initialize remote driver with specified GeckoDriver path and browserName "firefox"
  remDr <- remoteDriver(
    remoteServerAddr = "localhost",
    port = 4567L,
    browserName = "firefox",
    extraCapabilities = list(marionette = TRUE, geckodriver = geckodriver)
  )
  
  # Open the remote driver
  remDr$open()
  
  return(remDr)
}

# Function to check if a URL is accessible without logging in
is_accessible_without_login <- function(url) {
  page_content <- read_html(url)
  return(!grepl("Log in|Sign in|Log in to Twitter", page_content))
}

# Function to navigate to a URL
safe_navigate <- function(driver, url) {
  tryCatch({
    if (is.character(url) && !is.na(url) && nzchar(url) && length(url) == 1) {
      driver$navigate(url)
      return(TRUE)
    } else {
      stop("Invalid URL format")
    }
  }, error = function(e) {
    message(sprintf("Error navigating to URL %s: %s", url, e$message))
    return(FALSE)
  })
}

# Function to wait for elements to be present
waitForElements <- function(driver, css_selector, timeout = 20) {
  start_time <- Sys.time()
  elements <- list()
  while (TRUE) {
    elements <- try(driver$findElements(using = 'css selector', value = css_selector), silent = TRUE)
    if (!inherits(elements, "try-error") && length(elements) > 0) break
    if (difftime(Sys.time(), start_time, units = "secs") > timeout) {
      stop("Timeout reached waiting for elements.")
    }
    Sys.sleep(0.5)  # Check every half second
  }
  return(elements)
}

# Function to fetch image URLs for a tweet URL with retry mechanism
fetch_image_urls <- function(driver, tweet_url, max_retry = 2) {
  if (grepl("instagram\\.com", tweet_url, ignore.case = TRUE)) {
    message(sprintf("Skipping Instagram URL: %s", tweet_url))
    return(NA)
  }
  
  # Check if URL is accessible without logging in
  if (!is_accessible_without_login(tweet_url)) {
    message(sprintf("URL requires logging in: %s. Skipping.", tweet_url))
    return(NA)
  }
  
  # Log the current URL for debugging
  print(sprintf("Processing URL: %s", tweet_url))
  
  for (retry in 1:max_retry) {
    if (safe_navigate(driver, tweet_url)) {
      tryCatch({
        # Adjust CSS selector if necessary based on website structure
        imgElements <- waitForElements(driver, 'img[alt="Image"]', timeout = 40)
        
        # Extract the 'src' from each image element
        srcs <- sapply(imgElements, function(element) {
          element$getElementAttribute("src")[[1]]
        })
        
        # Return all image URLs if found
        if (length(srcs) > 0) {
          return(srcs)
        } else {
          message(sprintf("No images found. Retrying URL: %s (Attempt %d)", tweet_url, retry))
          Sys.sleep(2)  # Wait before retrying
        }
      }, error = function(e) {
        message(sprintf("Error at URL %s: %s. Retrying (Attempt %d)", tweet_url, e$message, retry))
        Sys.sleep(2)  # Wait before retrying
      })
    } else {
      message(sprintf("Failed to navigate URL: %s. Retrying (Attempt %d)", tweet_url, retry))
      Sys.sleep(2)  # Wait before retrying
    }
  }
  
  # Return NA if max retries reached
  return(NA)
}

# Connect to the running Selenium server
remDr <- connect_rsDriver()

# Read Excel file containing URLs using environment variable
tweets_data <- read_excel(Sys.getenv("TWEETS_EXCEL_PATH"))
tweet_urls <- tweets_data$url_1

# Store image URLs
image_urls <- vector("list", length(tweet_urls))

# Iterate over tweet URLs to extract images
for (i in seq_along(tweet_urls)) {
  current_url <- tweet_urls[[i]]  # Extract URL
  image_urls[[i]] <- fetch_image_urls(remDr, current_url)
  
  Sys.sleep(2)  # Throttle requests
}

# Flatten the image_urls list
image_urls_flat <- sapply(image_urls, function(urls) {
  if (is.null(urls) || length(urls) == 0) {
    return(NA)
  } else {
    paste(urls, collapse = " ")  # Consider using separator other than space if needed
  }
})

# Create the data frame
output_df <- data.frame(tweet_url = tweet_urls, image_url = image_urls_flat, stringsAsFactors = FALSE)

# Write to CSV using environment variable
write.csv(output_df, Sys.getenv("OUTPUT_CSV_PATH"), row.names = FALSE)

# Close browser session
tryCatch({
  remDr$close()
  remDr$server$stop()
}, error = function(e) {
  message("Error closing RSelenium session: ", e$message)
})
