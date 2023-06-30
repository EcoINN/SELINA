#' Tweet Processing
#' 
#' Set of functions to carry out the preprocessing of the raw twitter data.
#' 
#' @author EcoINN
#' @date "February 2023"
#' @return data.frame


#' Clean and Pre-process Tweets
#' 
#' @param tweets_df A dataframe containing tweets.
#' @return A dataframe of cleaned and pre-processed tweets.
#' @examples
#' \dontrun{
#'   # Example using your process_tweets function here.
#' }
#'
process_tweets <- function(tweets_df) {
  # Validate input dataframe
  if (!inherits(tweets_df, "data.frame")) {
    stop("Input must be a data frame.")
  }
  
  # Check for necessary columns
  req_cols <- c("text", "lang", "entities.urls", "author_id", "created_at",
                "geo.coordinates.coordinates")
  missing_cols <- setdiff(req_cols, colnames(tweets_df))
  if (length(missing_cols) > 0) {
    stop(paste0("Missing required columns: ", paste(missing_cols, 
                                                    collapse = ", ")))
  }
  print("This may take a couple of minutes...")
  
  # Unnest and select relevant columns from the original dataframe
  tweets <- tweets_df %>%
    filter(lang == "en") %>% # Keep only English tweets
    unnest_wider(geo.coordinates.coordinates, names_sep = '.') %>%
    unnest_wider(entities.urls) %>%
    select(author_id, text, lang, created_at, 
           longitude = geo.coordinates.coordinates.1, 
           latitude = geo.coordinates.coordinates.2, 
           url, expanded_url, display_url, media_key) %>%
    distinct(text, .keep_all=TRUE) %>%
    mutate(url1 = stringr::str_extract(text, 
                                       "(https?://t\\.co/[^[:space:]]+)")) %>%
    mutate(text = gsub("(https?://t\\.co/[^[:space:]]+)", "", text)) %>%
    mutate(across(c(text, url1), ~gsub("&amp;", "and", .))) %>%
    select(-url1) %>%
    mutate(hashtags = sapply(str_extract_all(text, "#\\w+"), 
                             function(x) str_c(unlist(x), collapse = " "))) %>%
    mutate(hashtags = ifelse(hashtags == "", NA , hashtags)) %>%
    mutate(hashtags = gsub("#", "", hashtags)) %>% # Remove '#' from the hashtags
    mutate(hashtags = tolower(hashtags)) # Convert hashtags to lowercase
  
  # Clean the text and add it as a new column called 'clean_text'
  tweets$clean_text <- sapply(tweets$text, function(text) {
    # Convert text to lowercase
    text <- tolower(text)
    # Remove URLs, mentions, and hashtags
    text <- gsub("http[^[:blank:]]*|@[^[:blank:]]*|#\\S+", "", text)
    # Remove emojis and other Unicode symbols, except spaces
    text <- stri_replace_all_regex(text, "[\\p{So}\\p{Cs}\\p{Cn}]+", "")
    # Remove punctuation and numbers
    text <- gsub("[[:punct:]]|[[:digit:]]", "", text)
    # Remove extra whitespace
    text <- gsub("\\s+", " ", text)
    # Remove leading and trailing whitespace
    text <- str_trim(text)
    return(text)
  })
  
  # Remove duplicate tweets based on the 'clean_text' column
  tweets <- tweets %>%
    distinct(clean_text, .keep_all = TRUE)
  
  return(tweets)
}


#' Separate and Filter URLs
#' 
#' @param df A dataframe.
#' @return A dataframe where URLs are separated into different columns and non-photo URLs are filtered out.
#' @examples
#' \dontrun{
#'   # Example using your process_urls function here.
#' }
#' 
process_urls <- function(df) {
  # Takes a data frame as input, separates multiple URLs in the expanded_url 
  # column into different columns, and filters out the URLs that don't have 
  # the word 'photo' in them.
  #
  # Args:
  #    A df
  #
  # Returns: 
  #    A df with filtered URLs
  #
  # Validate input dataframe
  if (!inherits(df, "data.frame")) {
    stop("Input must be a data frame.")
  }
  
  # Check if 'expanded_url' column is present
  if (!"expanded_url" %in% colnames(df)) {
    stop("Missing required column: expanded_url.")
  }
  
  # Remove rows with empty cells in the 'expanded_url' column
  df <- df[!sapply(df$expanded_url, is.null), ]
  
  # Create a function to filter and separate URLs containing '/photo/' or 'instagram'
  filter_urls <- function(cell) {
    valid_urls <- cell[grepl("\\/photo\\/|instagram", cell, ignore.case = TRUE)]
    return(valid_urls)
  }
  
  # Apply the function to the 'expanded_url' column
  df$expanded_url <- lapply(df$expanded_url, filter_urls)
  
  # Find the maximum number of URLs in a single cell
  max_urls <- max(sapply(df$expanded_url, length))
  
  # Create new columns for the URLs
  for (i in 1:max_urls) {
    column_name <- paste0("url_", i)
    df[[column_name]] <- sapply(df$expanded_url, function(x) {
      if (length(x) >= i) {
        return(x[i])
      } else {
        return(NA)
      }
    })
  }
  
  # Remove rows with no URLs in all the new URL columns
  url_columns <- paste0("url_", 1:max_urls)
  df <- df[rowSums(is.na(df[, url_columns])) != max_urls, ]
  
  return(df)
}


#' Tokenize, Lemmatize, and Filter Text
#' 
#' @param df A dataframe.
#' @return A dataframe with tokenized, lemmatized, and filtered text in a new 'lemmatized_text' column.
#' @examples
#' \dontrun{
#'   # Example using your lemmatise function here.
#' }
#' 
lemmatise <- function(df) {
  # Define a list of words to exclude from lemmatization
  exclude_list <- c("stunning")
  
  # Tokenize the clean_text column
  tokenized_tweets <- df %>%
    unnest_tokens(output = "word", input = clean_text)
  
  # Lemmatize the words, excluding words in the exclude_list
  tokenized_tweets$lemma <- future_map(tokenized_tweets$word, function(word) {
    if (word %in% exclude_list) {
      return(word)
    } else {
      return(lemmatize_strings(word))
    }
  }) %>% unlist()
  
  # Remove short words (length < 3)
  tokenized_tweets <- tokenized_tweets %>%
    filter(nchar(lemma) >= 3)
  
  # Remove stop words
  tokenized_tweets <- tokenized_tweets %>%
    anti_join(stop_words, by = c("lemma" = "word"))
  
  # Group the tokenized tweets back together
  processed_tweets <- tokenized_tweets %>%
    group_by(author_id, text, lang, created_at,
             longitude, latitude, expanded_url, display_url,
             url_1, url_2, url_3, url_4, url_5, hashtags) %>%
    summarise(clean_text = paste(word, collapse = " "),
              lemmatized_text = paste(lemma, collapse = " "), .groups = "drop")
  
  return(processed_tweets)
}


#' Remove Rows with NA Values
#' 
#' @param df A dataframe.
#' @param column_name The name of the column to check for NA values.
#' @return A dataframe without rows containing NA values in the specified column.
#' @examples
#' \dontrun{
#'   # Example using your remove_na_rows function here.
#' }
#' 
remove_na_rows <- function(df, column_name) {
  df <- df[!is.na(df[[column_name]]), ]
  return(df)
}


#' Create Grouped Dataframes
#' 
#' @param df A dataframe.
#' @param column_names The column(s) to be used to filter the dataframe.
#' @param keyword_sets The keywords to filter the dataframe.
#' @return A list of dataframes, each dataframe is filtered based on a keyword.
#' @examples
#' \dontrun{
#'   # Example using your groups_df function here.
#' }
#' 
groups_df <- function(df, column_names, keyword_sets) {
  # Check if the input is a dataframe
  if (!is.data.frame(df)) {
    stop("The input df must be a data frame.")
  }
  
  # Check if the column_names is a character vector
  if (!is.character(column_names)) {
    stop("The column_names must be a character vector.")
  }
  
  # Check if the keyword_sets is a list
  if (!is.list(keyword_sets)) {
    stop("The keyword_sets must be a list.")
  }
  
  # Define the count_keywords function
  count_keywords <- function(text, keywords) {
    keyword_pattern <- paste0("(?i)\\b(", paste(keywords, collapse = "|"), ")\\b")
    return(sum(grepl(keyword_pattern, text, perl = TRUE)))
  }
  
  # Calculate the keyword counts for each keyword set
  keyword_counts <- lapply(keyword_sets, function(keywords) {
    rowSums(sapply(column_names, function(col) {
      sapply(df[[col]], count_keywords, keywords)
    }))
  })
  
  # Assign the group based on the maximum keyword count
  max_counts <- do.call(cbind, keyword_counts)
  max_indexes <- max.col(max_counts, ties.method = "first")
  df$group <- names(keyword_sets)[max_indexes]
  
  # Assign 'places' group for rows without a group
  df$group[apply(max_counts, 1, max) == 0] <- "places"
  
  # Create a list of dataframes for each group
  filtered_dfs <- setNames(lapply(c(names(keyword_sets), "places"), function(name) {
    df[df$group == name, ]
  }), c(names(keyword_sets), "places"))
  
  return(filtered_dfs)
}


#' Clean Text Column in a Dataframe
#' 
#' @param df A dataframe.
#' @param text_column The column containing the text to be cleaned.
#' @return The dataframe with the cleaned text column.
#' @examples
#' \dontrun{
#'   # Example using your clean_text_function here.
#' }
#' 
clean_text_function <- function(df, text_column = 'text') {
  df <- df %>%
    mutate(
      clean_text = str_replace_all(!!sym(text_column), "http[^ ]*", "") %>%  # Remove urls
        str_replace_all("#[^ ]*", "") %>%  # Remove hashtags
        str_replace_all("@[^ ]*", "") %>%  # Remove mentions
        str_replace_all("-", " ") %>%  # Replace hyphens with spaces
        str_replace_all("[^\\p{L}\\p{Zs}]", "") %>%  # Remove non-alphabetic, non-whitespace characters (emojis, Unicode symbols, punctuation, numbers)
        str_trim() %>%  # Trim whitespace from start and end of string
        str_squish() %>%  # Remove excess whitespace
        str_to_lower()  # Convert to lowercase
    )
  
  return(df)
}


#' Modify Dataframe
#' 
#' @param df A dataframe.
#' @return A modified dataframe with renamed and rearranged columns.
#' @examples
#' \dontrun{
#'   # Example using your modify_dataframe function here.
#' }
#' 
modify_dataframe <- function(df) {
  # Replace 'longitude' and 'latitude' with 'longitude.x' and 'latitude.x'
  df <- df %>%
    mutate(
      longitude = ifelse(is.na(longitude), longitude.x, longitude),
      latitude = ifelse(is.na(latitude), latitude.x, latitude)
    )
  
  # Remove the 'longitude.x' and 'latitude.x' columns
  df <- df %>%
    select(-c(longitude.x, latitude.x))
  
  # Move 'type' after 'lemmatized_text' 
  df <- df %>%
    relocate(group, .after = lemmatized_text) 
  
  return(df)
}