#' ---
#' Title: "Tweets processing"
#' Author: EcoINN
#' Date: "February 2023"
#' Output: data frame
#' ---
#' 


process_tweets <- function(tweets_df) {
  # Creates a df with the necessary information for this study
  #
  # Args:
  #    tweets_df: A df containing the tweets 
  #
  # Returns: 
  #    A df with only unique tweets 
  #
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


lemmatize_and_filter <- function(df) {
  # Function to tokenize, lemmatize, and filter the text in the clean_text column of a dataframe
  #
  # Args:
  #   df: A dataframe containing a 'clean_text' column with the text to be processed
  #
  # Returns:
  #   A dataframe with the processed text in a new 'lemmatized_text' column
  #
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


remove_na_rows <- function(df, column_name) {
  # A function to remove rows containing NA values in a specified column
  #
  # Args:
  #   df: A dataframe containing the data
  #   column_name: The name of the column to check for NA values
  #
  # Returns:
  #   A dataframe without rows containing NA values in the specified column
  
  df <- df[!is.na(df[[column_name]]), ]
  return(df)
}


groups_df <- function(df, column_names, keyword_sets) {
  # This function creates a list of dataframes, 
  # filtered based on the provided keywords.
  #
  # Args:
  #    A df, the columns to be used to filter the df, and the keywords
  #
  # Returns: 
  #    A list of different dfs
  #
  
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


filter_by_word_pairs <- function(df, threshold, column) {
  # A function to interactively filter rows containing specific word pairs
  # in a given column.
  #
  # Args:
  #   df: A dataframe containing the data
  #   threshold: Minimum number of occurrences for a word pair to be considered
  #   column: The name of the column containing the text to filter
  #
  # Returns:
  #   A filtered dataframe
  
  find_word_pairs <- function(df, threshold, column) {
    # Create a dataframe of word pairs and their frequencies
    
    # Tokenize the input column
    tokenized_df <- df %>%
      unnest_tokens(output = "word", input = !!sym(column), token = "words")
    
    # Find word pairs in the tokenized dataframe
    word_pairs <- tokenized_df %>%
      mutate(next_word = lead(word)) %>%
      count(word, next_word) %>%
      filter(n >= threshold) %>%
      arrange(desc(n))
    
    return(word_pairs)
  }
  
  remove_rows_by_word_pairs <- function(df, pairs_to_remove, column) {
    # Create a regex pattern with the word pairs to remove
    pattern <- paste0("\\b(", paste(pairs_to_remove, collapse = "|"), ")\\b")
    
    # Create a logical vector indicating which rows contain the word pairs to remove
    rows_to_remove <- grepl(pattern, df[[column]], ignore.case = TRUE)
    
    # Remove the rows from the dataframe
    df <- df[!rows_to_remove, ]
    
    # Return the modified dataframe
    return(df)
  }
  
  cat("Finding word pairs...\n")
  
  repeat_process <- TRUE
  
  while (repeat_process) {
    # Find word pairs
    word_pairs <- find_word_pairs(df, threshold, column)
    
    # Check if the dataframe is empty
    if (nrow(df) == 0) {
      cat("All rows have been removed. The resulting dataframe is empty.\n")
      return(df)
    }
    
    # Inform the user of the maximum number of word pairs available
    max_pairs <- nrow(word_pairs)
    cat("The maximum number of word pairs available to display is:", max_pairs, "\n")
    
    # Ask the user for the number of word pairs to display
    cat("Enter the number of word pairs to display: ")
    num_pairs <- as.integer(readLines(n = 1))
    
    # Ask the user if they want to display word pairs from the beginning or the end of the list
    cat("Do you want to display word pairs from the beginning or the end of the list? (beginning/end): ")
    display_from <- tolower(readLines(n = 1))
    
    if (display_from == "end") {
      start_idx <- max(1, nrow(word_pairs) - num_pairs + 1)
      end_idx <- nrow(word_pairs)
    } else {
      start_idx <- 1
      end_idx <- min(num_pairs, nrow(word_pairs))
    }
    
    # Print the word pairs
    displayed_word_pairs <- word_pairs[start_idx:end_idx, ]
    print(displayed_word_pairs, n = num_pairs)
    
    # Ask the user for the word pairs to remove (by index)
    cat("Enter the index (indices) of the word pair(s) to remove (separated by commas if more than one): ")
    pair_indices <- scan("", what = integer(), sep = ",")
    pairs_to_remove <- displayed_word_pairs[pair_indices, c("word", "next_word")]
    pairs_to_remove <- paste(pairs_to_remove$word, pairs_to_remove$next_word)
    
    # Remove rows containing the selected word pairs
    df <- remove_rows_by_word_pairs(df, pairs_to_remove, column)
    
    # Ask the user if they want to repeat the process
    cat("Do you want to repeat the filtering process? (yes/no): ")
    user_input <- tolower(readLines(n = 1))
    
    if (user_input != "yes") {
      repeat_process <- FALSE
    }
  }
  
  return(df)
}


filter_dataframe <- function(df) {
  # A function to interactively filter rows based on user-selected words
  # from a specified column of a dataframe.
  #
  # Args:
  #   df: A dataframe containing the data
  #
  # Returns:
  #   A filtered dataframe
  
  repeat_process <- TRUE
  
  remove_rows <- function(df, words, column_name) {
    # A function to remove rows containing certain words in a dataframe
    #
    # Args:
    #   df: A dataframe containing the data
    #   words: The words to find within the text
    #   column_name: The name of the column to search for the words
    #
    # Returns: 
    #   A modified dataframe
    
    # Check if the words parameter is empty
    if (length(words) == 0) {
      # If words is empty, return the original dataframe without modifications
      return(df)
    }
    
    # Create a logical vector indicating which rows contain the words to remove
    rows_to_remove <- grepl(paste(words, collapse = "|"), 
                            df[[column_name]], 
                            ignore.case = TRUE)
    
    # Remove the rows from the dataframe
    df <- df[!rows_to_remove, ]
    
    # Return the modified dataframe
    return(df)
  }
  
  while (repeat_process) {
    cat("Processing dataframe...\n")
    
    # Ask the user for the column to process
    cat("Enter the column name to process: ")
    column_name <- readLines(n = 1)
    
    # Check if the column_name exists in the dataframe
    if (!(column_name %in% colnames(df))) {
      cat("Column not found in the dataframe. Please check the column name and try again.\n")
      next
    }
    
    # Calculate the total number of unique words in the specified column
    unique_words_count <- df %>%
      unnest_tokens(word, !!sym(column_name)) %>%
      count(word, sort = TRUE) %>%
      nrow()
    
    cat("The maximum number of unique words in the selected column is:", unique_words_count, "\n")
    
    # Ask the user for the number of top words to display
    cat("Enter the number of top words to display: ")
    num_words <- as.integer(readLines(n = 1))
    
    # Check if num_words is a valid input
    if (is.na(num_words) || num_words < 1 || num_words > unique_words_count) {
      cat("Invalid input. Please enter a positive integer within the range of unique words.\n")
      next
    }
    
    # Create a table with the top common words
    top_words_table <- df %>%
      unnest_tokens(word, !!sym(column_name)) %>%
      count(word, sort = TRUE) %>%
      head(num_words)
    
    # Display the table
    print(top_words_table, n = num_words)
    
    # Ask the user for the indices of the words to remove
    cat("Enter the index (indices) of the word(s) to remove (separated by commas if more than one): ")
    word_indices <- scan("", what = integer(), sep = ",")
    words_to_remove <- top_words_table$word[word_indices]
    
    # Remove the rows containing the words
    df <- remove_rows(df, words_to_remove, column_name)
    
    # Ask the user if they want to repeat the process
    cat("Do you want to repeat the filtering process? (yes/no): ")
    user_input <- tolower(readLines(n = 1))
    
    if (user_input != "yes") {
      repeat_process <- FALSE
    }
  }
  
  return(df)
}
    

