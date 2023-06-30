#' Tweet Processing and Filtering
#' 
#' Set of functions to interactively filter rows based on user-selected words from a specified column of a dataframe.
#' 
#' @author EcoINN
#' @date "June 2023"
#' @return data.frame A filtered dataframe



#' Filter Data Frame
#' 
#' Function to interactively filter rows based on user-selected words from a specified column of a dataframe.
#' 
#' @param df A dataframe containing the data
#' @return A filtered dataframe
#' @details The user selects a column and the function displays the most frequent words.
#' The user can then specify which words to use to filter the dataframe.
#' @seealso remove_rows
#' @examples 
#' data <- data.frame(text = c("Hello world", "This is a test", "Another test"))
#' filter_dataframe(data)
filter_dataframe <- function(df) {
  repeat_process <- TRUE
  
  remove_rows <- function(df, words, column_name) {
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
    
    # Ask the user if they want to display words from the beginning or the end of the list
    cat("Do you want to display words from the beginning or the end of the list? (beginning/end): ")
    display_from <- tolower(readLines(n = 1))
    
    if (display_from == "end") {
      start_idx <- max(1, unique_words_count - num_words + 1)
      end_idx <- unique_words_count
    } else {
      start_idx <- 1
      end_idx <- min(num_words, unique_words_count)
    }
    
    # Create a table with the top common words
    all_words_table <- df %>%
      unnest_tokens(word, !!sym(column_name)) %>%
      count(word, sort = TRUE)
    
    # Get the words based on the user's choice
    top_words_table <- all_words_table[start_idx:end_idx, ]
    
    # Display the table
    print(head(top_words_table, n = num_words))
    
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


#' Filter Data Frame by Word Pairs
#' 
#' Function to interactively filter rows containing specific word pairs in a given column.
#' 
#' @param df A dataframe containing the data
#' @param threshold Minimum number of occurrences for a word pair to be considered
#' @param column The name of the column containing the text to filter
#' @return A filtered dataframe
#' @details The user selects a column and the function displays the most frequent word pairs.
#' The user can then specify which word pairs to use to filter the dataframe.
#' @seealso word_pairs remove_rows_by_word_pairs
#' @examples
#' data <- data.frame(text = c("Hello world", "This is a test", "Another test"))
#' filter_by_word_pairs(data, threshold = 2, column = "text")
filter_by_word_pairs <- function(df, threshold, column) {
  # Find word pairs and their frequencies
  find_word_pairs <- function(df, threshold, column) {
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
  
  # Remove rows containing the selected word pairs
  remove_rows_by_word_pairs <- function(df, pairs_to_remove, column) {
    # Create regex patterns for each word pair to remove
    patterns_to_remove <- lapply(pairs_to_remove, function(pair) {
      paste0("\\b", pair[1], "\\b.*\\b", pair[2], "\\b")
    })
    
    # Create a logical vector indicating which rows contain the word pairs to remove
    rows_to_remove <- Reduce(`|`, lapply(patterns_to_remove, function(pattern) {
      grepl(pattern, df[[column]], ignore.case = TRUE)
    }))
    
    # Remove the rows from the dataframe
    df <- df[!rows_to_remove, ]
    
    return(df)
  }
  
  # Check if the provided column exists in the dataframe
  if (!column %in% colnames(df)) {
    stop("The provided column does not exist in the dataframe.")
  }
  
  cat("Finding word pairs...\n")
  repeat_process <- TRUE
  removed_pairs <- c()
  
  while (repeat_process) {
    # Find word pairs
    word_pairs <- find_word_pairs(df, threshold, column)
    
    # Remove already removed word pairs from the list
    word_pairs <- word_pairs %>% filter(!(paste(word, next_word) %in% removed_pairs))
    
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
    
    # Validate user input for the number of word pairs to display
    if (num_pairs <= 0 || num_pairs > max_pairs) {
      cat("Invalid input. Please enter a number between 1 and the maximum number of word pairs available.\n")
      next
    }
    
    # Ask the user if they want to display word pairs from the beginning or the end of the list
    cat("Do you want to display word pairs from the beginning or the end of the list? (beginning/end): ")
    display_from <- tolower(readLines(n = 1))
    
    # Validate user input for the direction of word pairs
    if (!(display_from %in% c("beginning", "end"))) {
      cat("Invalid input. Please enter either 'beginning' or 'end'.\n")
      next
    }
    
    if (display_from == "end") {
      start_idx <- max(1, nrow(word_pairs) - num_pairs + 1)
      end_idx <- nrow(word_pairs)
    } else {
      start_idx <- 1
      end_idx <- min(num_pairs, nrow(word_pairs))
    }
    
    # Print the word pairs with 1-based indices
    displayed_word_pairs <- word_pairs[start_idx:end_idx, ]
    cat("\nIndex\tWord\tNext Word\tFrequency\n")
    for (i in 1:nrow(displayed_word_pairs)) {
      cat(i, displayed_word_pairs[i, "word"], displayed_word_pairs[i, "next_word"], displayed_word_pairs[i, "n"], sep = "\t", end = "\n")
    }
    
    # Ask the user for the word pairs to remove (by index)
    cat("Enter the index (indices) of the word pair(s) to remove (separated by commas if more than one): ")
    indices_to_remove <- as.integer(strsplit(gsub(" ", "", readLines(n = 1)), ",")[[1]])
    
    # Validate user input for the indices of word pairs to remove
    if (any(indices_to_remove <= 0 | indices_to_remove > nrow(displayed_word_pairs))) {
      cat("Invalid input. Please enter valid indices corresponding to the displayed word pairs.\n")
      next
    }
    
    if (!all(is.na(indices_to_remove))) {
      pairs_to_remove <- displayed_word_pairs[indices_to_remove, c("word", "next_word")]
      
      for (i in 1:nrow(pairs_to_remove)) {
        pair <- pairs_to_remove[i,]
        df <- df[!(grepl(paste0("\\b", pair[1], "\\b"), df[[column]]) &
                     grepl(paste0("\\b", pair[2], "\\b"), df[[column]])), ]
      }
      
      removed_pairs <- c(removed_pairs, apply(pairs_to_remove, 1, paste, collapse = " "))
      cat("Removed", length(indices_to_remove), "word pair(s).\n")
    }
    
    # Ask the user if they want to repeat the process
    cat("Do you want to repeat the filtering process? (yes/no): ")
    user_input <- tolower(readLines(n = 1))
    
    # Validate user input for repeating the process
    if (!(user_input %in% c("yes", "no"))) {
      cat("Invalid input. Please enter either 'yes' or 'no'.\n")
      next
    }
    
    if (user_input != "yes") {
      repeat_process <- FALSE
    }
  }
  return(df)
}


#' Filter Data Frame by Word Triples
#' 
#' This function interactively filters rows containing specific word triples in a given column.
#' 
#' @param df A dataframe containing the data.
#' @param threshold Minimum number of occurrences for a word triple to be considered.
#' @param column The name of the column containing the text to filter.
#' @return A dataframe that's been filtered based on user-selected word triples.
#' @details The function creates word triples from the specified column, then prompts the user to select
#' which triples to remove from the dataframe.
#' @seealso find_word_triples remove_rows_by_word_triples
#' @examples 
#' # For examples, suppose df is your data frame, threshold is 3, and column is "text".
#' filter_by_word_triples(df, 3, "text")
filter_by_word_triples <- function(df, threshold, column) {
  
  find_word_triples <- function(df, threshold, column) {
    # Create a dataframe of word triples and their frequencies
    
    # Check if input is a dataframe or tibble
    if (!inherits(df, "data.frame") && !inherits(df, "tbl")) {
      stop("Input must be a dataframe or tibble.")
    }
    
    # Add a unique identifier column to the dataframe
    df <- df %>%
      mutate(id = row_number())
    
    # Tokenize the input column
    tokenized_df <- df %>%
      unnest_tokens(output = "word", input = !!sym(column), token = "words")
    
    # Find word triples in the tokenized dataframe
    word_triples <- tokenized_df %>%
      group_by(id) %>%
      mutate(prev_word = lag(word),
             next_word = lead(word)) %>%
      filter(!is.na(prev_word) & !is.na(next_word)) %>%
      count(prev_word, word, next_word) %>%
      filter(n >= threshold) %>%
      arrange(desc(n))
    
    # Ensure that the output is a tibble
    word_triples <- as_tibble(word_triples)
    
    return(word_triples)
  }
  
  remove_rows_by_word_triples <- function(df, triples_to_remove, column) {
    # Check if input is a dataframe or tibble
    if (!inherits(df, "data.frame") && !inherits(df, "tbl")) {
      stop("Input must be a dataframe or tibble.")
    }
    
    # Loop through each word triple and remove rows containing it
    for (triple in triples_to_remove) {
      words <- unlist(strsplit(triple, " "))
      
      pattern_complete <- paste0("\\b", words[1], "\\b \\b", words[2], "\\b \\b", words[3], "\\b")
      pattern_first_second <- paste0("\\b", words[1], "\\b \\b", words[2], "\\b")
      pattern_second_third <- paste0("\\b", words[2], "\\b \\b", words[3], "\\b")
      
      rows_to_remove_complete <- grepl(pattern_complete, df[[column]], ignore.case = TRUE)
      rows_to_remove_first_second <- grepl(pattern_first_second, df[[column]], ignore.case = TRUE)
      rows_to_remove_second_third <- grepl(pattern_second_third, df[[column]], ignore.case = TRUE)
      
      rows_to_remove <- rows_to_remove_complete | rows_to_remove_first_second | rows_to_remove_second_third
      df <- df[!rows_to_remove, ]
    }
    
    # Ensure that the output is a dataframe or tibble
    if (inherits(df, "data.frame")) {
      df <- as.data.frame(df)
    } else if (inherits(df, "tbl")) {
      df <- as_tibble(df)
    }
    
    # Return the modified dataframe
    return(df)
  }
  
  cat("Finding word triples...\n")
  
  repeat_process <- TRUE
  removed_triples <- c()
  
  while (repeat_process) {
    # Find word triples
    word_triples <- find_word_triples(df, threshold, column)
    word_triples <- word_triples %>% filter(!paste(prev_word, word, next_word) %in% removed_triples) # Update the list of word triples, removing the ones already removed
    
    # Check if the dataframe is empty
    if (nrow(df) == 0) {
      cat("All rows have been removed. The resulting dataframe is empty.\n")
      return(df)
    }
    
    # Inform the user of the maximum number of word triples available
    max_triples <- nrow(word_triples)
    cat("The maximum number of word triples available to display is:", max_triples, "\n")
    
    # Ask the user for the number of word triples to display
    cat("Enter the number of word triples to display: ")
    num_triples <- as.integer(readLines(n = 1))
    
    # Ask the user if they want to display word triples from the beginning or the end of the list
    cat("Do you want to display word triples from the beginning or the end of the list? (beginning/end): ")
    display_from <- tolower(readLines(n = 1))
    
    if (display_from == "end") {
      start_idx <- max(1, nrow(word_triples) - num_triples + 1)
      end_idx <- nrow(word_triples)
    } else {
      start_idx <- 1
      end_idx <- min(num_triples, nrow(word_triples))    
    }
    
    # Display the word triples
    displayed_triples <- word_triples[start_idx:end_idx, ]
    print(displayed_triples, n = num_triples)
    
    # Ask the user for the indices of the word triples to remove
    cat("Enter the index/indices of the word triple(s) to remove (separated by commas if more than one): ")
    indices_to_remove <- as.integer(strsplit(gsub(" ", "", readLines(n = 1)), ",")[[1]])
    
    if (!all(is.na(indices_to_remove))) {
      # Get the word triples to remove
      triples_to_remove <- displayed_triples[indices_to_remove, c("prev_word", "word", "next_word")]
      triples_to_remove <- apply(triples_to_remove, 1, paste, collapse = " ")
      
      # Add the removed triples to the removed_triples vector
      removed_triples <- c(removed_triples, triples_to_remove)
      
      # Remove rows containing the selected word triples
      df <- remove_rows_by_word_triples(df, triples_to_remove, column)
      
      # Inform the user of the number of rows removed
      cat("Removed", length(indices_to_remove), "word triple(s).\n")
    }
    
    # Ask the user if they want to repeat the filtering process
    cat("Do you want to repeat the filtering process? (yes/no): ")
    user_input <- tolower(readLines(n = 1))
    if (user_input == "no") {
      repeat_process <- FALSE
    }
  }
  return(df)
}


#' Filter Data Frame by List
#' 
#' This function filters a dataframe based on a list of places. The function checks for matches in the 'hashtags', 
#' 'clean_text', and 'lemmatized_text' columns.
#' 
#' @param df A dataframe containing the data.
#' @param list_data A dataframe of places, latitude, and longitude.
#' @return A dataframe that's been filtered by the specified places, with new columns for the place, latitude, and longitude.
#' @details This function is case-insensitive, and replaces hyphens in place names with spaces for matching.
#' @seealso place_in_text
#' @examples 
#' # For examples, suppose df is your data frame, and list_data is your list of places with latitude and longitude.
#' filter_df_by_list(df, list_data)
filter_df_by_list <- function(df, list_data) {
  # Convert all place names and df text to lowercase for case-insensitive matching
  # Also replace hyphens in place names with spaces
  list_data$place <- tolower(str_replace_all(list_data$place, "-", " "))
  list_data$place <- tolower(list_data$place)
  df$hashtags <- tolower(as.character(df$hashtags))
  df$clean_text <- tolower(as.character(df$clean_text))
  df$lemmatized_text <- tolower(as.character(df$lemmatized_text))
  
  # Function to check if any place is in a text string
  place_in_text <- function(text, places) {
    # Check if the text is NA
    if (is.na(text)) {
      return(NA_character_)
    }
    
    # Tokenize the text into sentences
    sentences <- tokenize_sentences(text)
    
    # Initialize an empty vector to store matched places
    matched <- c()
    
    # Loop over the sentences in reverse order
    for (i in rev(seq_along(sentences))) {
      sentence <- sentences[[i]]
      
      # Loop over the places
      for (place in places) {
        # Check if the place name contains non-alphabetic characters
        if (any(!str_detect(place, "\\p{L}"))) {
          # If it does, use fixed = TRUE in str_detect
          if (str_detect(sentence, fixed(place))) {
            matched <- c(matched, place)
          }
        } else {
          # If it doesn't, use word boundary markers in str_detect
          if (str_detect(sentence, paste0("\\b", place, "\\b"))) {
            matched <- c(matched, place)
          }
        }
      }
    }
    
    # If any places are matched, return all of them
    if (length(matched) > 0) {
      return(paste(matched, collapse = ",")) 
    } else {
      return(NA_character_)
    }
  }
  
  # Apply the place_in_text function to each row and each specified column in df
  df <- df %>%
    mutate(
      matched_place = mapply(place_in_text, hashtags, MoreArgs = list(places = list_data$place)),
      matched_place = ifelse(is.na(matched_place), mapply(place_in_text, clean_text, MoreArgs = list(places = list_data$place)), matched_place),
      matched_place = ifelse(is.na(matched_place), mapply(place_in_text, lemmatized_text, MoreArgs = list(places = list_data$place)), matched_place)
    ) %>%
    filter(!is.na(matched_place)) # filter rows where a place was matched
  
  # Separate matched places into multiple columns
  df <- tidyr::separate_rows(df, matched_place, sep = ",")
  
  # Join the latitude and longitude
  df <- left_join(df, list_data, by = c("matched_place" = "place"))
  
  return(df)
}