#' ---
#' Title: "Tweets preprocessing"
#' Author: EcoINN
#' Date: "February 2023"
#' Output: data frame
#' ---
#' 



preprocess <- function(df) {
  # This function creates a df with the necessary information for this study
  df_tw <- do.call(rbind,lapply(df, function (m)
    data.frame(text = df$text,
               lang = df$lang,
               geo = df$geo,
               date = df$created_at)))
  # Select unique rows based on the text column only
  df_u <- df_tw %>% distinct(text, .keep_all=TRUE)
  
  return(df)
}


df_clean <- function(df) {
  # Extract the URLs from the text column
  df <- df %>% 
    mutate(url = stringr::str_extract(text, "(https?://t\\.co/[^[:space:]]+)")) %>% 
    mutate(text = gsub("(https?://t\\.co/[^[:space:]]+)", "", text))
  
  # Filter the df based on the value of the 'lang' column
  df_ft <- df %>% 
    filter(lang == "en")
  
  # Replace '&amp;' for 'and'
  df_ft$text <- gsub("&amp;", "and", df_ft$text)
  
  return(df_ft)
}

