
# packages
library(dplyr)
library(twitterR)
library(rtweet)

# creating 
API_key = "" 
API_secret = ""
Access_token = ""
Access_secret = ""

# establish connection
setup_twitter_oauth(API_key,API_secret,Access_token,Access_secret)

# make a search and convert tweets into a data frame using twListToDF function
malta_raw <- searchTwitter("Malta", n=500, since="2021-01-01", until="2021-12-31")
malta_df <- malta_raw %>% 
  strip_retweets() %>%
  twListToDF()

malta_df <- malta_df %>%
  select(text,favoriteCount,created,truncated,longitude,latitude)

knitr::kable(malta_df[1:3,], format="markdown")