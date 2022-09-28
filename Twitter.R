#' ---
#' title: "Twitter spatial analysis"
#' author: EcoINN
#' date: "September 2022"
#' output: 
#' ---




#### preparation of the script ####


# set folder
setwd("C:/Ecostack/Selina/output")

# load the required packages

if( !("rjson" %in% installed.packages()[,1]))
  install.packages("rjson")
library(rjson)

if( !("dplyr" %in% installed.packages()[,1]))
  install.packages("dplyr")
library(dplyr)

if( !("httr" %in% installed.packages()[,1]))
  install.packages("httr")
library(httr)

if( !("rtweet" %in% installed.packages()[,1]))
  install.packages("rtweet")
library(rtweet)



#### connect to twitter ####

# connect to twitter
auth_setup_default()

#### search tweets ####

# create a df 
# this is just a test 
df <- search_tweets2(c("#Malta", "#nature"),
                    since_id="2022-09-21", max_id="2022-09-26",
                    n=10, include_rts=FALSE)

