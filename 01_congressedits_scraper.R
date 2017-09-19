# Congressional Edits on Twitter
# Step 1: Scraping the tweets of https://twitter.com/congressedits
# Step 2: Using the Wikipedia page names from Step 1, scrape page categories from Wikipedia

# File 1 - Scraper

#################
##### Setup #####
#################

# wrangling packages
library(tidyverse)
library(tidytext)
library(stringr)

# twitter, wikipedia APIs and rvest for web scraping
library(twitteR)
library(WikipediR)
library(rvest)

# other packages helpful with visualization
library(padr)
library(ggrepel)

options(stringsAsFactors = FALSE)

# Twitter authentication step
# Generated at https://apps.twitter.com/
consumer_key <- "YOUR CONSUMER KEY HERE"
consumer_secret <- "YOUR CONSUMER SECRET HERE"
access_token <- "YOUR ACCESS TOKEN HERE"
access_token_secret <- "YOUR ACCESS TOKEN SECRET HERE"

setup_twitter_oauth(consumer_key, 
                    consumer_secret, 
                    access_token, 
                    access_token_secret)

############################
##### Scraping Twitter #####
############################

  ## Initial query
  congress_tweets <- userTimeline(user = "congressedits",
                                n = 3200)
  
  ## Process into tibble
  congress_tweets_df <- congress_tweets %>%
    map_df(as.data.frame) %>%
    tbl_df()

  ## Archive csv, save to .RData for easy future loading
  write.csv(congress_tweets_df, file = "CSV Data/raw_congressedits_tweets.csv")
  save(congress_tweets_df, file = "raw_congressedits_tweets.RData")

###################
##### Cleanup #####
###################

  ##### 1. Extract relevant information
  ## Great regex tutorial at https://regexone.com/
  
  load("raw_congressedits_tweets.RData")
  
  ## cleaning steps in order:
  ## 1. remove all non-bot tweets (i.e. tweets not containing edit reports)
  ## 2. extract page title of tweet into "subject"
  ## 3. extract url of edit into "url" for later scraping
  ## 4. extract edit source (house or senate IP address?)
  ## 5. remove all tweets where subject is an URL
  ## 6. remove all tweets with metadata edited - no categories to be scraped
  ## 7. convert all page titles with foreign characters to regular ASCII
  ## 8. remove all tweets with no subject
  clean_congress_tweets <- congress_tweets_df %>%
    filter(str_detect(text, "Wikipedia article edited anonymously")) %>%
    select(text, favoriteCount, created, id, retweetCount) %>%
    mutate(subject = str_replace_all(text, " Wikipedia article edited anonymously from US (House of Representatives|Senate) (http|https)://t.co/[A-Za-z\\d]+|&amp;", "")) %>%
    mutate(url = str_extract(text, "(http|https)://t.co/[A-Za-z\\d]+|&amp;")) %>%
    mutate(source = str_extract(text, "US (House of Representatives|Senate)")) %>%
    filter(str_detect(subject, "(http|https)://[A-Za-z\\d]+") == FALSE) %>%
    filter(str_detect(subject, "(File|Commons|User talk):") == FALSE) %>%
    mutate(subject = iconv(subject, to = "ASCII")) %>%
    filter(subject != "NA")
  
  ##### 2. Use WikipediR package to scrape category of page 
  ## Each Wikipedia page belongs to a list of categories. 
  ## Starting from tweet-level data, we want to get category-level data, and ideally see what broad themes emerge.
  
  ## function collects categories and pastes together in one long string  
  getCategories <- function(x) {
    wikiquery <- categories_in_page(language = "en",
                                project = "wikipedia",
                                pages = x)
    if (is.null(wikiquery$query$pages[[1]]$categories)) {
      return(NA)
    } else {
      table <- bind_rows(wikiquery$query$pages[[1]]$categories)
      return(paste(table$title, collapse = ";"))
    }
  }
  
  ## add empty "categories" column to tibble
  congress_categories <- clean_congress_tweets %>%
    mutate(categories = "")
  
  ## original function hit API limits, so we use a for loop to call the function separately for each row
  for (i in 1:length(congress_categories$subject)) {
    congress_categories$categories[i] <- getCategories(congress_categories$subject[i])
    print(i)
  }
  
  ## finally, convert long string of multiple categories into tidy tweet-category-level data
  congress_categories <- congress_categories %>%
    mutate(categories = strsplit(categories, ";")) %>%
    unnest(categories) %>%
    mutate(categories = str_replace_all(categories, "Category:", ""))
  
  ## save data to csv archive
  write.csv(clean_congress_tweets, file = "CSV Data/clean_congressedits_tweets.csv")
  write.csv(congress_categories, file = "CSV Data/congress_categories.csv")
  
  ## save .Rdata file for easy loading 
  save(clean_congress_tweets, file = "clean_congressedits_tweets.RData")
  save(congress_categories, file = "congress_categories.RData")
  