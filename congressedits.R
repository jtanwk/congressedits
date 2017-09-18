# Congressional Edits on Twitter
# Scraping the tweets of https://twitter.com/congressedits

# Questions
# 1. What pages are edited most frequently from congress locations?
# 2. What pages are edited most by house/senate locations?
# 3. When/what time do page edits occur most frequently?
# 4. What categories of pages are edited most frequently from congress locations?
# 5. What categories of pages are edited most frequently from house/senate locations?
# 6. What were the most fav'd/RT'd congressedit tweets?

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

# REMOVE BEFORE PUBLISHING
# Generated at https://apps.twitter.com/
consumer_key <- "RYs9y1MmwQdgqbkXqd2HyMjod"
consumer_secret <- "ObOTO18T5BjK15D5flPf5GLql2n5jbqIEcleQEuoYsp8NzTjom"
access_token <- "2680028551-P4TgiMCsRF9qgWTrch9GNeicoRST2g6h2U0Dm4U"
access_token_secret <- "0SLYy8cDpP56zVDgA1yHsooaGIhNpyCPAyTFOCzNzbWHa"

setup_twitter_oauth(consumer_key, 
                    consumer_secret, 
                    access_token, 
                    access_token_secret)

####################
##### Scraping #####
####################

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
   
####################  
##### Analysis #####
####################
  
  ## For easy loading 
  
  load("clean_congressedits_tweets.RData")
  load("congress_categories.RData")
  
  ## 1. What pages are edited most frequently from congress locations?
  
  clean_congress_tweets %>%
    count(subject, sort = TRUE) %>%
    top_n(10) %>%
    mutate(subject = str_wrap(subject, width = 20)) %>%
    ggplot(aes(x = reorder(subject, n), y = n)) +
    geom_col() +
    coord_flip() +
    theme_minimal() +
    labs(x = NULL,
         y = "Number of Edits",
         title = "The 10 Most Frequent Wikipedia Page Edits from Congress",
         subtitle = "Using 1,430 tweets scraped from @congressedits between Aug 17, 2014 and Sep 15, 2017.")
  
  ggsave("Images/tweet_bypage.png", width = 8, height = 6)
  
  ## 2. What pages are edited most by house/senate locations?
  
  clean_congress_tweets %>%
    count(source)
  
  ## 995 edits from the House, versus 435 edits from the Senate
  
  clean_congress_tweets %>%
    count(source, subject, sort = TRUE) %>%
    top_n(10) %>%
    mutate(subject = str_wrap(subject, width = 20)) %>%
    ggplot(aes(x = reorder(subject, n), y = n, fill = source)) +
    geom_col(show.legend = FALSE) +
    coord_flip() +
    facet_wrap(~source, nrow = 1, scales = "free") +
    theme_minimal() +
    labs(x = NULL,
         y = "Number of Edits",
         title = "The 10 Most Frequent Wikipedia Page Edits from House vs. Senate",
         subtitle = "Using 1,430 tweets scraped from @congressedits between Aug 17, 2014 and Sep 15, 2017.")
  
  ggsave("Images/tweet_bysource.png", width = 8, height = 6)
  
  
  ## 3. When/what time do page edits occur most frequently?
  
  clean_congress_tweets %>%
    mutate(created = as.Date(created)) %>%
    count(created) %>%
    pad() %>%
    mutate(n = ifelse(is.na(n), 0, n)) %>%
    ggplot(aes(x = created, y = n)) +
    geom_line(color = "grey50") +
    theme_minimal() +
    labs(x = "Date",
         y = "Number of Edits",
         title = "Number of Wikipedia Edits from Congress by Date",
         subtitle = "Using 1,430 tweets scraped from @congressedits between Aug 17, 2014 and Sep 15, 2017.")
  
  ggsave("Images/tweet_bydate.png", width = 8, height = 6)
  
  ## Followup - does this change when congress is in session? 
  
  closed <- data.frame(xmin = as.Date(c("2014-08-01", "2015-08-01", "2016-08-01", "2016-10-01", "2017-08-01")),
                       xmax = as.Date(c("2014-08-31", "2015-08-31", "2016-08-31", "2016-10-31", "2017-08-31")),
                       ymin = -Inf,
                       ymax = Inf)
  
  clean_congress_tweets %>%
    filter(source == "US House of Representatives") %>%
    mutate(created = as.Date(created)) %>%
    count(created) %>%
    pad() %>%
    mutate(n = ifelse(is.na(n), 0, n)) %>%
    ggplot(aes(x = created, y = n)) +
    geom_line(color = "grey50") +
    geom_rect(data = closed,
              inherit.aes = FALSE,
              aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
              fill = "orange",
              alpha = 0.3) +
    theme_minimal() +
    labs(x = "Date",
         y = "Number of Edits",
         title = "Number of Wikipedia Edits from the House of Representatives by Date",
         subtitle = "Using 1,430 tweets scraped from @congressedits between Aug 17, 2014 and Sep 15, 2017.")
  
  ## What was the spike in 2015?
  clean_congress_tweets %>%
    mutate(created = as.Date(created)) %>%
    count(created, sort = TRUE) %>%
    top_n(5)
  
  ## What pages were edited?
  clean_congress_tweets %>%
    mutate(created = as.Date(created)) %>%
    filter(created == "2015-06-22") %>%
    count(subject, sort = TRUE) %>%
    print(n = 29)
  
  ## Check times for June 22, 2015 edits
  
  clean_congress_tweets %>%
    mutate(created = as.POSIXct(created)) %>%
    mutate(day = as.Date(created)) %>%
    mutate(time = strftime(created, format = "%Y-%m-%d %H:%M:00")) %>%
    filter(day == "2015-06-22") %>%
    count(time) %>%
    mutate(time = as.POSIXct(time)) %>%
    pad() %>%
    ggplot(aes(x = time, y = n)) +
    geom_col()
  
  ## What about time of day? 
  
  clean_congress_tweets %>%
    select(id, created) %>%
    mutate(created = as.POSIXct(created)) %>%
    mutate(hour = format(created, "%H")) %>%
    count(hour) %>%
    ggplot(aes(x = hour, y = n)) +
    geom_col() +
    theme_minimal() +
    labs(x = "Hour in Day",
         y = "Count",
         title = "Number of Wikipedia Edits from Congress by Time of Day",
         subtitle = "Using 1,430 tweets scraped from @congressedits between Aug 17, 2014 and Sep 15, 2017.")
  
  ggsave("Images/tweet_times.png", width = 8, height = 6)
  
  ## Mostly around 6-7pm in the day, but starting at 1pm and ending at 9pm.
  
  ## Disaggregating by day + creating punchcard plot
  
  clean_congress_tweets %>%
    mutate(created = as.POSIXct(created)) %>%
    mutate(day = factor(weekdays(created),levels = c("Sunday", "Saturday", "Friday", "Thursday", "Wednesday", "Tuesday", "Monday"))) %>%
    mutate(created = format(created, "%Y-%m-%d %H:00:00")) %>%
    count(day, created) %>%
    mutate(created = as.POSIXct(created)) %>%
    pad() %>%                                           ## padr::pad inserts NA rows for missing date/times
    mutate(created = format(created, "%H")) %>% 
    mutate(n = ifelse(is.na(n), 0, n)) %>%
    group_by(day, created) %>%
    summarise(edits = ifelse(sum(n) == 0, NA, sum(n))) %>%
    ggplot(aes(x = created, y = day, size = edits)) +
    geom_point() +
    theme_minimal() +
    labs(x = "Hour in Day",
         y = "Number of Edits",
         title = "Number of Wikipedia Edits from Congress by Day/Time",
         subtitle = "Using 1,430 tweets scraped from @congressedits between Aug 17, 2014 and Sep 15, 2017.")
    
  ggsave("Images/tweet_daytimes.png", width = 8, height = 6)
  
  # Followup - what pages were edited during the true graveyard shift: after 3am and before 8am? 
  
  latenight <- clean_congress_tweets %>%
    select(subject, created, source) %>%
    mutate(created = as.POSIXct(created)) %>%
    mutate(hour = as.numeric(format(created, "%H"))) %>%
    filter(hour <= 8) %>%
    count(subject, sort = TRUE) 
  
  clean_congress_tweets %>%
    filter(subject %in% latenight$subject) %>%
    mutate(created = as.POSIXct(created)) %>%
    mutate(hour = as.numeric(format(created, "%H"))) %>%
    count(subject, hour) %>%
    ggplot(aes(x = hour, y = n)) +
    geom_col() +
    annotate(geom = "rect",
             xmin = 0, xmax = 8, ymin = -Inf, ymax = Inf,
             fill = "blue",
             alpha = 0.2) +
    xlim(-0.5, 23.5) +
    facet_wrap(~subject) +
    theme_minimal() +
    labs(x = "Hour in Day",
         y = "Number of Edits",
         title = "Late Night Wikipedia Page Edits from Congress",
         subtitle = "Using only pages with edits between 12 midnight and 8 am, represented by the shaded area.")
  
  ggsave("Images/tweets_midnightedits.png", width = 8, height = 6)
  
  
  ## 4. What categories of pages are edited most frequently from congress locations?
  
  congress_categories %>%
    filter(is.na(categories) == FALSE) %>%
    count(categories, sort = TRUE) %>%
    top_n(10) %>%
    mutate(categories = str_wrap(categories, width = 30)) %>%     ## wraps labels for long category names
    ggplot(aes(x = reorder(categories, n), y = n)) +
    geom_col() +
    coord_flip() +
    theme_minimal() +
    labs(x = NULL,
         y = "Number of Edits",
         title = "Categories of Most Frequent Wikipedia Edits from Congress",
         subtitle = "Using 1,430 tweets scraped from @congressedits between Aug 17, 2014 and Sep 15, 2017.")
  
  ggsave("Images/tweet_categories.png", width = 8, height = 6)
  
  ## Edit frequency: Politicians > Republicans > Democrats > Women > Female House members 
  
  congress_categories %>%
    filter(is.na(categories) == FALSE) %>%
    filter(source == "US House of Representatives") %>%
    count(categories, sort = TRUE) %>%
    top_n(10) %>%
    ggplot(aes(x = reorder(categories, n), y = n)) +
    geom_col() +
    coord_flip()
  
  congress_categories %>%
    filter(is.na(categories) == FALSE) %>%
    filter(source == "US Senate") %>%
    count(categories, sort = TRUE) %>%
    top_n(10) %>%
    ggplot(aes(x = reorder(categories, n), y = n)) +
    geom_col() +
    coord_flip()
  
  ## 5. What categories of pages are edited most frequently from house/senate locations? 
  
  ## Using log-odds ratio:
  
  congress_categories %>%
    filter(is.na(categories) == FALSE) %>%
    count(categories, source) %>%
    filter(sum(n) >= 5) %>%
    spread(source, n, fill = 0) %>%
    ungroup() %>%
    mutate_each(funs((. + 1) / sum(. + 1)), -categories) %>%
    mutate(logratio = log2(`US House of Representatives`/`US Senate`)) %>%
    mutate(posneg = factor(sign(logratio))) %>%
    arrange(desc(logratio)) %>%
    group_by(posneg) %>%
    top_n(10, abs(logratio)) %>%
    ggplot(aes(x = reorder(categories, logratio), y = logratio, fill = posneg)) +
    geom_col() +
    coord_flip() +
    theme_minimal() +
    scale_fill_discrete(name = "", labels = c("Senate", "House")) +
    labs(x = NULL,
         y = "House/Senate Log Odds Ratio")
  
  ## Alternative method: use TF-IDF on House/Senate "documents"
  
  congress_categories %>%
    count(source, categories, sort = TRUE) %>%
    ungroup() %>%
    bind_tf_idf(categories, source, n) %>%
    arrange(desc(tf_idf)) %>%
    group_by(source) %>%
    top_n(10) %>%
    mutate(categories = str_wrap(categories, width = 30)) %>%
    ggplot(aes(x = reorder(categories, tf_idf), y = tf_idf, fill = source)) +
    geom_col(show.legend = FALSE) +
    facet_wrap(~source, ncol = 2, scales = "free") +
    coord_flip() +
    theme_minimal() +
    labs(x = NULL, 
         y = NULL,
         title = "Wikipedia Edits by House vs. Senate",
         subtitle = "Using 1,430 tweets scraped from @congressedits between Aug 17, 2014 and Sep 15, 2017.")
  
  ## 6. What were the most fav'd/RT'd congressedit tweets?
  ## People tend to pick up on the weirder tweets.
    
  clean_congress_tweets %>%
    select(subject, favoriteCount, retweetCount, source) %>%
    mutate(labels = ifelse(favoriteCount > 1000 | retweetCount > 500, subject, "")) %>%
    ggplot(aes(x = retweetCount, y = favoriteCount)) +
    geom_point() +
    geom_text_repel(aes(label = labels)) +
    theme_minimal() +
    labs(x = "Retweets",
         y = "Favorites",
         title = "@CongressEdits Tweets by Twitter Engagement",
         subtitle = "Using 1,430 tweets scraped from @congressedits between Aug 17, 2014 and Sep 15, 2017.")
  
  ggsave("Images/tweets_engagement.png", width = 8, height = 6)
  
  ## 7. Can we tell which/how many users are doing all these edits?

  ## using rvest: function that takes edit url and returns user
  
  getEditUser <- function(url) {
    editurl <- read_html(url)
    edituser <- editurl %>%
      html_nodes("#mw-diff-ntitle2 bdi") %>%        ## node courtesy of SelectorGadget
      html_text()
    if (is_empty(edituser)) {
      edituser <- ""
    }
    return(edituser)
  }
  
  ## loop
  
  congress_edit_ids <- clean_congress_tweets %>%
    mutate(editor = "") %>%
    as_tibble()

  for (i in 1:length(congress_edit_ids$url)) {
    congress_edit_ids$editor[i] <- getEditUser(congress_edit_ids$url[i])
    print(paste(i, " of ", length(congress_edit_ids$url), sep = ""))
  }
  
  clean_congress_tweets %>%
    filter(subject == "Chaldean Neo-Aramaic") %>%
    select(created, url)
  
  ### Further QUestions
  # 1. Many separate edits are just one set of related edits done step by step. How would results change if we looked at edit-days?
  # 2. Are there fewer edits when the House or Senate is in session?
  