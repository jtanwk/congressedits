Wikipedia Edits from Congress
================
September 16, 2017

I first stumbled across the [@congressedits twitter account](https://twitter.com/congressedits) last year. If you haven't heard of it, it's a Twitter bot that detects edits made on Wikipedia pages that originate from certain IP addresses associated with Congress, and tweets out the changelog. Naturally, I saw an unmined dataset and pounced.

*Note: The underlying code is all open source and adaptable to different IP ranges. Definitely check the [Github repo](https://github.com/edsu/anon) out if you're interested in looking deeper. It's a great project.*

#### A Note on IP Addresses

It's also worth prefacing this entire article by saying that an IP address does not mean rock-solid attribution. Per the [CongressEdits Wikipedia page](https://en.wikipedia.org/wiki/CongressEdits):

> The changes could have been made by anyone using a computer on the U.S. Capitol complex's computer network, including both staff of U.S. elected representatives and senators as well as visitors such as journalists, constituents, tourists, and lobbyists.

My money's with PR teams and bored Congressional interns. But seriously: any insight from this project probably has very little external validity. It's just a fun way to practice.

Setup
-----

As usual, we complement base-R with the usual set of wrangling tools and other API packages.

``` r
# wrangling packages
library(tidyverse)
library(tidytext)
library(stringr)

# twitter and wikipedia APIs
library(twitteR)
library(WikipediR)

# other packages helpful with visualization
library(padr)
library(ggrepel)

options(stringsAsFactors = FALSE)
```

Getting the Tweets
------------------

#### Authentication

I followed [these instructions](https://stackoverflow.com/questions/1808855/getting-new-twitter-api-consumer-and-secret-keys) to obtain the authentication keys that the TwittR package needs.

``` r
# Generated at https://apps.twitter.com/
consumer_key <- "YOUR-CONSUMER-KEY-HERE"
consumer_secret <- "YOUR-CONSUMER-SECRET-HERE"
access_token <- "YOUR-ACCESS-TOKEN-HERE"
access_token_secret <- "YOUR-ACCESS-TOKEN-SECRET-HERE"

setup_twitter_oauth(consumer_key, 
                    consumer_secret, 
                    access_token, 
                    access_token_secret)
```

#### Scraping

After the authentication, it's a simple matter of scraping all available tweets from @congressedits using the `twittR::usertimeline` command.

``` r
  ## Initial query
  congress_tweets <- userTimeline(user = "congressedits",
                                n = 3200)
  
  ## Process into tibble
  congress_tweets_df <- congress_tweets %>%
    map_df(as.data.frame) %>%
    tbl_df()
```

#### Cleaning

I didn't know it when I started this project, but I was going to need a crash course in regex formatting. Fortunately, I found a short and simple tutorial [here](https://regexone.com/).

Cleaning steps below, in order:

1.  Remove all tweets that were not edit reports
2.  Extract Wikipedia page title to "subject"
3.  Extract Wikipedia page URL to "url"
4.  Extract source of edit to "source"
5.  Remove anomalous tweets where the subject is an URL
6.  Remove all tweets reporting edits to metadata pages (e.g. Commons or User Talk)
7.  Convert all page titles with foreign characters to ASCII
8.  Remove all tweets with no subject

``` r
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
```

Analysis
--------

#### 1. What Wikipedia pages are edited the most?

Tackling the obvious questions first.

``` r
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
         title = "The 10 Most Frequently Edited Wikipedia Pages at Congress",
         subtitle = "Using 1,430 tweets scraped from @congressedits between Aug 17, 2014 and Sep 15, 2017.")
```

    ## Selecting by n

![](congressedits_markdown_files/figure-markdown_github-ascii_identifiers/frequency-1.png)

No surprises here - all political figures or topics. Most likely PR teams doing their jobs. What about disaggregating this by edits from the House versus the Senate?

``` r
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
```

    ## Selecting by n

![](congressedits_markdown_files/figure-markdown_github-ascii_identifiers/housevssenate-1.png)

At this point, if there *weren't* people on Capitol Hill taking an intense interest in Chaldean Neo-Aramaic, I'd be concerned. Moving on.

#### 2. When are Wikipedia pages being edited?

Another simple question. Are there trends that we might be able to find in the dates or times of the tweets?

I'm assuming at this point that there's a fairly low discrepancy between the time of the wikipedia page edit and the time of the tweet. A brief manual comparison confirms this - the time listed on the tweet (`created`) matches the timestamp of the edit saved to Wikipedia.

Plotting first by calendar date:

``` r
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
```

    ## pad applied on the interval: day

![](congressedits_markdown_files/figure-markdown_github-ascii_identifiers/tweetsbydate-1.png)

No big cyclical trends that I can see. What's that big spike in 2015?

``` r
  clean_congress_tweets %>%
    mutate(created = as.Date(created)) %>%
    count(created, sort = TRUE) %>%
    top_n(5)
```

    ## Selecting by n

    ## # A tibble: 5 × 2
    ##      created     n
    ##       <date> <int>
    ## 1 2015-06-22    30
    ## 2 2015-01-22    23
    ## 3 2014-07-23    19
    ## 4 2014-08-05    18
    ## 5 2014-09-22    16

``` r
  ## Spike occured on June 22, 2015.  
  ## What pages were edited?
  clean_congress_tweets %>%
    mutate(created = as.Date(created)) %>%
    filter(created == "2015-06-22") %>%
    count(subject, sort = TRUE) %>%
    print(n = 29)
```

    ## # A tibble: 29 × 2
    ##                                              subject     n
    ##                                                <chr> <int>
    ## 1    United States Congress Joint Economic Committee     2
    ## 2                                     Alaska Natives     1
    ## 3        American Catholic Philosophical Association     1
    ## 4                           American Financial Group     1
    ## 5                                          Bluetooth     1
    ## 6                                    Central America     1
    ## 7                             Citizens United v. FEC     1
    ## 8                                Colchester Garrison     1
    ## 9                                   Computer science     1
    ## 10                               Deductive reasoning     1
    ## 11                               Fort Wayne, Indiana     1
    ## 12                              God of War: Betrayal     1
    ## 13                                           Hunting     1
    ## 14 Institute of Electrical and Electronics Engineers     1
    ## 15                      List of U.S. state nicknames     1
    ## 16                                  Northern flicker     1
    ## 17               Penn Central Transportation Company     1
    ## 18                                   Predicate logic     1
    ## 19                                 Puzzle video game     1
    ## 20                              Red-winged blackbird     1
    ## 21                                     Rick Santorum     1
    ## 22                                       Scholarship     1
    ## 23                                  Seymour, Indiana     1
    ## 24                                  Skokie, Illinois     1
    ## 25                                       Thomas Robb     1
    ## 26                                  Tyrion Lannister     1
    ## 27                                United States Navy     1
    ## 28                       United States Pacific Fleet     1
    ## 29                              Waterway restoration     1

Someone had a very, very slow Monday on June 22, 2015.

Let's try a different tactic. What patterns arise if we look at the number of edits by time and day of the week?

``` r
clean_congress_tweets %>%
    mutate(created = as.POSIXct(created)) %>%
    mutate(day = factor(weekdays(created),levels = c("Sunday", "Saturday", "Friday", "Thursday", "Wednesday", "Tuesday", "Monday"))) %>%
    mutate(created = format(created, "%Y-%m-%d %H:00:00")) %>%
    count(day, created) %>%
    mutate(created = as.POSIXct(created)) %>%
    pad() %>%    ## padr::pad inserts NA rows for missing date/times
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
```

    ## pad applied on the interval: hour

![](congressedits_markdown_files/figure-markdown_github-ascii_identifiers/punchcard-1.png)

Some observations here:

-   Most edits happen starting around noon on weekdays, peak around 6pm and carry on through 9-10pm.
-   There is non-trivial Wikipedia activity happening at Congress in the early morning hours, particularly on Tuesdays and Thursdays. To be fair, these might just be one or two overnight politics-fueled editing sprees.

In fact - let's delve deeper into these late night edits. Which pages are actually being edited at completely unreasonable hours - say, between 12 midnight and 8:00 am?

``` r
  ## which pages had late night tweets?
  latenight <- clean_congress_tweets %>%
    select(subject, created, source) %>%
    mutate(created = as.POSIXct(created)) %>%
    mutate(hour = as.numeric(format(created, "%H"))) %>%
    filter(hour <= 8) %>%
    count(subject, sort = TRUE) 
  
  ## plotting frequency of late night tweets by page
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
```

![](congressedits_markdown_files/figure-markdown_github-ascii_identifiers/latenight-1.png)

Someone at the Nebraska Republican Party is working *hard*.

#### 3. What *types* of Wikipedia pages are being edited?

So far, we've dealt mainly with page titles. However, each Wikipedia page also belong to several categories, which we can scrape for each page using the `wikipediR` package. The raw output is a little hairy, so I wrote a function to make it a little easier on the eyes.

``` r
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

## example
getCategories("Bill Hader")
```

    ## [1] "Category:1978 births;Category:21st-century American male actors;Category:American impressionists (entertainers);Category:American male comedians;Category:American male film actors;Category:American male television actors;Category:American male voice actors;Category:American people of Danish descent;Category:American people of English descent;Category:American people of German descent;Category:American people of Irish descent;Category:American sketch comedians;Category:American stand-up comedians;Category:American television writers;Category:Comedians from Oklahoma;Category:Living people;Category:Male actors from Tulsa, Oklahoma;Category:Male television writers;Category:Peabody Award winners;Category:Primetime Emmy Award winners;Category:Scottsdale Community College alumni;Category:Writers from Tulsa, Oklahoma"

Beautiful. Now to recursively call `getCategories` for all the pages in our dataset, and wrangle it into a tidy tweet-category format.

``` r
  ## add empty "categories" column to tibble
  congress_categories <- clean_congress_tweets %>%
    mutate(categories = "")
  
  ## original function hit API limits, so we use a for loop to call the function separately for each row
  for (i in 1:length(congress_categories$subject)) {
    congress_categories$categories[i] <- getCategories(congress_categories$subject[i])
    print(paste(i, " of ", length(congress_categories$subject), sep = ""))
  }
  
  ## finally, convert long string of multiple categories into tidy tweet-category-level data
  congress_categories <- congress_categories %>%
    mutate(categories = strsplit(categories, ";")) %>%
    unnest(categories) %>%
    mutate(categories = str_replace_all(categories, "Category:", ""))
```

What a lot of work. So what **are** the most frequent categories of Wikipedia pages that get edited from Congress IP addresses?

``` r
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
         title = "Categories of Most Frequent Wikipedia Page Edits from Congress",
         subtitle = "Using 1,430 tweets scraped from @congressedits between Aug 17, 2014 and Sep 15, 2017.")
```

    ## Selecting by n

![](congressedits_markdown_files/figure-markdown_github-ascii_identifiers/mostfreqcategories-1.png)

In terms of edit activity, Republican representatives' pages are edited more often than those for Democrat representatives - at least within the date range of our tweets.

Outside of that, no particularly surprising results. If anything, we've learned that most of these edits are happening within the political bubble. The interesting stuff, if it's there, might lie outside of it.

#### 4. Which tweets got the most engagement?

Let's be real: some of these page edits are pretty weird. When a tweet pops up alerting you that [the Wikipedia page for Carly Rae Jepsen was edited from a Congress location](https://twitter.com/congressedits/status/908078698083086342), that's the kind of thing that Twitter takes notice of. Especially if the specific edit is to describe her as the "worlds \[*sic*\] greatest singer".

This is my shorthand for finding the rare non-political edit amongst this dataset. If anyone can tell me a better way to find out what shouldn't be edited from Congress but has been, I'm all ears.

Back to the weird stuff. Looking at some of the more popular tweets in terms of Twitter engagement (in terms of favorites and retweets), what actually got traction?

``` r
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
```

![](congressedits_markdown_files/figure-markdown_github-ascii_identifiers/twitterengagement-1.png)

A little back story: the @congressedits Twitter account actually gained minor fame back in 2014 in reporting the anonymous edits to the page on "Senate Intelligence Committee report on CIA torture". The [actual edit](https://en.wikipedia.org/w/index.php?diff=637495578&oldid=637494879), which removed a phrase describing "enhanced interrogation techniques" as "a euphemism for torture", was tweeted out and news media [quickly picked up on it](http://mashable.com/2014/12/10/senate-wikipedia-torture-report/#eppO_ueOCkqM). I'm a little surprised it's this low on the list.

As for the other categories, I'll let the chart speak for itself.

Further Exploration
-------------------

As always, some answers only brought up more questions. Some that I'd like to answer in a future revision:

-   High edit counts might just mirror a particular editor's working style (e.g. saving after each small edit versus saving once after all edits to a page are done). What if we measured edit activity by number of unique editors or number of unique days containing edits?
-   Does edit frequency change when the House or Senate is in session?
