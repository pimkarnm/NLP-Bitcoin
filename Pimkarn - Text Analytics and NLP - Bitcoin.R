################################################
## Text Analytics and NLP: Bitcoin
################################################

#load necessary packages
library(dplyr)
library(tidytext)
library(tidyverse)
library(stringr)
library(ggplot2)

#load CSV data  
library(textreadr)
library(readr)

##############################
## Extract Bitcoin Price
##############################

#load Bitcoin price
btc_price <- read_csv("./datasets/csv/BTC-USD.csv")

#create new dataset without missing data
btc_price_nona <- na.omit(btc_price)

#create line chart
ggplot(btc_price_nona,
       aes(x = Date, y = Close)) +
    geom_line() +
    labs(x = 'Date',
         y = "Close Price",
         title = "Bitcoin Price Chart",
         subtitle = "February 2016 - February 2021" ) +
    scale_x_date(date_breaks = "year",
                 date_labels = "%b %y")

#########################################
## Analysis I: Sentiment analysis
#########################################

##############################
## Extract News articles 
##############################

## Load news articles 
library(pdftools) 

# importing all PDF files 
setwd("................/datasets/pdf") #specify local path!!

nm <- list.files(path="................/datasets/pdf")

my_pdf <- do.call(rbind, lapply(nm, function(x) paste(pdf_text(x), collapse = " ")))

# change column name to text
colnames(my_pdf) <- c("text")

# create new object to store news 
bitcoin_news <- data.frame(line = 1:28, text = my_pdf)

## Working with stop words
# create custom stop words to remove uninformative words
custom_stop_words_btc <- tribble(
    ~word,    ~lexicon,
    "crypto",    "CUSTOM",
    "cryptocurrency",    "CUSTOM",
    "cryptocurrencies",  "CUSTOM",
    "https",    "CUSTOM",
    "bitcoin",  "CUSTOM",
    "t.co",     "CUSTOM",
    "btc",      "CUSTOM",  
    "1",        "CUSTOM",
    "3",        "CUSTOM",
    "5",        "CUSTOM",
    "24",       "CUSTOM",
    "10",       "CUSTOM",
    "100",      "CUSTOM",
    "yahoo",    "CUSTOM",
    "finance",  "CUSTOM"
)

# create new object for custom stop words
stop_words_btc <- stop_words %>% 
    bind_rows(custom_stop_words_btc)

library(stringr)

# create vector for news sources
news_sources <- c('bitcoin','bitcoin','bitcoin',
                  'bloomberg','bloomberg','bloomberg',
                  'coindesk','coindesk','coindesk',
                  'cointelegraph','cointelegraph','cointelegraph',
                  'ft','ft','ft',
                  'medium','medium',
                  'newsbtc','newsbtc',
                  'reddit','reddit','reddit',
                  'wsj','wsj','wsj',
                  'yahoo','yahoo','yahoo')

tidy_news <-  bitcoin_news %>% 
    add_column(sources = news_sources) %>% 
    unnest_tokens(word, text) %>% 
    filter(!str_detect(word, "\\d+")) %>%
    anti_join(stop_words_btc, by = "word")

# visualize
tidy_news %>%
    count(word) %>%
    inner_join(get_sentiments("loughran"), by = "word") %>%
    group_by(sentiment) %>%
    top_n(5, n) %>%
    ungroup() %>%
    mutate(word = reorder(word, n)) %>%
    ggplot(aes(word, n)) +
    geom_col() +
    coord_flip() +
    facet_wrap(~ sentiment, scales = "free") +
    ylab("Frequency of this word in the recent Bitcoin articles")


#########################################
## Analysis II: TF-IDF
#########################################

library(rtweet)

#Keyword: BITCOIN or BTC
bitcoin <- search_tweets("bitcoin OR btc", n = 18000, include_rts = FALSE, lang = "en")

# create vector for twitter 
add_twitter <- tibble(sources = "twitter")

# find the most common words across bitcoin tweet
tidy_bitcoin <- bitcoin %>%
    select(user_id, status_id, created_at, screen_name, source, text) %>% 
    add_column(add_twitter) %>% 
    unnest_tokens(word, text) %>%
    anti_join(stop_words_btc, by = "word")
word_counts_bitcoin <- tidy_bitcoin %>% 
    count(word, sort = TRUE) %>% 
    filter(n > 200) %>% 
    mutate(word2_bitcoin = fct_reorder(word, n))

# create bar plot
ggplot(word_counts_bitcoin,
       aes( x = word2_bitcoin, y = n)) +
    geom_col() +
    coord_flip() +
    labs(
        title = "Bitcoin Word Counts",
        x = "Word",
        y = "Counts"
    )

# create custom stop words to remove uninformative words for TF-IDF
custom_stop_words_btc_idf <- tribble(
    ~word,    ~lexicon,
    "bitcoin.com",    "CUSTOM",
    "news.bitcoin.com",    "CUSTOM",
    "site:bitcoin.com",  "CUSTOM",
    "bloomberg",    "CUSTOM",
    "site:bloomberg.com",  "CUSTOM",
    "coindesk",    "CUSTOM",
    "site:coindesk.com",  "CUSTOM", 
    "cointelegraph",    "CUSTOM",
    "site:cointelegraph.com",  "CUSTOM", 
    "times",    "CUSTOM",
    "site:ft.com",  "CUSTOM", 
    "medium",    "CUSTOM",
    "site:medium.com",  "CUSTOM", 
    "reddit",    "CUSTOM",
    "site:reddit.com",  "CUSTOM", 
    "newsbtc.com",    "CUSTOM",
    "site:newsbtc.com",  "CUSTOM", 
    "newsbtc",    "CUSTOM",
    "journal",  "CUSTOM", 
    "site:wsj.com",  "CUSTOM", 
    "finance",  "CUSTOM",
    "street",  "CUSTOM" ,
    "wall",  "CUSTOM",
    "shutterstock",  "CUSTOM",
    "pst",  "CUSTOM",
    "el",  "CUSTOM",
    "de",  "CUSTOM",
    "sobre",  "CUSTOM",
    "la",  "CUSTOM",
    "su",  "CUSTOM",
    "los",  "CUSTOM",
    "votes",  "CUSTOM",
    "comments",  "CUSTOM",
    "ta",  "CUSTOM",
    "ft",  "CUSTOM" 
)

# create new object for custom stop words
stop_words_btc_idf <- stop_words_btc %>% 
    bind_rows(custom_stop_words_btc_idf)

# combine twitter and news articles data
news_tweet <- bind_rows(mutate(tidy_bitcoin),
                        mutate(tidy_news)) 

news_words <- news_tweet %>%
    anti_join(stop_words_btc_idf, by = "word") %>% 
    count(sources, word, sort = TRUE) %>% 
    ungroup()

# sum total words by sources
total_words <- news_words %>%
    group_by(sources) %>%
    summarize(total = sum(n))

sources_words <- left_join(news_words, total_words)

# calculate term frequency
freq_by_rank <- sources_words %>%
    group_by(sources) %>%
    mutate(rank = row_number(),
           `term frequency` = n/total)

sources_words <- sources_words %>%
    bind_tf_idf(word, sources, n)  %>%
    select(-total) %>%
    arrange(desc(tf_idf))

#visualize
sources_words %>%
    arrange(desc(tf_idf)) %>%
    filter(!str_detect(word, "\\d+")) %>%
    mutate(word = factor(word, levels = rev(unique(word)))) %>%
    group_by(sources) %>%
    # filter(n<50) %>% 
    top_n(8) %>%
    ungroup %>%
    ggplot(aes(word, tf_idf, fill = sources)) +
    geom_col(show.legend = FALSE) +
    labs(x = NULL, y = "tf-idf") +
    facet_wrap(~sources, ncol = 2, scales = "free") +
    coord_flip()



