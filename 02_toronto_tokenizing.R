library(tidytext)
library(tidyverse)
library(topicmodels)

# ---- read in data -----
  toronto <- read_csv("data/toronto_comments_2016_to_2018.csv")
  
  # sanity check that we have all months up to 2018 october 
  toronto %>% 
    mutate(year_month = paste(as.character(lubridate::year(date_month_day)),
                              as.character(lubridate::month(date_month_day)))) %>%
    distinct(year_month) %>%
    arrange(year_month) %>%
    print(n = 50)
  
  # # of comments before 2018 october
  toronto %>% filter(as.Date(date_month_day) <= "2018-09-30")
  
  # reddit stop list from https://github.com/eigenfoo/reddit-clusters
  stop_list_reddit <- read_csv("data/stoplist.txt", col_names = FALSE) %>% rename("word" = "X1")

# ---- tokenizing words -----
  
  toronto_words <- toronto %>% 
    filter(!author %in% ("thetorontobot")) %>% 
    group_by(post_id = link_id) %>%
    unnest_tokens(word, body)
  rm(toronto)
  
  # remove stop words, digits and punctuation then count the number of words by post
  toronto_clean_words_count <- toronto_words  %>% 
    ungroup() %>% 
    anti_join(stop_words) %>% 
    anti_join(stop_list_reddit) %>% 
    filter(!word %in% c("https", "deleted", "amp", "http", "gt", "removed", "www.reddit.com",
                        "city", "toronto"),
           !str_detect(word, "\\d")) %>% 
    # remove punctuation AFTEr because stop words have punctuation in them
    mutate(word = gsub("[[:punct:]]", "", word)) %>% 
    filter(word != "") %>%
    count(word, post_id, sort = TRUE)
  rm(toronto_words)

# ---- save data as tokenizing and cleaning is a lengthy process ----
  write_csv(toronto_clean_words_count, "data/toronto_clean_words_count.csv")

