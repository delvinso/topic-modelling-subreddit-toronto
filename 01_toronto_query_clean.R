library(bigrquery)
library(tidyverse)


billing <- 'nyc-taxi-208814'

# In R, note that backslashes and quotes are escaped.

# ---- Method 1. The following pulls comments made between 2016 and 2018 from r/Toronto. ----
sql <-
  "
#standardSQL
SELECT body, author, 
Date(Timestamp_seconds(created_utc)) AS date_month_day,
link_id, id as comment_id
FROM `fh-bigquery.reddit_comments.201*` 
# WHERE _TABLE_SUFFIX > '16'
WHERE _TABLE_SUFFIX BETWEEN '6' and '9'
AND subreddit = 'toronto'
AND body NOT IN ('[deleted]', '[removed]')
AND author NOT IN (SELECT author FROM `fh-bigquery.reddit_comments.bots_201505`) 
AND author NOT IN ('AutoModerator')
"
toronto_comments <- bq_project_query(billing, sql, use_legacy_sql = FALSE) %>% 
  bq_table_download(., max_results = Inf, page_size = 5000)
  # bq_table_save(., ("gs://pushshift_ds/toronto_comments.csv"))

write_csv(toronto_comments, "data/toronto_comments_2016_to_2018.csv")




# ---- Method 2. The following pulls all counted WORDs from comments made between 2016 and 2018 from r/Toronto. ----
# NOT SUGGESTED FOR NOW. ----
sql2 <- "
#standardSQL
SELECT word, COUNT(*) AS num_words, link_id
FROM `fh-bigquery.reddit_comments.201*`,
UNNEST(SPLIT(LOWER(regexp_replace(body, r'[\\.\\\",*:()\\[\\]/|\\n]', ' ')), ' ')) word
# WHERE _TABLE_SUFFIX > '16'
WHERE _TABLE_SUFFIX BETWEEN '6' and '9'
AND subreddit = 'toronto'
AND body NOT IN ('[deleted]', '[removed]')
AND word NOT IN (SELECT word FROM `nyc-taxi-208814.my_dataset.stop_words`)
AND word != ''
AND author NOT IN (SELECT author FROM `fh-bigquery.reddit_comments.bots_201505`) 
AND author NOT IN ('AutoModerator')
GROUP BY word, link_id
#ORDER BY num_words DESC 
"

toronto_comments <- bq_project_query(billing, sql, use_legacy_sql = FALSE) %>% 
  # bq_table_download(., max_results = Inf, page_size = 5000)
bq_table_save(., ("gs://pushshift_ds/toronto_comments.csv"))

toronto_clean_words_count <- jsonlite::stream_in(file("data/toronto_words.csv")) %>% as_tibble()

toronto_clean_words_count  <- toronto_clean_words_count %>% 
  anti_join(stop_words) %>%
  # remove punctuation after stop words
  mutate(word = gsub("[[:punct:]]", "", word),
         word = trimws(word)) %>% 
  filter(!word %in% c("https", "deleted", "amp", "http", "gt", "removed", "www.reddit.com", "\r",
                      "city", "toronto", "", "&gt:", "www", "wiki", "wikipedia", "ca", "org", "en"), 
         !str_detect(word, "\\d"),
         !str_detect(word, "^\\s+")) %>%
  group_by(word, link_id) %>% summarize(num_words = sum(as.integer(num_words)))

toronto_clean_words_count %>% ungroup() %>% group_by(word) %>% 
  summarize(num_words = sum(as.integer(num_words))) %>%
  arrange(desc(num_words))
toronto_clean_words_count %>% write_csv("data/toronto_words_clean_count.csv")

toronto_clean_words_count


# ---- querying for posts so we can assign a given post a date -----
# we start from 2015 rather than 2016 as some comments made could be on a thread from the new year or even before
sql <-"
#standardSQL
SELECT title, subreddit, 
date(timestamp_seconds(created_utc)) AS date, 
format_date('%Y', date(timestamp_seconds(created_utc))) AS year, 
ID as id 
FROM `fh-bigquery.reddit_posts.201*`
WHERE _TABLE_SUFFIX BETWEEN '5' AND '9'
AND subreddit = 'toronto'
"
toronto_posts <-  bq_project_query(billing, sql, use_legacy_sql = FALSE) %>% 
  bq_table_download(., max_results = Inf)

toronto_posts %>% write_csv("data/toronto_posts_2016_to_2018.csv")
