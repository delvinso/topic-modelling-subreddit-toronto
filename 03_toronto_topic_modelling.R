library(tidyverse)
library(topicmodels)
library(tidytext)
# read in word counts
toronto_clean_words_count <- read_csv("data/toronto_clean_words_count.csv")

# ---- 1a. ALL DATA - convert our words to a document-term matrix, with each post as a document ----
# toronto_words_dtm <- toronto_clean_words_count %>% 
#   cast_dtm(post_id, word, n)

# toronto_words_dtm_tfidf <-  toronto_clean_words_count %>% cast_dtm(post_id, word, n, weighting = tm::weightTfIdf)

#### DO NOT RUN ####
dim(toronto_words_dtm) # 55702 posts, 220638 words
# remove sparse terms using a threshold of 92.5% to speed up LDA model. however sometimes the sparse/unique
# terms define topics. trade-off...
toronto_words_dtm <- tm::removeSparseTerms(toronto_words_dtm, 0.975)
dim(toronto_words_dtm) # 55702 posts, 703 words at 95
# 55702 posts, 1698 words at 97.5

# remove posts with 0 entries bc LDA doesn't like them
toronto_words_dtm  <- toronto_words_dtm[slam::row_sums(as.matrix(toronto_words_dtm)) >  0,]
dim(toronto_words_dtm) # 54504 703 at 95.
# 55008 1698 at 97.5
#### DO NOT RUN ####

# ---- 1b. SPLIT DATA - split data into training and validation dtm's -----

# we will need post_ids and their corresponding creation date which will be joined onto comments
toronto_posts <- read_csv("data/toronto_posts_2016_to_2018.csv")
toronto_post_dates <- toronto_posts %>% distinct(date, id) %>% rename("post_id" = id)

# join post ids with comment ids to endow dates
toronto_clean_words_count <- toronto_clean_words_count %>%
  mutate(post_id = gsub("t3_", "", post_id)) %>% 
  left_join(toronto_post_dates, by = "post_id")

# sanity check for dates
toronto_clean_words_count %>% distinct(date) %>% arrange(desc(date))

# randomly subset posts from october 2018 for test dataset

toronto_clean_words_count_oct_2018 <- toronto_clean_words_count %>%
  filter(date >= "2018-10-01") 

set.seed(1)
sample_oct_rows <- sample(nrow(toronto_clean_words_count_oct_2018), 50)
toronto_test_dtm <- toronto_clean_words_count_oct_2018[sample_oct_rows,] %>% 
  cast_dtm(post_id, word, n)

# run below if we want to use the entirety of 2018 october as test dataset
# toronto_test_dtm <- toronto_clean_words_count %>%
#   filter(date >= "2018-10-01") %>%
#   # arrange(desc(desc(date)))
#   cast_dtm(post_id, word, n)

toronto_test_dtm # 48 posts, 50 terms or 1788 posts, 37475 terms
# 2016 to 2018 september will be our training dataset

toronto_train_dtm <- toronto_clean_words_count %>%
  filter(date < "2018-10-01") %>%
  # arrange(desc(desc(date))) 
  cast_dtm(post_id, word, n)

toronto_train_dtm # 53841 posts, 216555 words

rm(toronto_clean_words_count)




# ---- 1c. 5-fold cross-validation for perplexity, different numbers of topics (took like 4 days to run..)  ----
# https://www.r-bloggers.com/cross-validation-of-topic-modelling/
library(doParallel)
# https://stackoverflow.com/questions/11162402/lda-topic-modeling-training-and-testing?noredirect=1&lq=1
# https://stackoverflow.com/questions/21355156/topic-models-cross-validation-with-loglikelihood-or-perplexity

# set up a cluster for parallel processing
cluster <- makeCluster(detectCores(logical = TRUE) - 1) # leave one CPU spare...
registerDoParallel(cluster)

# load up the needed R package on all the parallel sessions
clusterEvalQ(cluster, {
  library(topicmodels)
})
seed = 1234
burnin = 1000
iter = 1000
keep = 50
folds <- 5
set.seed(1234)
full_data <- toronto_words_dtm 
n <- nrow(full_data)
splitfolds <- sample(1:folds, n, replace = TRUE)
candidate_k <- c(2, 5, 10, 20, 50, 75, 100) # candidates for how many topics

# export all the needed R objects to the parallel sessions
clusterExport(cluster, c("seed", "full_data", "burnin", "iter", "keep", "splitfolds", "folds", "candidate_k"))

# we parallelize by the different number of topics.  A processor is allocated a value
# of k, and does the cross-validation serially.  This is because it is assumed there
# are more candidate values of k than there are cross-validation folds, hence it
# will be more efficient to parallelise
system.time({
  results <- foreach(j = 1:length(candidate_k), .combine = rbind) %dopar%{
    set.seed(seed)
    k <- candidate_k[j]
    results_1k <- matrix(0, nrow = folds, ncol = 2)
    colnames(results_1k) <- c("k", "perplexity")
    for(i in 1:folds){
      train_set <- full_data[splitfolds != i , ]
      valid_set <- full_data[splitfolds == i, ]
      
      fitted <- LDA(train_set, k = k, method = "Gibbs",
                    control = list(burnin = burnin, iter = iter, keep = keep) )
      results_1k[i,] <- c(k, perplexity(fitted, newdata = valid_set))
    }
    return(results_1k)
  }
})
stopCluster(cluster)
#1245 to 1:07, 1300s
# user   system  elapsed  for trp
# 0.84     0.83 53728.50 
results_df <- as.data.frame(results)
results_df <- read_csv("~/Dropbox/quarantine/toronto_perplexity_20.csv")


ggplot(results_df, aes(x = k, y = perplexity)) +
  geom_point() +
  geom_smooth(se = FALSE) +
  ggtitle("5-fold cross-validation of topic modelling for /r/Toronto ",
          "(ie five different models fit for each candidate number of topics)") +
  labs(x = "Candidate number of topics", y = "Perplexity when fitting the trained model to the hold-out set")





# ---- 2a. Using ALL data for topic modeling (2016 to October 2018) - LDA ----
start_time <- Sys.time()
# toronto_lda10 <- topicmodels::LDA(toronto_words_dtm, k = 10,
#                                   control = list(seed = 1234, alpha = 50/10),
#                                   method = "Gibbs")
# toronto_lda15 <- topicmodels::LDA(toronto_words_dtm, k = 15,
#                                   control = list(seed = 1234, alpha = 50/15),
#                                   method = "Gibbs")
# toronto_lda20 <- topicmodels::LDA(toronto_words_dtm, 
#                                       k = 20, 
#                                       control = list(seed = 1234, alpha = 50/20),
#                                   method = "Gibbs")


# 41 minutes at k = 10, all terms
# 1 hour at k = 15, all terms
# 2 hours at k = 20, all terms
end_time <- Sys.time()
end_time - start_time

# saveRDS(toronto_lda10, "toronto_lda_10_topics_all.RDS")
# saveRDS(toronto_lda15, "toronto_lda_15_topics_all.RDS")
# saveRDS(toronto_lda20, "toronto_lda_20_topics_all.RDS")

toronto_lda10 <- readRDS("toronto_lda_10_topics_all.RDS")
toronto_lda15 <- readRDS("toronto_lda_15_topics_all.RDS")
toronto_lda20 <- readRDS("toronto_lda_20_topics_all.RDS")


# ---- 2b. Using training dtm (2016 to September 2018) for topic modeling - LDA -----

start_time <- Sys.time()
# toronto_lda10 <- topicmodels::LDA(toronto_words_dtm, k = 10,
#                                   control = list(seed = 1234, alpha = 50/10),
#                                   method = "Gibbs")
# toronto_lda15 <- topicmodels::LDA(toronto_words_dtm, k = 15,
#                                   control = list(seed = 1234, alpha = 50/15),
#                                   method = "Gibbs")
toronto_train_lda20 <- topicmodels::LDA(toronto_train_dtm,
                                        k = 20,
                                        control = list(seed = 1234, alpha = 50/20),
                                        method = "Gibbs")

toronto_train_lda50 <- topicmodels::LDA(toronto_train_dtm,
                                        k = 50,
                                        control = list(seed = 1234, alpha = 50/50),
                                        method = "Gibbs")
# 41 minutes at k = 10, all terms
# 1 hour at k = 15, all terms
# 2 hours at k = 20, all terms
end_time <- Sys.time()
end_time - start_time

saveRDS(toronto_train_lda20, "toronto_train_lda_20_topics_2016_to_2018_09.RDS")
saveRDS(toronto_train_lda50, "toronto_train_lda_50_topics_2016_to_2018_09.RDS")

