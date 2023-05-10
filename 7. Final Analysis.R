##### Load environment #####
#Set a seed to ensure reproducibility 
set.seed(10)

#Load datasets
tweets <- read.csv('tweets.csv')
news <- read.csv('newsdata.csv')

#Load libraries 
library(ggplot2)
library(dplyr)
library(stringi)
library(tm)
library(tidyr)
library(topicmodels)
library(cvTools)
library(grid)
library(reshape)
library(quanteda)
library(quanteda.textstats)
library(stm)
library(gridExtra)
##### News: Pre-Analysis steps #####

#Remove news duplicates
length(unique(news$body)) 
news <- news[!duplicated(news$body),] 

#Recode a date 
news$Date <- ifelse(news$Date == '2022 The 18', "18 Jan 2022", news$Date)

##### Twitter: Pre-Analysis steps #####

#Change tweets date format to fit with the news data
tweets$created_at <- strptime(tweets$created_at, "%a %b %d %H:%M:%S %z %Y")
tweets$date <- format(tweets$created_at, "%d %b %Y")

#Reorganise the order of the columns
tweets <- cbind(tweets[, c("id", "created_at", "date", "tweet")])
##### News: Descriptive statistics #####

#Get distribution of articles per source
prop.table(table(news$Source))

#Get distribution of articles per day
prop.table(table(news$Date))

#Change order of the dates such that they are ordered chronologically
dates <- distinct(news, Date)
date_order <- arrange(dates, 
                      ifelse(Date == "31 Dec 2021", 0, 
                             ifelse(Date == "01 Feb 2022", 2, 1)),
                      Date)$Date

# convert Date to a factor with the desired order
news$Date <- factor(news$Date, levels = date_order)

#Plot distribution of articles per day and source 
plot1 <- ggplot(data = news, aes(x = Date, fill = Source)) + 
  geom_bar(position = 'stack', color = 'black') + 
  scale_fill_manual(values = c('gray30', 'gray60', 'gray90')) + 
  xlab('Date') +
  ylab('Number of Articles Published') +
  ggtitle('Distribution of Articles per Day and Source') + 
  theme(plot.background = element_rect(fill = "gray100"),
        panel.background = element_rect(fill = 'gray95'),
        panel.grid.major = element_line(colour = 'gray100'),
        plot.title = element_text(hjust = 0.5),
        axis.title.x = element_text(hjust = 0.5),
        axis.title.y = element_text(hjust = 0.5), 
        axis.text.x = element_text(angle = 90, hjust = 1))

ggsave('News - Descriptive Statistics.pdf', plot = plot1)
##### Twitter: Descriptive statistics #####

# Create the plot
plot2 <- ggplot(data = tweets, aes(x = date)) +
  geom_bar(color = 'black', fill = 'gray60') + 
  xlab('Date') + 
  ylab('Number of Tweets') +
  ggtitle('Distribution of Tweets per Day') +
  theme(plot.title = element_text(hjust = 0.5),
        axis.title.x = element_text(hjust = 0.5),
        axis.title.y = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 90, hjust = 1), 
        plot.background = element_rect(fill = "gray100"),
        panel.background = element_rect(fill = 'gray95'),
        panel.grid.major = element_line(colour = 'gray100'))

plot2

ggsave('Twitter - Descriptive Statistics.pdf', plot = plot2)

##### News: Pre-processing #####
#Create a new variable for preprocessing
news_p <- news

#Replace the space between certain words by a hyphen so that R considers them as one word
news_p$body <- gsub("(?i)(?<=\\bhuman\\b)\\s+(?=\\bright[s]?\\b)", "-", news_p$body, perl = TRUE)
news_p$body <- gsub("(?i)(?<=\\bcivil\\b)\\s+(?=\\bright[s]?\\b)", "-", news_p$body, perl = TRUE)
news_p$body <- gsub("(?i)(?<=\\bpolitical\\b)\\s+(?=\\bright[s]?\\b)", "-", news_p$body, perl = TRUE)
news_p$body <- gsub("(?i)(?<=\\bcivil\\b)\\s+(?=\\bliberty\\b)", "-", news_p$body, perl = TRUE)
news_p$body <- gsub("(?i)(?<=\\bcivil\\b)\\s+(?=\\bliberties\\b)", "-", news_p$body, perl = TRUE)
news_p$body <- gsub("(?i)\\b(crime[s]?)\\s+against\\s+humanity\\b", "\\1-against-humanity", news_p$body, perl = TRUE)
news_p$body <- gsub("(?i)\\b(rule)\\s+(of)\\s+(law)\\b", "\\1-\\2-\\3", news_p$body, perl = TRUE)
news_p$body <- gsub("(?i)\\b(freedom\\s+of)\\s+(\\w+)", "\\1-\\2", news_p$body, perl = TRUE)
news_p$body <- gsub("(?i)\\b(freedom)\\s+(of-)\\b", "\\1-\\2", news_p$body, perl = TRUE)
news_p$body <- gsub("(?i)(?<=\\bbodily\\b)\\s+(?=\\bintegrity\\b)", "-", news_p$body, perl = TRUE)
news_p$body <- gsub("(?i)(?<=\\bbodily\\b)\\s+(?=\\bautonomy\\b)", "-", news_p$body, perl = TRUE)
news_p$body <- gsub("(?i)\\b(right\\s+to)\\s+(\\w+)", "\\1-\\2", news_p$body, perl = TRUE)
news_p$body <- gsub("(?i)\\b(right)\\s+(to-)\\b", "\\1-\\2", news_p$body, perl = TRUE)
news_p$body <- gsub("(?i)(?<=\\bvoting\\b)\\s+(?=\\bright[s]?\\b)", "-", news_p$body, perl = TRUE)
news_p$body <- gsub("(?i)(?<=\\bsexual\\b)\\s+(?=\\bviolence\\b)", "-", news_p$body, perl = TRUE)
news_p$body <- gsub("(?i)(?<=\\bhuman\\b)\\s+(?=\\btrafficking\\b)", "-", news_p$body, perl = TRUE)
news_p$body <- gsub("(?i)(?<=\\bgender\\b)\\s+(?=\\bequality\\b)", "-", news_p$body, perl = TRUE)

#Remove URLs
news_p$body <- gsub("https?://\\S+|www\\.\\S+", "", news_p$body)

#Hyphenise any two words starting with a capital letter
news_p$body <- gsub("\\b([[:upper:]][[:lower:]]*)\\s([[:upper:]][[:lower:]]*)\\b", "\\1-\\2", news_p$body, perl = TRUE)

##### Twitter: Pre-processing #####
#Create a copy of the tweets for preprocessing 
tweets_p <- tweets

#Replace the space between certain words by a hyphen so that R considers them as one word
tweets_p$tweet <- gsub("(?i)(?<=\\bhuman\\b)\\s+(?=\\bright[s]?\\b)", "-", tweets_p$tweet, perl = TRUE)
tweets_p$tweet <- gsub("(?i)(?<=\\bcivil\\b)\\s+(?=\\bright[s]?\\b)", "-", tweets_p$tweet, perl = TRUE)
tweets_p$tweet <- gsub("(?i)(?<=\\bpolitical\\b)\\s+(?=\\bright[s]?\\b)", "-", tweets_p$tweet, perl = TRUE)
tweets_p$tweet <- gsub("(?i)(?<=\\bcivil\\b)\\s+(?=\\bliberty\\b)", "-", tweets_p$tweet, perl = TRUE)
tweets_p$tweet <- gsub("(?i)(?<=\\bcivil\\b)\\s+(?=\\bliberties\\b)", "-", tweets_p$tweet, perl = TRUE)
tweets_p$tweet <- gsub("(?i)\\b(crime[s]?)\\s+against\\s+humanity\\b", "\\1-against-humanity", tweets_p$tweet, perl = TRUE)
tweets_p$tweet <- gsub("(?i)\\b(rule)\\s+(of)\\s+(law)\\b", "\\1-\\2-\\3", tweets_p$tweet, perl = TRUE)
tweets_p$tweet <- gsub("(?i)\\b(freedom\\s+of)\\s+(\\w+)", "\\1-\\2", tweets_p$tweet, perl = TRUE)
tweets_p$tweet <- gsub("(?i)\\b(freedom)\\s+(of-)\\b", "\\1-\\2", tweets_p$tweet, perl = TRUE)
tweets_p$tweet <- gsub("(?i)(?<=\\bbodily\\b)\\s+(?=\\bintegrity\\b)", "-", tweets_p$tweet, perl = TRUE)
tweets_p$tweet <- gsub("(?i)(?<=\\bbodily\\b)\\s+(?=\\bautonomy\\b)", "-", tweets_p$tweet, perl = TRUE)
tweets_p$tweet <- gsub("(?i)\\b(right\\s+to)\\s+(\\w+)", "\\1-\\2", tweets_p$tweet, perl = TRUE)
tweets_p$tweet <- gsub("(?i)\\b(right)\\s+(to-)\\b", "\\1-\\2", tweets_p$tweet, perl = TRUE)
tweets_p$tweet <- gsub("(?i)(?<=\\bvoting\\b)\\s+(?=\\bright[s]?\\b)", "-", tweets_p$tweet, perl = TRUE)
tweets_p$tweet <- gsub("(?i)(?<=\\bsexual\\b)\\s+(?=\\bviolence\\b)", "-", tweets_p$tweet, perl = TRUE)
tweets_p$tweet <- gsub("(?i)(?<=\\bnovak\\b)\\s+(?=\\bdjokovic\\b)", "-", tweets_p$tweet, perl = TRUE)
tweets_p$tweet <- gsub("(?i)(?<=\\bunited\\b)\\s+(?=\\bkingdom\\b)", "-", tweets_p$tweet, perl = TRUE)
tweets_p$tweet <- gsub("(?i)(?<=\\bunited\\b)\\s+(?=\\bstates\\b)", "-", tweets_p$tweet, perl = TRUE)
tweets_p$tweet <- gsub("(?i)(?<=\\bunited\\b)\\s+(?=\\bnations\\b)", "-", tweets_p$tweet, perl = TRUE)
tweets_p$tweet <- gsub("(?i)\\b(Rape)\\s+Of\\s+Britain\\b", "\\1-Of-Britain", tweets_p$tweet, perl = TRUE)
tweets_p$tweet <- gsub("(?i)(?<=\\bsupreme\\b)\\s+(?=\\bcourt\\b)", "-", tweets_p$tweet, perl = TRUE)
tweets_p$tweet <- gsub("(?i)(?<=\\bpriti\\b)\\s+(?=\\bpatel\\b)", "-", tweets_p$tweet, perl = TRUE)
tweets_p$tweet <- gsub("(?i)(?<=\\bboris\\b)\\s+(?=\\bjohnson\\b)", "-", tweets_p$tweet, perl = TRUE)
tweets_p$tweet <- gsub("(?i)(?<=\\bhuman\\b)\\s+(?=\\btrafficking\\b)", "-", tweets_p$tweet, perl = TRUE)
tweets_p$tweet <- gsub("(?i)(?<=\\btheresa\\b)\\s+(?=\\bmay\\b)", "-", tweets_p$tweet, perl = TRUE)
tweets_p$tweet <- gsub("(?i)(?<=\\bgender\\b)\\s+(?=\\bequality\\b)", "-", tweets_p$tweet, perl = TRUE)
tweets_p$tweet <- gsub("(?i)(?<=\\bhong\\b)\\s+(?=\\bkong\\b)", "-", tweets_p$tweet, perl = TRUE)
tweets_p$tweet <- gsub("(?i)\\b(Nationality)\\s+And\\s+Borders\\s+Bill\\b", "\\1-And-Borders-Bill", tweets_p$tweet, perl = TRUE)
tweets_p$tweet <- gsub("(?i)\\b(Online)\\s+Safety\\s+Bill\\b", "\\1-Safety-Bill", tweets_p$tweet, perl = TRUE)
tweets_p$tweet <- gsub("(?i)(?<=\\bself\\b)\\s+(?=\\bdetermination\\b)", "-", tweets_p$tweet, perl = TRUE)

#Remove some observations which are irrelevant
tweets_p <- tweets_p[-grep("#nsfw", tweets_p$tweet), ]
tweets_p <- tweets_p[-grep("nipple", tweets_p$tweet), ]
tweets_p <- tweets_p[-grep("sexy", tweets_p$tweet), ]
tweets_p <- tweets_p[-grep("@Dominatrix", tweets_p$tweet), ]
tweets_p <- tweets_p[-grep("bath bombs", tweets_p$tweet), ]
tweets_p <- tweets_p[-grep("horny", tweets_p$tweet), ]
tweets_p <- tweets_p[-grep("bigdick", tweets_p$tweet), ]
tweets_p <- tweets_p[-grep("Disney", tweets_p$tweet), ]
tweets_p <- tweets_p[-grep("@avra_jam", tweets_p$tweet), ]
tweets_p <- tweets_p[-grep("titties", tweets_p$tweet), ]
tweets_p <- tweets_p[-grep("@Chloefayexxx", tweets_p$tweet), ]
tweets_p <- tweets_p[-grep("@coldrain1210", tweets_p$tweet), ]
tweets_p <- tweets_p[-grep("@tortured_toffee", tweets_p$tweet), ]

#Remove certain symbols
tweets_p$tweet <- gsub("(RT|via)((?:\\b\\W*@\\w+)+)", " ", tweets_p$tweet, ignore.case = TRUE)
tweets_p$tweet <- gsub("@\\w+", " ", tweets_p$tweet)
tweets_p$tweet <- gsub("<[^>]+>", " ", tweets_p$tweet)

#Mark non-UTF-8 characters
tweets_p$tweet_wrong <- stri_enc_mark(tweets_p$tweet) 
#Remove non-UTF-8 characters
tweets_p$tweet <- stri_replace_all_regex(tweets_p$tweet, "\\p{Cc}", "")
tweets_p$tweet <- gsub("(?<=\\B#|\\s)[^\\s\\p{L}#]+(?=\\s|$)", "", tweets_p$tweet, perl = TRUE)
tweets_p <- tweets_p[, 1:4]

#Remove URLs
tweets_p$tweet <- gsub("https?://\\S+|www\\.\\S+", " ", tweets_p$tweet)

#Aggregate processed tweets into one document 
tweets_agg <- paste(tweets_p$tweet, collapse = ' ')


##### News: Cross-Validation #####
#Create a DTM object for cross-validation

#Convert to corpus
corpus_news <- VCorpus(VectorSource(news_p$body))

# Clean and preprocess the corpus
corpus_news_clean <- corpus_news %>%
  tm_map(removeNumbers) %>%
  tm_map(removePunctuation) %>%
  tm_map(removeWords, c(stopwords('en'), 'Independent', 'Guardian', 'Times')) %>%
  tm_map(stemDocument) %>%
  tm_map(content_transformer(tolower))

# Create a document term matrix (dtm)
dtm_news <- DocumentTermMatrix(corpus_news_clean, control = list(minWordLength = 2, minDocFreq = 0.05, maxDocFreq = 0.95))

#Cross Validation function
#Define cross-validation function 
cross_validation <- function(topics, dtm , cv = 10) {
  folds <- cvFolds(nrow(dtm), cv, 1)
  perplex <- rep(NA , cv)
  llk <- rep(NA , cv)
  for (i in unique(folds$which)) {
    cat(i, " ")
    which_test <- folds$subsets[folds$which==i]
    which_train <- {1:nrow(dtm)}[-which_test]
    dtm_train <- dtm[which_train,]
    dtm_test <- dtm[which_test,]
    lda_fit <- LDA(dtm_train, k= topics, method="Gibbs",
                   control=list(verbose=50L, iter=100))
    llk[i] <- logLik(lda_fit)
    perplex[i] <- topicmodels::perplexity(lda_fit, dtm_test)
  }
  return(list(K = topics,perplexity = perplex,logLik = llk))
}

#Define number of topics to test
topics_news <- c(10, 20, 30, 40, 50, 60, 70, 80, 90, 100)

#Initialise results list
results_news <- list()

#Run function
i = 1
for (topic in topics_news){
  cat("\n\n\n##########\n\n\n ", topic, "topics", "\n\n\n")
  res <- cross_validation(topic, dtm_news)
  results_news[[i]] <- res
  i = i + 1
}

#Save results as dataframe
cv_results_news <- data.frame(
  topics = rep(topics_news, each = 10),
  perplex =  unlist(lapply(results_news, '[[', 'perplexity')),
  loglk = unlist(lapply(results_news, '[[', 'logLik')),
  stringsAsFactors=F)

write.csv(cv_results_news, 'cross-validation - news.csv')

#Get ratio of each value according to worst value (remember we want to minimise perplexity and maximise log-likelihood)
cv_results_news$ratio_perplex <- cv_results_news$perplex / max(cv_results_news$perplex)
cv_results_news$ratio_lk <- cv_results_news$loglk / min(cv_results_news$loglk)

#Get average values and associated standard deviations
cv_results_news <- data.frame(cbind(
  aggregate(cv_results_news$ratio_perplex, by=list(cv_results_news$topics), FUN=mean),
  aggregate(cv_results_news$ratio_perplex, by=list(cv_results_news$topics), FUN=sd)$x,
  aggregate(cv_results_news$ratio_lk, by=list(cv_results_news$topics), FUN=mean)$x,
  aggregate(cv_results_news$ratio_lk, by=list(cv_results_news$topics), FUN=sd)$x),
  stringsAsFactors=F)
names(cv_results_news) <- c("topics", "ratio_perp", "sd_perp", "ratio_lk", "sd_lk")


melted_ratios <- melt(cv_results_news[,c("topics","ratio_perp", "ratio_lk")], id.vars="topics")
melted_sd <- melt(cv_results_news[,c("topics","sd_perp", "sd_lk")], id.vars="topics")
melted_ratios$sd <- melted_sd$value
levels(melted_ratios$variable) <- c("Perplexity", "Log Likelihood")

plot3 <- ggplot(melted_ratios, aes(x = topics, y = value, linetype = variable)) +
  geom_line() +
  geom_point(aes(shape = variable), fill = "black", shape = 21, size= 1.50) +
  geom_errorbar(aes(ymax = value + sd, ymin = value - sd), width = 4) +
  scale_y_continuous("Ratio with respect to worst value") +
  scale_x_continuous("Number of topics", 
                     breaks=c(10, 20, 30, 40, 50, 60, 70, 80, 90, 100)) +
  ggtitle('Cross-Validation Statistics for the Newspaper Data') +
  theme_bw() +
  theme(axis.line = element_line(colour = "black"),
        plot.title = element_text(hjust = 0.5),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.background = element_rect(fill = "gray100"),
        panel.background = element_rect(fill = 'gray99'),
        legend.key.size=unit(0.5, "cm"), legend.position=c(0.85,0.60),
        legend.box.background = element_rect(fill = "gray99", color = "black"),
        legend.box.margin = margin(6, 6, 6, 6),
        legend.box.spacing = unit(0.2, "cm"),
        legend.box.just = "left", 
        legend.direction="vertical",
        legend.title=element_blank())

ggsave('Newspapers - cross-validation.pdf', plot3)
##### Twitter: Cross-Validation #####
#Create a DTM object for cross-validation
#Combine tweets to create documents of around 1,000 words each so that we have enough documents for cross-validation
tweets_split <- lapply(split(tweets_p$tweet, rep(1:22, each = 400, length.out = length(tweets_p$tweet))), paste, collapse = " ")
tweets_split <- as.data.frame(tweets_split)
tweets_split <- t(tweets_split)

#Convert to corpus
corpus_tweets <- VCorpus(VectorSource(tweets_split))

#Clean the corpus
corpus_tweets_clean <- corpus_tweets %>%
  tm_map(removeNumbers) %>%
  tm_map(removePunctuation) %>%
  tm_map(removeWords, stopwords('en')) %>%
  tm_map(removeWords, c('amp', 'fuck', 'shit', 'tweeting', 'tweet', 'fucking')) %>%
  tm_map(stemDocument) %>%
  tm_map(content_transformer(tolower))

#Create DTM
dtm_tweets <- DocumentTermMatrix(corpus_tweets_clean, control = list(minWordLength = 2, minDocFreq = 5))

#Run cross-validation

#Topic tweets
topics_tweets <- c(10, 20, 30, 40, 50, 60, 70, 80, 90, 100)

#Initialise results list
results_tweets <- list()

#Run function
i = 1
for (j in topics_tweets){
  cat("\n\n\n##########\n\n\n ", j, "topics", "\n\n\n")
  res_tweets <- cross_validation(j, dtm_tweets)
  results_tweets[[i]] <- res_tweets
  i = i + 1
}

#Save results as dataframe
cv_results_tweets <- data.frame(
  topics = rep(topics_tweets, each = 10),
  perplex =  unlist(lapply(results_tweets, '[[', 'perplexity')),
  loglk = unlist(lapply(results_tweets, '[[', 'logLik')),
  stringsAsFactors=F)

write.csv(cv_results_tweets, 'Cross-Validation - Tweets.csv')

#Get ratio of each value according to worst value (remember we want to minimise perplexity and maximise log-likelihood)
cv_results_tweets$ratio_perplex <- cv_results_tweets$perplex / max(cv_results_tweets$perplex)
cv_results_tweets$ratio_lk <- cv_results_tweets$loglk / min(cv_results_tweets$loglk)

#Get average values and associated standard deviations
cv_results_tweets <- data.frame(cbind(
  aggregate(cv_results_tweets$ratio_perplex, by=list(cv_results_tweets$topics), FUN=mean),
  aggregate(cv_results_tweets$ratio_perplex, by=list(cv_results_tweets$topics), FUN=sd)$x,
  aggregate(cv_results_tweets$ratio_lk, by=list(cv_results_tweets$topics), FUN=mean)$x,
  aggregate(cv_results_tweets$ratio_lk, by=list(cv_results_tweets$topics), FUN=sd)$x),
  stringsAsFactors=F)

names(cv_results_tweets) <- c("topics", "ratio_perp", "sd_perp", "ratio_lk", "sd_lk")


melted_ratios_tweets <- melt(cv_results_tweets[,c("topics","ratio_perp", "ratio_lk")], id.vars="topics")
melted_sd_tweets <- melt(cv_results_tweets[,c("topics","sd_perp", "sd_lk")], id.vars="topics")
melted_ratios_tweets$sd <- melted_sd_tweets$value
levels(melted_ratios_tweets$variable) <- c("Perplexity", "Log Likelihood")

plot4 <- ggplot(melted_ratios_tweets, aes(x = topics, y = value, linetype = variable)) +
  geom_line() +
  geom_point(aes(shape = variable), fill = "black", shape = 21, size= 1.50) +
  geom_errorbar(aes(ymax = value + sd, ymin = value - sd), width = 4) +
  scale_y_continuous("Ratio with respect to worst value") +
  scale_x_continuous("Number of topics", 
                     breaks=c(10, 20, 30, 40, 50, 60, 70, 80, 90, 100)) +
  ggtitle('Cross-Validation Statistics for the Twitter Data') +
  theme_bw() +
  theme(axis.line = element_line(colour = "black"),
        plot.title = element_text(hjust = 0.5),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.background = element_rect(fill = "gray100"),
        panel.background = element_rect(fill = 'gray99'),
        legend.key.size=unit(0.5, "cm"), legend.position=c(0.85,0.60),
        legend.box.background = element_rect(fill = "gray99", color = "black"),
        legend.box.margin = margin(6, 6, 6, 6),
        legend.box.spacing = unit(0.2, "cm"),
        legend.box.just = "left", 
        legend.direction="vertical",
        legend.title=element_blank())

plot4

ggsave('Twitter - Cross-Validation.pdf', plot4)

##### News: Run LDA Model with k = 50 ####

#Create a dfm object
corpus2_news <- corpus(news_p$body, docvars = news_p)

#Convert to DFM and pre-process
dfm_news <- corpus2_news %>% 
  tokens(what = 'word', remove_punct = TRUE, remove_symbols = TRUE,
         remove_url = TRUE, remove_numbers = TRUE,
         split_hyphens = FALSE,
         include_docvars = TRUE) %>%
  tokens_remove(c(stopwords('en'), 'Independent', 'Guardian', 'Times')) %>%
  tokens_wordstem() %>%
  tokens_keep(min_nchar = 2) %>%
  dfm(tolower = TRUE) %>%
  dfm_trim(min_docfreq = 0.05, max_docfreq = 0.95, docfreq_type = 'prop')

#Get number of unique words
nfeat(dfm_news)

#Get top 10 words per newspaper
comparison_news <- textstat_frequency(dfm_news, 10, groups = Source, force = TRUE)

plot5 <- ggplot(comparison_news,
                aes(x=frequency,y=reorder(feature, frequency))) +
  facet_wrap(~group) +
  geom_col(width = 0.75, fill = 'gray30') +
  ylab("") +
  xlab("Frequency") + 
  ggtitle('Top 10 Words per Newspaper') +
  theme(plot.title = element_text(hjust = 0.5),
        plot.background = element_rect(fill = "gray100"),
        panel.background = element_rect(fill = 'gray95'),
        panel.grid.major = element_line(colour = 'gray100'),
        axis.title.x = element_text(hjust = 0.5), 
        strip.background =element_rect(fill="gray30"), strip.text =
          element_text(colour = 'white'))

ggsave('News - Top Words.pdf', plot5)

news_model <- LDA(dfm_news, k = 50, method = 'Gibbs', control = list(seed = 10, iter = 5000, estimate.beta = TRUE, alpha = 0.02))
saveRDS(news_model, "News - LDA Model.rds")

#Get posterior results
news_fitted <- posterior(news_model, dfm_news)

#Extract beta and gamma
news_beta <- news_fitted$terms
news_gamma <- news_fitted$topics

#Write csv
write.csv(news_beta, 'news_beta.csv')
write.csv(news_gamma, 'news_gamma.csv')
##### Twitter: Run LDA Model with k = 30 #####

#Create corpus from aggregated data
corpus2_tweets <- corpus(tweets_agg)

#Create DFM
#Data preprocessing
dfm_tweets <- corpus2_tweets %>% tokens(what = 'word', remove_punct = TRUE, remove_symbols = TRUE,
                                        remove_url = TRUE, remove_numbers = TRUE, include_docvars = FALSE) %>%
  tokens_remove(c(stopwords('en'), 'amp', 'shit', 'fuck', 'retweet', 'tweet', 'dick', 'fucking')) %>%
  tokens_wordstem() %>%
  tokens_keep(min_nchar = 2) %>%
  dfm(tolower = TRUE) %>%
  dfm_trim(min_termfreq = 5)

#How many unique words
nfeat(dfm_tweets)

#get top 20 words
top20_tweets <- textstat_frequency(dfm_tweets, 20, force = TRUE)
plot6 <- ggplot(top20, aes(x = frequency, y = reorder(feature, frequency))) +
  geom_col(width = 0.75, fill = 'gray30') +
  ylab("") +
  xlab("Frequency") + 
  ggtitle('Top 20 Words in Tweets') +
  theme(plot.title = element_text(hjust = 0.5),
        plot.background = element_rect(fill = "gray100"),
        panel.background = element_rect(fill = 'gray95'),
        panel.grid.major = element_line(colour = 'gray100'),
        axis.title.x = element_text(hjust = 0.5), 
        strip.background =element_rect(fill="gray30"), strip.text =
          element_text(colour = 'white'))

ggsave('Top 20 words in Tweets.pdf', plot6)

#Run model
tweets_model <- LDA(dfm_tweets, k = 30, method = 'Gibbs', control = list(seed = 10, iter = 5000, estimate.beta = TRUE, alpha = 0.03))
saveRDS(tweets_model, "Tweets - LDA Model3.rds")


#Get posterior results on disaggregated tweets

#Create a new corpus containing the disaggregated tweets 
corpus3_tweets <- corpus(tweets_p$tweet, docvars = tweets_p)

#Convert to DFM
dfm_tweets2 <- corpus3_tweets %>% tokens(what = 'word', remove_punct = TRUE, remove_symbols = TRUE,
                                         remove_url = TRUE, remove_numbers = TRUE, include_docvars = FALSE) %>%
  tokens_remove(c(stopwords('en'), 'amp', 'shit', 'fuck', 'retweet', 'tweet', 'dick', 'fucking')) %>%
  tokens_wordstem() %>%
  tokens_keep(min_nchar = 2) %>%
  dfm(tolower = TRUE) %>%
  dfm_trim(min_termfreq = 5)

#Drop empty rows following preprocessing
dfm_tweets2_notna <- dfm_subset(dfm_tweets2, ntoken(dfm_tweets2) > 0)

#Get results
tweets_fitted <- topicmodels::posterior(tweets_model, dfm_tweets2_notna)

#Extract beta and gamma
tweets_beta <- tweets_fitted$terms
tweets_gamma <- tweets_fitted$topics

#Write csv
write.csv(tweets_beta, 'tweets_beta.csv')
write.csv(tweets_gamma, 'tweets_gamma.csv')
##### News: Characterise Topics #####
#Get top 10 frequency words associated with each topic 
news_beta_log <- as.data.frame(t(log(news_beta)))
top_news_terms <- apply(news_beta_log, 2, function(x) {
  sorted <- order(x, decreasing = TRUE)
  head(rownames(news_beta_log)[sorted], 10)
})

top_news_terms <- as.data.frame(top_news_terms)

#Get top 10 FREX words associated with each topic
news_frex <- as.matrix(news_beta)
news_frex <- log(news_frex)
news_frex <- calcfrex(news_frex, w = 0.5)
news_frex <- as.data.frame(t(news_frex))
top_news_frex <- apply(news_frex, 2, function(x) {
  sorted <- order(x, decreasing = TRUE)
  head(rownames(t(news_beta))[sorted], 10)
})


#Get top 5 articles per topic
top_news <- list()
top_news_prob <- list()
top_news_inf <- news_gamma


for (i in 1:ncol(news_gamma)) {
  top_news_i <- character(5)
  top_news_prob_i <- numeric(5)
  
  for (j in 1:5) {
    max_idx <- which.max(top_news_inf[,i])
    top_news_i[j] <- row_number(top_news_inf)[max_idx]
    top_news_prob_i[j] <- top_news_inf[max_idx,i]
    top_news_inf[max_idx, i] <- -Inf
  }
  
  top_news[[i]] <- top_news_i
  top_news_prob[[i]] <- top_news_prob_i
}

top_news_df <- data.frame(matrix(unlist(top_news), ncol = 5, byrow = TRUE))
colnames(top_news_df) <- paste0("Top_News_", 1:5)
top_news_prob_df <- data.frame(matrix(unlist(top_news_prob), ncol = 5, byrow = TRUE))
colnames(top_news_prob_df) <- paste0("Top_News_Prob_", 1:5)
top_news_df_final <- cbind(top_news_df, top_news_prob_df)
##### Twitter: Characterise Topics #####

#Get top 10 frequency words per topic
tweets_beta_log <- as.data.frame(t(log(tweets_beta)))
top_tweets_terms <- apply(tweets_beta_log, 2, function(x) {
  sorted <- order(x, decreasing = TRUE)
  head(rownames(tweets_beta_log)[sorted], 10)
})

#Get top 10 FREX words
tweets_frex <- as.matrix(tweets_beta)
tweets_frex <- log(tweets_beta)
tweets_frex <- calcfrex(tweets_frex, w = 0.5)
tweets_frex <- as.data.frame(t(tweets_frex))
top_tweets_frex <- apply(tweets_frex, 2, function(x) {
  sorted <- order(x, decreasing = TRUE)
  head(rownames(t(tweets_beta))[sorted], 10)
})

#Get top 5 tweet per topic
top_tweets <- list()
top_tweets_prob <- list()
top_tweets_inf <- tweets_gamma


for (i in 1:ncol(tweets_gamma)) {
  top_tweets_i <- character(5)
  top_tweets_prob_i <- numeric(5)
  
  for (j in 1:5) {
    max_idx <- which.max(top_tweets_inf[,i])
    top_tweets_i[j] <- rownames(top_tweets_inf)[max_idx]
    top_tweets_prob_i[j] <- top_tweets_inf[max_idx,i]
    top_tweets_inf[max_idx, i] <- -Inf
  }
  
  top_tweets[[i]] <- top_tweets_i
  top_tweets_prob[[i]] <- top_tweets_prob_i
}

top_tweets_df <- data.frame(matrix(unlist(top_tweets), ncol = 5, byrow = TRUE))
colnames(top_tweets_df) <- paste0("Top_Tweet_", 1:5)
top_tweets_prob_df <- data.frame(matrix(unlist(top_tweets_prob), ncol = 5, byrow = TRUE))
colnames(top_tweets_prob_df) <- paste0("Top_Tweet_Prob_", 1:5)
top_tweets_df_final <- cbind(top_tweets_df, top_tweets_prob_df)
##### News: Clean Topic Modelling #####
#Load labelling
news_label <- read.csv('News - Topic Labelling.csv')

#Combine similar topics 
rownames(news_gamma) <- news_gamma$X
news_gamma <- news_gamma[, 2:51]
colnames(news_gamma) <- 1:50

#Assign each document to a topic
news_assignment <- list()
news_assignment_prob <- list()
for (i in 1:nrow(news_gamma)) {
  news_assignment[i] <- colnames(news_gamma)[which.max(news_gamma[i,])]
  news_assignment_prob[i] <- max(news_gamma[i,])
}

news_assignment_df <- cbind(unlist(news_assignment), unlist(news_assignment_prob))
news_assignment_df <- as.data.frame(news_assignment_df)
colnames(news_assignment_df) <- c('Assignment', 'Probability')
news_assignment_df$Assignment <- as.factor(news_assignment_df$Assignment)
news_assignment_df$Probability <- as.numeric(news_assignment_df$Probability)

news_order <- as.numeric(news_assignment_df$Assignment)
news_order <- sort(news_order)

plot7 <- ggplot(data = news_assignment_df, aes(x = as.factor(news_order))) + 
  geom_bar(color = 'black', fill = 'gray60') + 
  xlab('Topic') +
  ylab('Number of Articles Assigned') +
  ggtitle('Distribution of Articles per Topic') + 
  theme(plot.background = element_rect(fill = "gray100"),
        panel.background = element_rect(fill = 'gray95'),
        panel.grid.major = element_line(colour = 'gray100'),
        plot.title = element_text(hjust = 0.5),
        axis.title.x = element_text(hjust = 0.5),
        axis.title.y = element_text(hjust = 0.5), 
        axis.text.x = element_text(angle = 90, hjust = 1))

plot7

ggsave('News - Topic Assignment.pdf', plot = plot7)


#Remove documents assigned to irrelevant and incoherent topics
news_assignment_df <- news_assignment_df[!news_assignment_df$Assignment %in% c(1,8,10,11,14, 17,18, 25,26, 27,30, 31,34,44,45),]
news_label <- news_label[-c(1,8,10,11,14, 17,18, 25,26, 27,30, 31,34,44,45), ]

#Remove documents with less than a 0.3 probability of being assigned to any topic
for (i in 1:nrow(news_assignment_df)) {
  if (news_assignment_df$Probability[i] < 0.3) {
    news_assignment_df$Assignment[i] <- NA
  }
  
  else {
    news_assignment_df$Assignment[i] <- news_assignment_df$Assignment[i]
  }
}

table(news_assignment_df$Assignment)
sum(is.na(news_assignment_df$Assignment))
##### Twitter: Clean Topic Modelling #####
#Load labelling
tweets_label <- read.csv('Tweets - Topic Labelling.csv')
tweets_gamma <- read.csv('tweets_gamma.csv')

rownames(tweets_gamma) <- tweets_gamma$X
tweets_gamma <- tweets_gamma[, 2:31]
colnames(tweets_gamma) <- 1:30

tweets_assignment <- list()
tweets_assignment_prob <- list()
for (i in 1:nrow(tweets_gamma)) {
  tweets_assignment[i] <- colnames(tweets_gamma)[which.max(tweets_gamma[i,])]
  tweets_assignment_prob[i] <- max(tweets_gamma[i,])
}

tweets_docs <- dfm_tweets2_notna@docvars$docname_
tweets_docs <- as.data.frame(tweets_docs)

tweets_assignment_df <- cbind(unlist(tweets_assignment), unlist(tweets_assignment_prob))
tweets_assignment_df <- as.data.frame(tweets_assignment_df)
colnames(tweets_assignment_df) <- c('Assignment', 'Probability')
tweets_assignment_df$Assignment <- as.factor(tweets_assignment_df$Assignment)
tweets_assignment_df$Probability <- as.numeric(tweets_assignment_df$Probability)

ordered <- sort(as.numeric(unique(tweets_assignment_df$Assignment)))
tweets_order <- factor(tweets_assignment_df$Assignment, levels = ordered)

plot8 <- ggplot(data = tweets_assignment_df, aes(x = tweets_order)) + 
  geom_bar(color = 'black', fill = 'gray60') + 
  xlab('Topic') +
  ylab('Number of Tweets Assigned') +
  ggtitle('Distribution of Tweets per Topic') + 
  theme(plot.background = element_rect(fill = "gray100"),
        panel.background = element_rect(fill = 'gray95'),
        panel.grid.major = element_line(colour = 'gray100'),
        plot.title = element_text(hjust = 0.5),
        axis.title.x = element_text(hjust = 0.5),
        axis.title.y = element_text(hjust = 0.5), 
        axis.text.x = element_text(angle = 90, hjust = 1))

plot8

ggsave('Tweets - Topic Assignment.pdf', plot = plot8)

#Combine both plots
plot_both <- grid.arrange(plot7, plot8, nrow = 2, ncol = 1)
ggsave('Both - Topic Assignment.pdf', plot = plot_both)

#Combine topics
tweets_gamma_clean <- tweets_gamma
tweets_gamma_clean$'31' <- tweets_gamma_clean[,20] + tweets_gamma_clean[,24]
Topic_31 <- data.frame('Topic' = 31, 'Issue' = 'Discrimination', 'Country' = 'UK', 'UK' = 1, 'UK.Link' = NA, 'Western' = NA, 'Negative' = 1, 'Personal' = 0, 'Relevant' = 1)
tweets_label <- rbind(tweets_label, Topic_31)
tweets_gamma_clean <- tweets_gamma_clean[, -c(13, 14, 20, 24, 29, 30)]

tweets_assignment2 <- list()
tweets_assignment2_prob <- list()
for (i in 1:nrow(tweets_gamma_clean)) {
  tweets_assignment2[i] <- colnames(tweets_gamma_clean)[which.max(tweets_gamma_clean[i,])]
  tweets_assignment2_prob[i] <- max(tweets_gamma_clean[i,])
}

tweets_assignment_df2 <- cbind(unlist(tweets_assignment2), unlist(tweets_assignment2_prob))
tweets_assignment_df2 <- as.data.frame(tweets_assignment_df2)
colnames(tweets_assignment_df2) <- c('Assignment', 'Probability')
tweets_assignment_df2$Assignment <- as.factor(tweets_assignment_df2$Assignment)
tweets_assignment_df2$Probability <- as.numeric(tweets_assignment_df2$Probability)

table(tweets_assignment_df2$Assignment)

tweets_label <- tweets_label[-c(13, 14, 20, 24, 29, 30), ]

#Remove tweets that have an assignment probability of less than 30%
for (i in 1:nrow(tweets_assignment_df2)) {
  if (tweets_assignment_df2$Probability[i] < 0.3) {
    tweets_assignment_df2$Assignment[i] <- NA
  }
  
  else {
    tweets_assignment_df2$Assignment[i] <- tweets_assignment_df2$Assignment[i]
  }
}

table(tweets_assignment_df2$Assignment)
sum(is.na(tweets_assignment_df2$Assignment))
##### Hypothesis 1a: Newspapers cover more international news than domestic news #####
prop.table(table(news_label$UK))

#Get number of articles assigned to 1 or 0 for the UK variable
news_H1a <- ifelse(news_assignment_df$Assignment %in% c(5, 7, 9, 13, 15, 20, 21, 22, 24, 28, 39, 40, 42, 43, 50), 1, 0)
prop.table(table(news_H1a))

#Get number of articles assigned to 1 or 0 for the UK and the UK link variable
news_H1a2 <- news_label[news_label$UK == 1 | news_label$UK.Link == 1, ]
length(news_H1a2$Topic)/length(news_label$Topic)
1 - length(news_H1a2$Topic)/length(news_label$Topic)

news_H1a2 <- ifelse(news_assignment_df$Assignment %in% c(3, 5, 7, 9, 12, 13, 15, 20, 21, 22, 24, 28, 29, 36, 47, 39, 40, 42, 43, 50), 1, 0)
prop.table(table(news_H1a2))



##### Hypothesis 1b: Newspapers are more likely to cover international news from Western countries #####

#Strictly Western
news_H1b <- news_label[news_label$UK == 0,]
(sum(news_H1b$Western == 1) + 1) / (length(news_H1b$Topic) + 1)
1 - (sum(news_H1b$Western == 1) + 1) / (length(news_H1b$Topic) + 1)

news_H1b2 <- news_assignment_df[news_assignment_df$Assignment %in% c(2, 3, 4, 6, 12, 13, 16, 19, 23, 29, 32, 33, 35, 36, 37, 38, 41, 46, 47, 48, 49), ]
news_H1b21 <- ifelse(news_H1b2$Assignment %in% c(2, 3, 4, 6, 16, 23, 32, 33, 36, 13), 1, 0)
prop.table(table(news_H1b21))

#Western related
(sum(news_H1b$Western == 1) + sum(news_H1b$Western.Link == 1, na.rm = TRUE) + 1)/ (length(news_H1b$Topic) + 1)
1 - (sum(news_H1b$Western == 1) + sum(news_H1b$Western.Link == 1, na.rm = TRUE) + 1)/ (length(news_H1b$Topic) + 1)

news_H1b22 <- ifelse(news_H1b2$Assignment %in% c(2, 3, 4, 6, 16, 23, 32, 33, 36, 13, 12, 29, 19, 37, 38, 46), 1, 0)
prop.table(table(news_H1b22))
##### Hypothesis 2: Newspapers cover more stories of human rights violations #####
prop.table(table(news_label$Negative))

news_H2 <- ifelse(news_assignment_df$Assignment %in% c(6, 23, 36, 5, 21, 50), 0, 1)
prop.table(table(news_H2))

news_H2b <- news_label[news_label$UK == 1,]
prop.table(table(news_H2b$Negative))

news_H2b <- news_assignment_df[news_assignment_df$Assignment %in% c(5, 7, 9, 13, 15, 20, 21, 22, 24, 28, 39, 40, 42, 43, 50),]
news_H2b <- ifelse(news_H2b$Assignment %in% c(5, 21, 50), 0, 1)
prop.table(table(news_H2b))

news_H2c <- news_label[news_label$Western == 1,]
prop.table(table(news_H2c$Negative))

news_H2c <- news_assignment_df[news_assignment_df$Assignment %in% c(2, 3, 4, 6, 13, 16, 23, 32, 33, 36),]
news_H2c <- ifelse(news_H2c$Assignment %in% c(6, 23, 36), 0, 1)
prop.table(table(news_H2c))

prop.test(table(news_H2b), n = 297, p = 0.9036, conf.level = 0.95, alternative = 'less', correct = FALSE)
prop.test(table(news_H2c), n = 248, p = 0.9036, conf.level = 0.95, alternative = 'less', correct = FALSE)

##### Hypothesis 4: Comparison between News and Tweets #####
Overlap <- c(1, 2, 1, 0, 1, 0, 1, 0, 1, 1, 0, 1, 1, 3, 1, 0, 2, 2, 3, 1, 1, 0, 3, 2, 1)
tweets_label <- cbind(tweets_label, Overlap)
prop.table(table(tweets_label$Overlap)) #76% of topics somewhat overlap with news

#News values
#Proximity (1)
tweets_H11 <- tweets_label[tweets_label$UK == 1 | tweets_label$UK.Link == 1, ]
length(tweets_H11$Topic)/length(tweets_label$Topic)

tweets_v1 <- ifelse(tweets_assignment_df2$Assignment %in% c(4, 11, 1, 3, 7, 9, 10, 12, 25, 2, 19, 21, 6, 18, 26, 5, 17, 23, 13, 27), 1, 0)
prop.table(table(tweets_v1))

#Proximity (2)
tweets_H12 <- tweets_label[tweets_label$UK == 0,]
5/17*100
tweets_v12 <- tweets_assignment_df2[tweets_assignment_df2$Assignment %in% c(2, 3, 6, 7, 8, 9, 10, 12, 19, 21, 25, 28, 1, 4, 16, 22),]
tweets_v12 <- ifelse(tweets_v12$Assignment %in% c(22, 4, 11, 1, 16), 1, 0)
prop.table(table(tweets_v12))

#Negative
prop.table(table(tweets_label$Negative))
tweets_v2 <- ifelse(!tweets_assignment_df2$Assignment %in% c(11, 12), 1, 0)
prop.table(table(tweets_v2))
##### News: Robustness Checks - For alpha = {0.01, 0.5, 1} #####
#For alpha = 0.01
news_model_alpha1 <- LDA(dfm_news, k = 50, method = 'Gibbs', control = list(seed = 10, iter = 5000, estimate.beta = TRUE, alpha = 0.01))
news_fitted_alpha1 <- posterior(news_model_alpha, dfm_news)

news_beta_alpha1 <- news_fitted_alpha1$terms
news_gamma_alpha1 <- news_fitted_alpha1$topics

top_news_alpha1 <- list()
top_news_prob_alpha1 <- list()
top_news_inf_alpha1 <- news_gamma_alpha1


for (i in 1:ncol(news_gamma_alpha1)) {
  top_news_i_alpha1 <- character(5)
  top_news_prob_i_alpha1 <- numeric(5)
  
  for (j in 1:5) {
    max_idx_alpha1 <- which.max(top_news_inf_alpha1[,i])
    top_news_i_alpha1[j] <- row_number(top_news_inf_alpha1)[max_idx_alpha1]
    top_news_prob_i_alpha1[j] <- top_news_inf_alpha1[max_idx_alpha1,i]
    top_news_inf_alpha1[max_idx_alpha1, i] <- -Inf
  }
  
  top_news_alpha1[[i]] <- top_news_i_alpha1
  top_news_prob_alpha1[[i]] <- top_news_prob_i_alpha1
}

top_news_df_alpha1 <- data.frame(matrix(unlist(top_news_alpha1), ncol = 5, byrow = TRUE))
colnames(top_news_df_alpha1) <- paste0("Top_News_", 1:5)
top_news_prob_df_alpha1 <- data.frame(matrix(unlist(top_news_prob_alpha1), ncol = 5, byrow = TRUE))
colnames(top_news_prob_df_alpha1) <- paste0("Top_News_Prob_", 1:5)
top_news_df_final_alpha1 <- cbind(top_news_df_alpha1, top_news_prob_df_alpha1)

#For alpha = 0.5
news_model_alpha2 <- LDA(dfm_news, k = 50, method = 'Gibbs', control = list(seed = 10, iter = 5000, estimate.beta = TRUE, alpha = 0.5))
news_fitted_alpha2 <- posterior(news_model_alpha2, dfm_news)

news_beta_alpha2 <- news_fitted_alpha2$terms
news_gamma_alpha2 <- news_fitted_alpha2$topics

top_news_alpha2 <- list()
top_news_prob_alpha2 <- list()
top_news_inf_alpha2 <- news_gamma_alpha2


for (i in 1:ncol(news_gamma_alpha2)) {
  top_news_i_alpha2 <- character(5)
  top_news_prob_i_alpha2 <- numeric(5)
  
  for (j in 1:5) {
    max_idx_alpha2 <- which.max(top_news_inf_alpha2[,i])
    top_news_i_alpha2[j] <- rownames(top_news_inf_alpha2)[max_idx_alpha2]
    top_news_prob_i_alpha2[j] <- top_news_inf_alpha2[max_idx_alpha2,i]
    top_news_inf_alpha2[max_idx_alpha2, i] <- -Inf
  }
  
  top_news_alpha2[[i]] <- top_news_i_alpha2
  top_news_prob_alpha2[[i]] <- top_news_prob_i_alpha2
}

top_news_df_alpha2 <- data.frame(matrix(unlist(top_news_alpha2), ncol = 5, byrow = TRUE))
colnames(top_news_df_alpha2) <- paste0("Top_News_", 1:5)
top_news_prob_df_alpha2 <- data.frame(matrix(unlist(top_news_prob_alpha2), ncol = 5, byrow = TRUE))
colnames(top_news_prob_df_alpha2) <- paste0("Top_News_Prob_", 1:5)
top_news_df_final_alpha2 <- cbind(top_news_df_alpha2, top_news_prob_df_alpha2)

#For alpha = 1
news_model_alpha3 <- LDA(dfm_news, k = 50, method = 'Gibbs', control = list(seed = 10, iter = 5000, estimate.beta = TRUE, alpha = 1))
news_fitted_alpha3 <- posterior(news_model_alpha3, dfm_news)

news_beta_alpha3 <- news_fitted_alpha3$terms
news_gamma_alpha3 <- news_fitted_alpha3$topics

top_news_alpha3 <- list()
top_news_prob_alpha3 <- list()
top_news_inf_alpha3 <- news_gamma_alpha3


for (i in 1:ncol(news_gamma_alpha3)) {
  top_news_i_alpha3 <- character(5)
  top_news_prob_i_alpha3 <- numeric(5)
  
  for (j in 1:5) {
    max_idx_alpha3 <- which.max(top_news_inf_alpha3[,i])
    top_news_i_alpha3[j] <- rownames(top_news_inf_alpha3)[max_idx_alpha3]
    top_news_prob_i_alpha3[j] <- top_news_inf_alpha3[max_idx_alpha3,i]
    top_news_inf_alpha3[max_idx_alpha3, i] <- -Inf
  }
  
  top_news_alpha3[[i]] <- top_news_i_alpha3
  top_news_prob_alpha3[[i]] <- top_news_prob_i_alpha3
}

top_news_df_alpha3 <- data.frame(matrix(unlist(top_news_alpha3), ncol = 5, byrow = TRUE))
colnames(top_news_df_alpha3) <- paste0("Top_News_", 1:5)
top_news_prob_df_alpha3 <- data.frame(matrix(unlist(top_news_prob_alpha3), ncol = 5, byrow = TRUE))
colnames(top_news_prob_df_alpha3) <- paste0("Top_News_Prob_", 1:5)
top_news_df_final_alpha3 <- cbind(top_news_df_alpha3, top_news_prob_df_alpha3)

##### News: Robustness Checks - For delta = {0.001, 0.01, 0.5, 1} #####
#For delta = 0.001
news_model_delta1 <- LDA(dfm_news, k = 50, method = 'Gibbs', control = list(seed = 10, iter = 5000, estimate.beta = TRUE, alpha = 0.02, delta = 0.001))
news_fitted_delta1 <- posterior(news_model_delta1, dfm_news)

news_beta_delta1 <- news_fitted_delta1$terms
news_gamma_delta1 <- news_fitted_delta1$topics

top_news_delta1 <- list()
top_news_prob_delta1 <- list()
top_news_inf_delta1 <- news_gamma_delta1


for (i in 1:ncol(news_gamma_delta1)) {
  top_news_i_delta1 <- character(5)
  top_news_prob_i_delta1 <- numeric(5)
  
  for (j in 1:5) {
    max_idx_delta1 <- which.max(top_news_inf_delta1[,i])
    top_news_i_delta1[j] <- rownames(top_news_inf_delta1)[max_idx_delta1]
    top_news_prob_i_delta1[j] <- top_news_inf_delta1[max_idx_delta1,i]
    top_news_inf_delta1[max_idx_delta1, i] <- -Inf
  }
  
  top_news_delta1[[i]] <- top_news_i_delta1
  top_news_prob_delta1[[i]] <- top_news_prob_i_delta1
}

top_news_df_delta1 <- data.frame(matrix(unlist(top_news_delta1), ncol = 5, byrow = TRUE))
colnames(top_news_df_delta1) <- paste0("Top_News_", 1:5)
top_news_prob_df_delta1 <- data.frame(matrix(unlist(top_news_prob_delta1), ncol = 5, byrow = TRUE))
colnames(top_news_prob_df_delta1) <- paste0("Top_News_Prob_", 1:5)
top_news_df_final_delta1 <- cbind(top_news_df_delta1, top_news_prob_df_delta1)

#For delta = 0.01
news_model_delta2 <- LDA(dfm_news, k = 50, method = 'Gibbs', control = list(seed = 10, iter = 5000, estimate.beta = TRUE, alpha = 0.02, delta = 0.01))
news_fitted_delta2 <- posterior(news_model_delta2, dfm_news)

news_beta_delta2 <- news_fitted_delta2$terms
news_gamma_delta2 <- news_fitted_delta2$topics

top_news_delta2 <- list()
top_news_prob_delta2 <- list()
top_news_inf_delta2 <- news_gamma_delta2


for (i in 1:ncol(news_gamma_delta2)) {
  top_news_i_delta2 <- character(5)
  top_news_prob_i_delta2 <- numeric(5)
  
  for (j in 1:5) {
    max_idx_delta2 <- which.max(top_news_inf_delta2[,i])
    top_news_i_delta2[j] <- rownames(top_news_inf_delta2)[max_idx_delta2]
    top_news_prob_i_delta2[j] <- top_news_inf_delta2[max_idx_delta2,i]
    top_news_inf_delta2[max_idx_delta2, i] <- -Inf
  }
  
  top_news_delta2[[i]] <- top_news_i_delta2
  top_news_prob_delta2[[i]] <- top_news_prob_i_delta2
}

top_news_df_delta2 <- data.frame(matrix(unlist(top_news_delta2), ncol = 5, byrow = TRUE))
colnames(top_news_df_delta2) <- paste0("Top_News_", 1:5)
top_news_prob_df_delta2 <- data.frame(matrix(unlist(top_news_prob_delta2), ncol = 5, byrow = TRUE))
colnames(top_news_prob_df_delta2) <- paste0("Top_News_Prob_", 1:5)
top_news_df_final_delta2 <- cbind(top_news_df_delta2, top_news_prob_df_delta2)

#For delta = 0.5
news_model_delta3 <- LDA(dfm_news, k = 50, method = 'Gibbs', control = list(seed = 10, iter = 5000, estimate.beta = TRUE, alpha = 0.02, delta = 0.5))
news_fitted_delta3 <- posterior(news_model_delta3, dfm_news)

news_beta_delta3 <- news_fitted_delta3$terms
news_gamma_delta3 <- news_fitted_delta3$topics

top_news_delta3 <- list()
top_news_prob_delta3 <- list()
top_news_inf_delta3 <- news_gamma_delta3


for (i in 1:ncol(news_gamma_delta3)) {
  top_news_i_delta3 <- character(5)
  top_news_prob_i_delta3 <- numeric(5)
  
  for (j in 1:5) {
    max_idx_delta3 <- which.max(top_news_inf_delta3[,i])
    top_news_i_delta3[j] <- rownames(top_news_inf_delta3)[max_idx_delta3]
    top_news_prob_i_delta3[j] <- top_news_inf_delta3[max_idx_delta3,i]
    top_news_inf_delta3[max_idx_delta3, i] <- -Inf
  }
  
  top_news_delta3[[i]] <- top_news_i_delta3
  top_news_prob_delta3[[i]] <- top_news_prob_i_delta3
}

top_news_df_delta3 <- data.frame(matrix(unlist(top_news_delta3), ncol = 5, byrow = TRUE))
colnames(top_news_df_delta3) <- paste0("Top_News_", 1:5)
top_news_prob_df_delta3 <- data.frame(matrix(unlist(top_news_prob_delta3), ncol = 5, byrow = TRUE))
colnames(top_news_prob_df_delta3) <- paste0("Top_News_Prob_", 1:5)
top_news_df_final_delta3 <- cbind(top_news_df_delta3, top_news_prob_df_delta3)

#For delta = 1
news_model_delta4 <- LDA(dfm_news, k = 50, method = 'Gibbs', control = list(seed = 10, iter = 5000, estimate.beta = TRUE, alpha = 0.02, delta = 1))
news_fitted_delta4 <- posterior(news_model_delta4, dfm_news)

news_beta_delta4 <- news_fitted_delta4$terms
news_gamma_delta4 <- news_fitted_delta4$topics

top_news_delta4 <- list()
top_news_prob_delta4 <- list()
top_news_inf_delta4 <- news_gamma_delta4


for (i in 1:ncol(news_gamma_delta4)) {
  top_news_i_delta4 <- character(5)
  top_news_prob_i_delta4 <- numeric(5)
  
  for (j in 1:5) {
    max_idx_delta4 <- which.max(top_news_inf_delta4[,i])
    top_news_i_delta4[j] <- rownames(top_news_inf_delta4)[max_idx_delta4]
    top_news_prob_i_delta4[j] <- top_news_inf_delta4[max_idx_delta4,i]
    top_news_inf_delta4[max_idx_delta4, i] <- -Inf
  }
  
  top_news_delta4[[i]] <- top_news_i_delta4
  top_news_prob_delta4[[i]] <- top_news_prob_i_delta4
}

top_news_df_delta4 <- data.frame(matrix(unlist(top_news_delta4), ncol = 5, byrow = TRUE))
colnames(top_news_df_delta4) <- paste0("Top_News_", 1:5)
top_news_prob_df_delta4 <- data.frame(matrix(unlist(top_news_prob_delta4), ncol = 5, byrow = TRUE))
colnames(top_news_prob_df_delta4) <- paste0("Top_News_Prob_", 1:5)
top_news_df_final_delta4 <- cbind(top_news_df_delta4, top_news_prob_df_delta4)

##### News: Robustness Checks - For n.iterations = {2500, 7500, 10000} #####
#For n.iterations = 2500
news_model_iter1 <- LDA(dfm_news, k = 50, method = 'Gibbs', control = list(seed = 10, iter = 2500, estimate.beta = TRUE, alpha = 0.02))
news_fitted_iter1 <- posterior(news_model_iter1, dfm_news)

news_beta_iter1 <- news_fitted_iter1$terms
news_gamma_iter1 <- news_fitted_iter1$topics

top_news_iter1 <- list()
top_news_prob_iter1 <- list()
top_news_inf_iter1 <- news_gamma_iter1


for (i in 1:ncol(news_gamma_iter1)) {
  top_news_i_iter1 <- character(5)
  top_news_prob_i_iter1 <- numeric(5)
  
  for (j in 1:5) {
    max_idx_iter1 <- which.max(top_news_inf_iter1[,i])
    top_news_i_iter1[j] <- rownames(top_news_inf_iter1)[max_idx_iter1]
    top_news_prob_i_iter1[j] <- top_news_inf_iter1[max_idx_iter1,i]
    top_news_inf_iter1[max_idx_iter1, i] <- -Inf
  }
  
  top_news_iter1[[i]] <- top_news_i_iter1
  top_news_prob_iter1[[i]] <- top_news_prob_i_iter1
}

top_news_df_iter1 <- data.frame(matrix(unlist(top_news_iter1), ncol = 5, byrow = TRUE))
colnames(top_news_df_iter1) <- paste0("Top_News_", 1:5)
top_news_prob_df_iter1 <- data.frame(matrix(unlist(top_news_prob_iter1), ncol = 5, byrow = TRUE))
colnames(top_news_prob_df_iter1) <- paste0("Top_News_Prob_", 1:5)
top_news_df_final_iter1 <- cbind(top_news_df_iter1, top_news_prob_df_iter1)

#For n.iterations = 7500
news_model_iter2 <- LDA(dfm_news, k = 50, method = 'Gibbs', control = list(seed = 10, iter = 7500, estimate.beta = TRUE, alpha = 0.02))
news_fitted_iter2 <- posterior(news_model_iter2, dfm_news)

news_beta_iter2 <- news_fitted_iter2$terms
news_gamma_iter2 <- news_fitted_iter2$topics

top_news_iter2 <- list()
top_news_prob_iter2 <- list()
top_news_inf_iter2 <- news_gamma_iter2


for (i in 1:ncol(news_gamma_iter2)) {
  top_news_i_iter2 <- character(5)
  top_news_prob_i_iter2 <- numeric(5)
  
  for (j in 1:5) {
    max_idx_iter2 <- which.max(top_news_inf_iter2[,i])
    top_news_i_iter2[j] <- rownames(top_news_inf_iter2)[max_idx_iter2]
    top_news_prob_i_iter2[j] <- top_news_inf_iter2[max_idx_iter2,i]
    top_news_inf_iter2[max_idx_iter2, i] <- -Inf
  }
  
  top_news_iter2[[i]] <- top_news_i_iter2
  top_news_prob_iter2[[i]] <- top_news_prob_i_iter2
}

top_news_df_iter2 <- data.frame(matrix(unlist(top_news_iter2), ncol = 5, byrow = TRUE))
colnames(top_news_df_iter2) <- paste0("Top_News_", 1:5)
top_news_prob_df_iter2 <- data.frame(matrix(unlist(top_news_prob_iter2), ncol = 5, byrow = TRUE))
colnames(top_news_prob_df_iter2) <- paste0("Top_News_Prob_", 1:5)
top_news_df_final_iter2 <- cbind(top_news_df_iter2, top_news_prob_df_iter2)

#For n.iterations = 10000
news_model_iter3 <- LDA(dfm_news, k = 50, method = 'Gibbs', control = list(seed = 10, iter = 10000, estimate.beta = TRUE, alpha = 0.02))
news_fitted_iter3 <- posterior(news_model_iter3, dfm_news)

news_beta_iter3 <- news_fitted_iter3$terms
news_gamma_iter3 <- news_fitted_iter3$topics

top_news_iter3 <- list()
top_news_prob_iter3 <- list()
top_news_inf_iter3 <- news_gamma_iter3


for (i in 1:ncol(news_gamma_iter3)) {
  top_news_i_iter3 <- character(5)
  top_news_prob_i_iter3 <- numeric(5)
  
  for (j in 1:5) {
    max_idx_iter3 <- which.max(top_news_inf_iter3[,i])
    top_news_i_iter3[j] <- rownames(top_news_inf_iter3)[max_idx_iter3]
    top_news_prob_i_iter3[j] <- top_news_inf_iter3[max_idx_iter3,i]
    top_news_inf_iter3[max_idx_iter3, i] <- -Inf
  }
  
  top_news_iter3[[i]] <- top_news_i_iter3
  top_news_prob_iter3[[i]] <- top_news_prob_i_iter3
}

top_news_df_iter3 <- data.frame(matrix(unlist(top_news_iter3), ncol = 5, byrow = TRUE))
colnames(top_news_df_iter3) <- paste0("Top_News_", 1:5)
top_news_prob_df_iter3 <- data.frame(matrix(unlist(top_news_prob_iter3), ncol = 5, byrow = TRUE))
colnames(top_news_prob_df_iter3) <- paste0("Top_News_Prob_", 1:5)
top_news_df_final_iter3 <- cbind(top_news_df_iter3, top_news_prob_df_iter3)

##### Twitter: Robustness Checks - For alpha = {0.01, 0.5, 1} ####

#Alpha = 0.01
tweets_model_alpha1 <- LDA(dfm_tweets, k = 30, method = 'Gibbs', control = list(seed = 10, iter = 5000, estimate.beta = TRUE, alpha = 0.01))
tweets_fitted_alpha1 <- topicmodels::posterior(tweets_model_alpha1, dfm_tweets2_notna)

tweets_beta_alpha1 <- tweets_fitted_alpha1$terms
tweets_gamma_alpha1 <- tweets_fitted_alpha1$topics

tweets_assignment_alpha1 <- list()
tweets_assignment_prob_alpha1 <- list()
for (i in 1:nrow(tweets_gamma_alpha1)) {
  tweets_assignment_alpha1[i] <- colnames(tweets_gamma_alpha1)[which.max(exp(tweets_gamma_alpha1[i,]))]
  tweets_assignment_prob_alpha1[i] <- max(exp(tweets_gamma_alpha1[i,]))
}

tweets_assignment_df_alpha1 <- cbind(unlist(tweets_assignment_alpha1), unlist(tweets_assignment_prob_alpha1))
tweets_assignment_df_alpha1 <- as.data.frame(tweets_assignment_df_alpha1)
colnames(tweets_assignment_df_alpha1) <- c('Assignment', 'Probability')
tweets_assignment_df_alpha1$Assignment <- as.factor(tweets_assignment_df_alpha1$Assignment)
tweets_assignment_df_alpha1$Probability <- as.numeric(tweets_assignment_df_alpha1$Probability)

ordered_alpha1 <- sort(as.numeric(unique(tweets_assignment_df_alpha1$Assignment)))
tweets_order_alpha1 <- factor(tweets_assignment_df_alpha1$Assignment, levels = ordered_alpha1)

plot_rob1 <- ggplot(data = tweets_assignment_df_alpha1, aes(x = tweets_order_alpha1)) + 
  geom_bar(color = 'black', fill = 'gray60') + 
  xlab('Topic') +
  ylab('Number of Tweets Assigned') +
  ggtitle('Distribution of Tweets per Topic for Alpha = 0.01') + 
  theme(plot.background = element_rect(fill = "gray100"),
        panel.background = element_rect(fill = 'gray95'),
        panel.grid.major = element_line(colour = 'gray100'),
        plot.title = element_text(hjust = 0.5),
        axis.title.x = element_text(hjust = 0.5),
        axis.title.y = element_text(hjust = 0.5), 
        axis.text.x = element_text(angle = 90, hjust = 1))

plot_rob1

ggsave('Tweets - Alpha 1.pdf', plot = plot_rob1)

#Alpha = 0.5
tweets_model_alpha2 <- LDA(dfm_tweets, k = 30, method = 'Gibbs', control = list(seed = 10, iter = 5000, estimate.beta = TRUE, alpha = 0.5))
tweets_fitted_alpha2 <- topicmodels::posterior(tweets_model_alpha2, dfm_tweets2_notna)

tweets_beta_alpha2 <- tweets_fitted_alpha2$terms
tweets_gamma_alpha2 <- tweets_fitted_alpha2$topics

tweets_assignment_alpha2 <- list()
tweets_assignment_prob_alpha2 <- list()
for (i in 1:nrow(tweets_gamma_alpha2)) {
  tweets_assignment_alpha2[i] <- colnames(tweets_gamma_alpha2)[which.max(exp(tweets_gamma_alpha2[i,]))]
  tweets_assignment_prob_alpha2[i] <- max(exp(tweets_gamma_alpha2[i,]))
}

tweets_assignment_df_alpha2 <- cbind(unlist(tweets_assignment_alpha2), unlist(tweets_assignment_prob_alpha2))
tweets_assignment_df_alpha2 <- as.data.frame(tweets_assignment_df_alpha2)
colnames(tweets_assignment_df_alpha2) <- c('Assignment', 'Probability')
tweets_assignment_df_alpha2$Assignment <- as.factor(tweets_assignment_df_alpha2$Assignment)
tweets_assignment_df_alpha2$Probability <- as.numeric(tweets_assignment_df_alpha2$Probability)

ordered_alpha2 <- sort(as.numeric(unique(tweets_assignment_df_alpha2$Assignment)))
tweets_order_alpha2 <- factor(tweets_assignment_df_alpha2$Assignment, levels = ordered_alpha2)

plot_rob2 <- ggplot(data = tweets_assignment_df_alpha2, aes(x = tweets_order_alpha2)) + 
  geom_bar(color = 'black', fill = 'gray60') + 
  xlab('Topic') +
  ylab('Number of Tweets Assigned') +
  ggtitle('Distribution of Tweets per Topic for Alpha = 0.5') + 
  theme(plot.background = element_rect(fill = "gray100"),
        panel.background = element_rect(fill = 'gray95'),
        panel.grid.major = element_line(colour = 'gray100'),
        plot.title = element_text(hjust = 0.5),
        axis.title.x = element_text(hjust = 0.5),
        axis.title.y = element_text(hjust = 0.5), 
        axis.text.x = element_text(angle = 90, hjust = 1))

plot_rob2

ggsave('Tweets - Alpha 2.pdf', plot = plot_rob2)

#Alpha = 1
tweets_model_alpha3 <- LDA(dfm_tweets, k = 30, method = 'Gibbs', control = list(seed = 10, iter = 5000, estimate.beta = TRUE, alpha = 1))
tweets_fitted_alpha3 <- topicmodels::posterior(tweets_model_alpha3, dfm_tweets2_notna)

tweets_beta_alpha3 <- tweets_fitted_alpha3$terms
tweets_gamma_alpha3 <- tweets_fitted_alpha3$topics

tweets_assignment_alpha3 <- list()
tweets_assignment_prob_alpha3 <- list()
for (i in 1:nrow(tweets_gamma_alpha3)) {
  tweets_assignment_alpha3[i] <- colnames(tweets_gamma_alpha3)[which.max(exp(tweets_gamma_alpha3[i,]))]
  tweets_assignment_prob_alpha3[i] <- max(exp(tweets_gamma_alpha3[i,]))
}

tweets_assignment_df_alpha3 <- cbind(unlist(tweets_assignment_alpha3), unlist(tweets_assignment_prob_alpha3))
tweets_assignment_df_alpha3 <- as.data.frame(tweets_assignment_df_alpha3)
colnames(tweets_assignment_df_alpha3) <- c('Assignment', 'Probability')
tweets_assignment_df_alpha3$Assignment <- as.factor(tweets_assignment_df_alpha3$Assignment)
tweets_assignment_df_alpha3$Probability <- as.numeric(tweets_assignment_df_alpha3$Probability)

ordered_alpha3 <- sort(as.numeric(unique(tweets_assignment_df_alpha3$Assignment)))
tweets_order_alpha3 <- factor(tweets_assignment_df_alpha3$Assignment, levels = ordered_alpha3)

plot_rob3 <- ggplot(data = tweets_assignment_df_alpha3, aes(x = tweets_order_alpha3)) + 
  geom_bar(color = 'black', fill = 'gray60') + 
  xlab('Topic') +
  ylab('Number of Tweets Assigned') +
  ggtitle('Distribution of Tweets per Topic for Alpha = 1') + 
  theme(plot.background = element_rect(fill = "gray100"),
        panel.background = element_rect(fill = 'gray95'),
        panel.grid.major = element_line(colour = 'gray100'),
        plot.title = element_text(hjust = 0.5),
        axis.title.x = element_text(hjust = 0.5),
        axis.title.y = element_text(hjust = 0.5), 
        axis.text.x = element_text(angle = 90, hjust = 1))

plot_rob3

ggsave('Tweets - Alpha 3.pdf', plot = plot_rob3)

#Combine all plots
plot_alpha <- grid.arrange(plot8, plot_rob1, plot_rob2, plot_rob3, nrow = 2, ncol = 2)
ggsave('Plots - Alpha.pdf', plot = plot_alpha)


##### Twitter: Robustness Checks - For delta = {0.001, 0.01, 0.5, 1} ####
#Delta = 0.001
tweets_model_delta1 <- LDA(dfm_tweets, k = 30, method = 'Gibbs', control = list(seed = 10, iter = 5000, estimate.beta = TRUE, alpha = 0.03, delta = 0.001))
tweets_fitted_delta1 <- topicmodels::posterior(tweets_model_delta1, dfm_tweets2_notna)

tweets_beta_delta1 <- tweets_fitted_delta1$terms
tweets_gamma_delta1 <- tweets_fitted_delta1$topics

tweets_assignment_delta1 <- list()
tweets_assignment_prob_delta1 <- list()
for (i in 1:nrow(tweets_gamma_delta1)) {
  tweets_assignment_delta1[i] <- colnames(tweets_gamma_delta1)[which.max(exp(tweets_gamma_delta1[i,]))]
  tweets_assignment_prob_delta1[i] <- max(exp(tweets_gamma_delta1[i,]))
}

tweets_assignment_df_delta1 <- cbind(unlist(tweets_assignment_delta1), unlist(tweets_assignment_prob_delta1))
tweets_assignment_df_delta1 <- as.data.frame(tweets_assignment_df_delta1)
colnames(tweets_assignment_df_delta1) <- c('Assignment', 'Probability')
tweets_assignment_df_delta1$Assignment <- as.factor(tweets_assignment_df_delta1$Assignment)
tweets_assignment_df_delta1$Probability <- as.numeric(tweets_assignment_df_delta1$Probability)

ordered_delta1 <- sort(as.numeric(unique(tweets_assignment_df_delta1$Assignment)))
tweets_order_delta1 <- factor(tweets_assignment_df_delta1$Assignment, levels = ordered_delta1)

plot_robd1 <- ggplot(data = tweets_assignment_df_delta1, aes(x = tweets_order_delta1)) + 
  geom_bar(color = 'black', fill = 'gray60') + 
  xlab('Topic') +
  ylab('Number of Tweets Assigned') +
  ggtitle('Distribution of Tweets per Topic for Delta = 0.001') + 
  theme(plot.background = element_rect(fill = "gray100"),
        panel.background = element_rect(fill = 'gray95'),
        panel.grid.major = element_line(colour = 'gray100'),
        plot.title = element_text(hjust = 0.5),
        axis.title.x = element_text(hjust = 0.5),
        axis.title.y = element_text(hjust = 0.5), 
        axis.text.x = element_text(angle = 90, hjust = 1))

plot_robd1

ggsave('Tweets - Delta 1.pdf', plot = plot_robd1)

#Delta = 0.01
tweets_model_delta2 <- LDA(dfm_tweets, k = 30, method = 'Gibbs', control = list(seed = 10, iter = 5000, estimate.beta = TRUE, alpha = 0.03, delta = 0.01))
tweets_fitted_delta2 <- topicmodels::posterior(tweets_model_delta2, dfm_tweets2_notna)

tweets_beta_delta2 <- tweets_fitted_delta2$terms
tweets_gamma_delta2 <- tweets_fitted_delta2$topics

tweets_assignment_delta2 <- list()
tweets_assignment_prob_delta2 <- list()
for (i in 1:nrow(tweets_gamma_delta2)) {
  tweets_assignment_delta2[i] <- colnames(tweets_gamma_delta2)[which.max(exp(tweets_gamma_delta2[i,]))]
  tweets_assignment_prob_delta2[i] <- max(exp(tweets_gamma_delta2[i,]))
}

tweets_assignment_df_delta2 <- cbind(unlist(tweets_assignment_delta2), unlist(tweets_assignment_prob_delta2))
tweets_assignment_df_delta2 <- as.data.frame(tweets_assignment_df_delta2)
colnames(tweets_assignment_df_delta2) <- c('Assignment', 'Probability')
tweets_assignment_df_delta2$Assignment <- as.factor(tweets_assignment_df_delta2$Assignment)
tweets_assignment_df_delta2$Probability <- as.numeric(tweets_assignment_df_delta2$Probability)

ordered_delta2 <- sort(as.numeric(unique(tweets_assignment_df_delta2$Assignment)))
tweets_order_delta2 <- factor(tweets_assignment_df_delta2$Assignment, levels = ordered_delta2)

plot_robd2 <- ggplot(data = tweets_assignment_df_delta2, aes(x = tweets_order_delta2)) + 
  geom_bar(color = 'black', fill = 'gray60') + 
  xlab('Topic') +
  ylab('Number of Tweets Assigned') +
  ggtitle('Distribution of Tweets per Topic for Delta = 0.01') + 
  theme(plot.background = element_rect(fill = "gray100"),
        panel.background = element_rect(fill = 'gray95'),
        panel.grid.major = element_line(colour = 'gray100'),
        plot.title = element_text(hjust = 0.5),
        axis.title.x = element_text(hjust = 0.5),
        axis.title.y = element_text(hjust = 0.5), 
        axis.text.x = element_text(angle = 90, hjust = 1))

plot_robd2

ggsave('Tweets - Delta 2.pdf', plot = plot_robd2)

#Delta = 0.5
tweets_model_delta3 <- LDA(dfm_tweets, k = 30, method = 'Gibbs', control = list(seed = 10, iter = 5000, estimate.beta = TRUE, alpha = 0.03, delta = 0.5))
tweets_fitted_delta3 <- topicmodels::posterior(tweets_model_delta3, dfm_tweets2_notna)

tweets_beta_delta3 <- tweets_fitted_delta3$terms
tweets_gamma_delta3 <- tweets_fitted_delta3$topics

tweets_assignment_delta3 <- list()
tweets_assignment_prob_delta3 <- list()
for (i in 1:nrow(tweets_gamma_delta3)) {
  tweets_assignment_delta3[i] <- colnames(tweets_gamma_delta3)[which.max(exp(tweets_gamma_delta3[i,]))]
  tweets_assignment_prob_delta3[i] <- max(exp(tweets_gamma_delta3[i,]))
}

tweets_assignment_df_delta3 <- cbind(unlist(tweets_assignment_delta3), unlist(tweets_assignment_prob_delta3))
tweets_assignment_df_delta3 <- as.data.frame(tweets_assignment_df_delta3)
colnames(tweets_assignment_df_delta3) <- c('Assignment', 'Probability')
tweets_assignment_df_delta3$Assignment <- as.factor(tweets_assignment_df_delta3$Assignment)
tweets_assignment_df_delta3$Probability <- as.numeric(tweets_assignment_df_delta3$Probability)

ordered_delta3 <- sort(as.numeric(unique(tweets_assignment_df_delta3$Assignment)))
tweets_order_delta3 <- factor(tweets_assignment_df_delta3$Assignment, levels = ordered_delta3)

plot_robd3 <- ggplot(data = tweets_assignment_df_delta3, aes(x = tweets_order_delta3)) + 
  geom_bar(color = 'black', fill = 'gray60') + 
  xlab('Topic') +
  ylab('Number of Tweets Assigned') +
  ggtitle('Distribution of Tweets per Topic for Delta = 0.5') + 
  theme(plot.background = element_rect(fill = "gray100"),
        panel.background = element_rect(fill = 'gray95'),
        panel.grid.major = element_line(colour = 'gray100'),
        plot.title = element_text(hjust = 0.5),
        axis.title.x = element_text(hjust = 0.5),
        axis.title.y = element_text(hjust = 0.5), 
        axis.text.x = element_text(angle = 90, hjust = 1))

plot_robd3

ggsave('Tweets - Delta 3.pdf', plot = plot_robd3)

#Delta = 1
tweets_model_delta4 <- LDA(dfm_tweets, k = 30, method = 'Gibbs', control = list(seed = 10, iter = 5000, estimate.beta = TRUE, alpha = 0.03, delta = 1))
tweets_fitted_delta4 <- topicmodels::posterior(tweets_model_delta4, dfm_tweets2_notna)

tweets_beta_delta4 <- tweets_fitted_delta4$terms
tweets_gamma_delta4 <- tweets_fitted_delta4$topics

tweets_assignment_delta4 <- list()
tweets_assignment_prob_delta4 <- list()
for (i in 1:nrow(tweets_gamma_delta4)) {
  tweets_assignment_delta4[i] <- colnames(tweets_gamma_delta4)[which.max(exp(tweets_gamma_delta4[i,]))]
  tweets_assignment_prob_delta4[i] <- max(exp(tweets_gamma_delta4[i,]))
}

tweets_assignment_df_delta4 <- cbind(unlist(tweets_assignment_delta4), unlist(tweets_assignment_prob_delta4))
tweets_assignment_df_delta4 <- as.data.frame(tweets_assignment_df_delta4)
colnames(tweets_assignment_df_delta4) <- c('Assignment', 'Probability')
tweets_assignment_df_delta4$Assignment <- as.factor(tweets_assignment_df_delta4$Assignment)
tweets_assignment_df_delta4$Probability <- as.numeric(tweets_assignment_df_delta4$Probability)

ordered_delta4 <- sort(as.numeric(unique(tweets_assignment_df_delta4$Assignment)))
tweets_order_delta4 <- factor(tweets_assignment_df_delta4$Assignment, levels = ordered_delta4)

plot_robd4 <- ggplot(data = tweets_assignment_df_delta4, aes(x = tweets_order_delta4)) + 
  geom_bar(color = 'black', fill = 'gray60') + 
  xlab('Topic') +
  ylab('Number of Tweets Assigned') +
  ggtitle('Distribution of Tweets per Topic for Delta = 1') + 
  theme(plot.background = element_rect(fill = "gray100"),
        panel.background = element_rect(fill = 'gray95'),
        panel.grid.major = element_line(colour = 'gray100'),
        plot.title = element_text(hjust = 0.5),
        axis.title.x = element_text(hjust = 0.5),
        axis.title.y = element_text(hjust = 0.5), 
        axis.text.x = element_text(angle = 90, hjust = 1))

plot_robd4

ggsave('Tweets - Delta 4.pdf', plot = plot_robd4)

#Combine all plots
plot_delta <- grid.arrange(plot8, plot_robd1, plot_robd2, plot_robd3, plot_robd4, nrow = 3, ncol = 2)
ggsave('Plots - Delta.pdf', plot = plot_delta)

##### Twitter: Robustness Checks - For n.iterations = {2500, 7500, 10000} #####
#Iter = 2,500
tweets_model_iter1 <- LDA(dfm_tweets, k = 30, method = 'Gibbs', control = list(seed = 10, iter = 2500, estimate.beta = TRUE, alpha = 0.03))
tweets_fitted_iter1 <- topicmodels::posterior(tweets_model_iter1, dfm_tweets2_notna)

tweets_beta_iter1 <- tweets_fitted_iter1$terms
tweets_gamma_iter1 <- tweets_fitted_iter1$topics

tweets_assignment_iter1 <- list()
tweets_assignment_prob_iter1 <- list()
for (i in 1:nrow(tweets_gamma_iter1)) {
  tweets_assignment_iter1[i] <- colnames(tweets_gamma_iter1)[which.max(exp(tweets_gamma_iter1[i,]))]
  tweets_assignment_prob_iter1[i] <- max(exp(tweets_gamma_iter1[i,]))
}

tweets_assignment_df_iter1 <- cbind(unlist(tweets_assignment_iter1), unlist(tweets_assignment_prob_iter1))
tweets_assignment_df_iter1 <- as.data.frame(tweets_assignment_df_iter1)
colnames(tweets_assignment_df_iter1) <- c('Assignment', 'Probability')
tweets_assignment_df_iter1$Assignment <- as.factor(tweets_assignment_df_iter1$Assignment)
tweets_assignment_df_iter1$Probability <- as.numeric(tweets_assignment_df_iter1$Probability)

ordered_iter1 <- sort(as.numeric(unique(tweets_assignment_df_iter1$Assignment)))
tweets_order_iter1 <- factor(tweets_assignment_df_iter1$Assignment, levels = ordered_iter1)

plot_iter1 <- ggplot(data = tweets_assignment_df_iter1, aes(x = tweets_order_iter1)) + 
  geom_bar(color = 'black', fill = 'gray60') + 
  xlab('Topic') +
  ylab('Number of Tweets Assigned') +
  ggtitle('Distribution of Tweets per Topic for 2,500 iterations') + 
  theme(plot.background = element_rect(fill = "gray100"),
        panel.background = element_rect(fill = 'gray95'),
        panel.grid.major = element_line(colour = 'gray100'),
        plot.title = element_text(hjust = 0.5),
        axis.title.x = element_text(hjust = 0.5),
        axis.title.y = element_text(hjust = 0.5), 
        axis.text.x = element_text(angle = 90, hjust = 1))

plot_iter1

ggsave('Tweets - Iter 1.pdf', plot = plot_iter1)

#Iter = 7,500
tweets_model_iter2 <- LDA(dfm_tweets, k = 30, method = 'Gibbs', control = list(seed = 10, iter = 7500, estimate.beta = TRUE, alpha = 0.03))
tweets_fitted_iter2 <- topicmodels::posterior(tweets_model_iter2, dfm_tweets2_notna)

tweets_beta_iter2 <- tweets_fitted_iter2$terms
tweets_gamma_iter2 <- tweets_fitted_iter2$topics

tweets_assignment_iter2 <- list()
tweets_assignment_prob_iter2 <- list()
for (i in 1:nrow(tweets_gamma_iter2)) {
  tweets_assignment_iter2[i] <- colnames(tweets_gamma_iter2)[which.max(exp(tweets_gamma_iter2[i,]))]
  tweets_assignment_prob_iter2[i] <- max(exp(tweets_gamma_iter2[i,]))
}

tweets_assignment_df_iter2 <- cbind(unlist(tweets_assignment_iter2), unlist(tweets_assignment_prob_iter2))
tweets_assignment_df_iter2 <- as.data.frame(tweets_assignment_df_iter2)
colnames(tweets_assignment_df_iter2) <- c('Assignment', 'Probability')
tweets_assignment_df_iter2$Assignment <- as.factor(tweets_assignment_df_iter2$Assignment)
tweets_assignment_df_iter2$Probability <- as.numeric(tweets_assignment_df_iter2$Probability)

ordered_iter2 <- sort(as.numeric(unique(tweets_assignment_df_iter2$Assignment)))
tweets_order_iter2 <- factor(tweets_assignment_df_iter2$Assignment, levels = ordered_iter2)

plot_iter2 <- ggplot(data = tweets_assignment_df_iter2, aes(x = tweets_order_iter2)) + 
  geom_bar(color = 'black', fill = 'gray60') + 
  xlab('Topic') +
  ylab('Number of Tweets Assigned') +
  ggtitle('Distribution of Tweets per Topic for 7,500 iterations') + 
  theme(plot.background = element_rect(fill = "gray100"),
        panel.background = element_rect(fill = 'gray95'),
        panel.grid.major = element_line(colour = 'gray100'),
        plot.title = element_text(hjust = 0.5),
        axis.title.x = element_text(hjust = 0.5),
        axis.title.y = element_text(hjust = 0.5), 
        axis.text.x = element_text(angle = 90, hjust = 1))

plot_iter2

ggsave('Tweets - Iter 2.pdf', plot = plot_iter2)

#Iter = 10,000
tweets_model_iter3 <- LDA(dfm_tweets, k = 30, method = 'Gibbs', control = list(seed = 10, iter = 10000, estimate.beta = TRUE, alpha = 0.03))
tweets_fitted_iter3 <- topicmodels::posterior(tweets_model_iter3, dfm_tweets2_notna)

tweets_beta_iter3 <- tweets_fitted_iter3$terms
tweets_gamma_iter3 <- tweets_fitted_iter3$topics

tweets_assignment_iter3 <- list()
tweets_assignment_prob_iter3 <- list()
for (i in 1:nrow(tweets_gamma_iter3)) {
  tweets_assignment_iter3[i] <- colnames(tweets_gamma_iter3)[which.max(exp(tweets_gamma_iter3[i,]))]
  tweets_assignment_prob_iter3[i] <- max(exp(tweets_gamma_iter3[i,]))
}

tweets_assignment_df_iter3 <- cbind(unlist(tweets_assignment_iter3), unlist(tweets_assignment_prob_iter3))
tweets_assignment_df_iter3 <- as.data.frame(tweets_assignment_df_iter3)
colnames(tweets_assignment_df_iter3) <- c('Assignment', 'Probability')
tweets_assignment_df_iter3$Assignment <- as.factor(tweets_assignment_df_iter3$Assignment)
tweets_assignment_df_iter3$Probability <- as.numeric(tweets_assignment_df_iter3$Probability)

ordered_iter3 <- sort(as.numeric(unique(tweets_assignment_df_iter3$Assignment)))
tweets_order_iter3 <- factor(tweets_assignment_df_iter3$Assignment, levels = ordered_iter3)

plot_iter3 <- ggplot(data = tweets_assignment_df_iter3, aes(x = tweets_order_iter3)) + 
  geom_bar(color = 'black', fill = 'gray60') + 
  xlab('Topic') +
  ylab('Number of Tweets Assigned') +
  ggtitle('Distribution of Tweets per Topic for 10,000 iterations') + 
  theme(plot.background = element_rect(fill = "gray100"),
        panel.background = element_rect(fill = 'gray95'),
        panel.grid.major = element_line(colour = 'gray100'),
        plot.title = element_text(hjust = 0.5),
        axis.title.x = element_text(hjust = 0.5),
        axis.title.y = element_text(hjust = 0.5), 
        axis.text.x = element_text(angle = 90, hjust = 1))

plot_iter3

ggsave('Tweets - Iter 3.pdf', plot = plot_iter3)

#Combine plots
plot_iter <- grid.arrange(plot8, plot_iter1, plot_iter2, plot_iter3, nrow = 2, ncol = 2)
ggsave('Plots - Iter.pdf', plot = plot_iter)