
# Introduction
This exercise performs sentiment analysis on tweets and hashtags, explores topic modeling, and conducts an A/B test on a dataset using R. The code includes data preprocessing, word clouds, topic modeling using LDA, and various statistical analyses.

Load necessary packages. Install the packages first if you don't have them using `install.packages("packagename")`

```{r, warning = FALSE, message = FALSE}
library(data.table)
library(wordcloud)
library(tm)
library(slam)
library(openxlsx)
library(sentimentr)
library(tidyverse)
library(stargazer)
library(rms)
```


## Sentiment Analysis

### Data Import & Word Cloud

The file can be downloaded at [Refresha Tweets.xlsx](https://github.com/user-attachments/files/18375989/Refresha.Tweets.xlsx). 

Word clouds are created for both tweets and hashtags using the wordcloud package, visually representing word frequency. The bigger the words, the more frequent it occured.

```{r, warning = FALSE, message = FALSE}
data <- read.xlsx("Refresha Tweets.xlsx")

tweet_words <- data$Tweet
wordcloud(tweet_words, min.freq = 1, max.words = 50,
          colors = brewer.pal(8, "Dark2"))

hashtag_words <- data$Hashtag
wordcloud(hashtag_words, min.freq = 1, max.words = 50,
          colors = brewer.pal(8, "Dark2"))

#######################

#Run sentiment analysis
data$tweet_sentiment <- sapply(data$Tweet, function(text) sentiment(text)$tweet_sentiment)

#sentiment count
data$tweet_sentiment_category <- ifelse(data$Tweet > 0.5, "Positive",
                                        ifelse(data$Tweet < -0.5, "Negative", "Neutral"))

#Count the number of each sentiment category
tweet_sentiment_count <- table(data$tweet_sentiment_category)

##Further exploration of hashtags
#Run sentiment analysis
data$hashtag_sentiment <- sapply(data$Hashtag, function(text) sentiment(text)$hashtag_sentiment)

#sentiment count
data$hashtag_sentiment_category <- ifelse(data$Hashtag > 0.5, "Positive",
                                          ifelse(data$Hashtag < -0.5, "Negative", "Neutral"))

#Count the number of each sentiment category
hashtag_sentiment_count <- table(data$hashtag_sentiment_category)

# view the counts 
print(hashtag_sentiment_count)
```

### Topic Modelling

```{r, warning = FALSE, message = FALSE}
#Create a Document-Term Matrix (DTM)
library(tm)

#Create a corpus
corpus <- Corpus(VectorSource(tweet_words))

#View the documents in the corpus
inspect(corpus)

#Pre-processing steps (e.g., remove punctuation, convert to lowercase)
corpus <- tm_map(corpus, content_transformer(tolower))
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, removeNumbers)
corpus <- tm_map(corpus, removeWords, stopwords("english"))

#Create the DTM
dtm <- DocumentTermMatrix(corpus)
inspect(dtm)

#Identify empty documents (documents with all zero entries) 
#Some zero entries are created due to stopword removal
empty_docs <- rowSums(as.matrix(dtm))==0

#Remove empty documents from the DTM
dtm <- dtm[!empty_docs,]

#View the DTM
inspect(dtm)

#Run Latent Dirichlet allocation (LDA)
library(topicmodels)
num_topics <- 10
lda_models <- LDA(dtm, k=num_topics)

#Get the terms associated with each topic
terms <- terms(lda_models, 15) #top 15 terms of each topic
print(terms)
```

### Further Exploration of Tweets

This section explores the occurences of words in the tweets and their usage.

```{r, warning = FALSE, message = FALSE}
# Calculate cooccurences of words in the tweets and plot
# parts of speech tagging - assigning parts of speech(Noun, verb, .. to token words)
library(udpipe)
udmodel <- udpipe_download_model(language = "english")
udmodel <- udpipe_load_model(file = udmodel$file_model)

x <- udpipe_annotate(udmodel, x = data$Tweet, 
                     doc_id = data$TweetID)
x <- as.data.frame(x)
table(x$upos)

##Words which follow one another
cooc <- cooccurrence(x = subset(x, upos %in% c("NOUN","ADJ", "VERB")), 
                     term = "lemma", 
                     skipgram = 1,
                     group = c("doc_id", "paragraph_id", "sentence_id"))

#top combinations
head(cooc, 5)
cooc

#visualization 
library(igraph)
wordnetwork <- head(cooc, 300)
wordnetwork <- graph_from_data_frame(wordnetwork)

library(ggraph)
ggraph(wordnetwork, layout = "fr") +
  geom_edge_link(aes(width = cooc, edge_alpha = cooc), 
                 edge_colour = "blue") +
  geom_node_text(aes(label = name), 
                 col = "darkgreen", size = 4) +
  theme_graph(base_family = "Arial Narrow") +
  labs(title = "Words following one another", 
       subtitle = "Nouns, Verb & Adjective")
```


## AB Test

Load the data [AB_test.csv](https://github.com/user-attachments/files/18376003/AB_test.csv)

```{r, warning = FALSE, message = FALSE}
data2 <- read.csv("AB_test.csv")
```

The results below show that the coupon is effective for students to sign up for the new AI course. The odds of a new course signup are 2.4881 times higher when a coupon is applied compared to when it's not and the mean aggregate compared to new AI course signup without coupon is significantly higher.

```{r, warning = FALSE, message = FALSE}
###convert previous AI courses taken to factor variable
data2$num_AI_courses <- as.factor(data2$num_AI_courses)

#set previous AI courses taken as reference group
data2$num_AI_courses <- relevel(data2$num_AI_courses, ref = "1")

###convert previous non-AI courses taken to factor variable
data2$num_non_AI_courses <- as.factor(data2$num_non_AI_courses)

#set previous non-AI courses taken as reference group
data2$num_non_AI_courses <- relevel(data2$num_non_AI_courses, ref = "1")

#compare means & sd of treatment and control group
aggregate(new_course_signup~coupon,
          data = data2,
          FUN=function(x)  c(mean=mean(x), sd=sd(x)))

#relationship between coupon and new AI course
fmla <- as.formula(paste("new_course_signup~coupon"))
m1 <- lrm(formula=fmla, data=data2)
dd <- datadist(data2)
options(datadist="dd")
summary(m1)

#relationship between new course signup, coupon and AI course
fmla <- as.formula(paste("new_course_signup~coupon + num_AI_courses"))
m2 <- lm(formula=fmla, data=data2)
summary(m2)

#relationship between new course signup, coupon and non AI course
fmla <- as.formula(paste("new_course_signup~coupon + num_non_AI_courses"))
m3 <- lm(formula=fmla, data=data2)
summary(m3)

#relationship between all variables
m4 <- lm(formula="new_course_signup~coupon + num_non_AI_courses +
                     num_AI_courses + gender", data=data2)

stargazer(m1, m2, m3, m4, type="text", single.row=T)
```

The interaction effect between the new AI course and the variables listed below were significant and positive. Therefore, they are the variables suggested that should be considered.
- Coupon
- Number of AI courses previously taken 
- Gender

```{r, warning = FALSE, message = FALSE}
#interaction effect between new course and AI courses
fmla <- as.formula(paste("new_course_signup~coupon*num_AI_courses"))
lm_course_signup_AI <- lm(formula=fmla, data=data2)
summary(lm_course_signup_AI)

fmla <- as.formula(paste("new_course_signup~coupon*num_non_AI_courses"))
lm_course_signup_non_AI <- lm(formula=fmla, data=data2)
summary(lm_course_signup_non_AI)

#compare results
fmla <- as.formula(paste("new_course_signup~coupon*num_AI_courses + num_non_AI_courses +
                     + gender"))
lm_course_signup_AI2 <- lm(formula=fmla, data = data2)

stargazer(lm_course_signup_AI, lm_course_signup_non_AI, lm_course_signup_AI2, type="text", single.row=T) 
```

