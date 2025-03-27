---
title: "customer_service_analysis"
author: "Lateefah Yusuf"
date: "2025-03-15"
output: rmarkdown::github_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```


# Customer Service Satisfaction Research

This [dataset](https://www.kaggle.com/datasets/ddosad/ecommerce-customer-service-satisfaction) is from kaggle. It captures customer satisfaction scores for a one-month period at an e-commerce platform called Shopzilla (a pseudonym). It includes various features such as category and sub-category of interaction, customer remarks, survey response date, category, item price, agent details (name, supervisor, manager), and CSAT score etc.


```{r, warning = FALSE, message = FALSE}
pacman::p_load(
  tidyverse,      # Data manipulation and visualization
  lubridate,      # Date manipulation
  ggplot2,        # Visualization
  ggplot,
  dplyr,          # Data manipulation
  caret,          # Machine learning
  randomForest,   # Random Forest algorithm
  glmnet,         # Regularized regression
  forcats,        # Factor manipulation
  scales,         # Scale functions for visualization
  corrplot,       # Correlation visualization
  gridExtra,      # Multiple plots arrangement
  knitr,          # Tables formatting
  stringr,        # String manipulation
  pROC            # ROC curves
)
```


## Import data 


```{r pressure, echo=TRUE}
# Load the data
df <- read.csv("Customer_support_data.csv", stringsAsFactors = FALSE)

# Display basic information
head(df)

# Check data types 
str(df)
summary(df)

# Check for missing values
missing_values <- colSums(is.na(df))
print(missing_values)
```

## Data Pre-Processing

Upon observation of the dataset, I noticed that in some rows the data isn't properly inputted so they will be removed.


```{r}
# remove rows that didn't import properly
df <- df %>% 
  slice(-c(15437:15484, 45128, 45176))

# change CSAT to numeric 
df <- df %>% mutate(CSAT.Score=as.numeric(CSAT.Score), Item_price=as.numeric(Item_price))
```


I also noticed that several of the columns in the dataset didn't import properly when I tried to check for the distinct values (this might be a problem only while using R) so i will be cleaning most of the columns. 

For example, survey response date has have wildly different lengths. Some are 9 characters (the expected "01-Aug-23" format), but many are extremely long—up to 59765 characters which explains the "input string is too long" error when I try to convert it to date format


```{r}
# cleaning survey dates column
unique(df$Survey_response_Date)

# check date strings are the same length
all_lengths <- unique(nchar(df$Survey_response_Date))
print(all_lengths)

# Look at the actual byte representation of a sample string to reveal hidden characters
print(charToRaw(df$Survey_response_Date[1]))

# Let's try parsing a single value first as a test
test_date <- as.Date("01-Aug-23", format = "%d-%b-%y")
print(test_date)

# Try creating a new data frame with just a few rows to test
test_df <- data.frame(date_str = c("01-Aug-23", "02-Aug-23", "03-Aug-23"))
test_df$date_parsed <- as.Date(test_df$date_str, format = "%d-%b-%y")
print(test_df)

# Create new column for the cleaned dates
df$Clean_Survey_Date <- NA

# process strings that look like actual dates
date_pattern <- "^\\d{2}-[A-Za-z]{3}-\\d{2}$"
valid_dates <- grepl(date_pattern, df$Survey_response_Date) & nchar(df$Survey_response_Date) == 9

# Convert only the valid dates
df$Clean_Survey_Date[valid_dates] <- as.Date(df$Survey_response_Date[valid_dates], format = "%d-%b-%y")

# How many valid dates did we find?
sum(valid_dates)

# Look at the first few problematic entries
head(df$Survey_response_Date[!valid_dates])

# Count how many values are valid vs invalid - true(53133) & false(50)
table(valid_dates)

# Try to extract date patterns from longer strings
for (i in which(!valid_dates)) {
  # Look for date patterns in the text
  date_match <- regexpr("\\d{2}-[A-Za-z]{3}-\\d{2}", df$Survey_response_Date[i])
  if (date_match > 0) {
    extracted_date <- substr(df$Survey_response_Date[i], date_match, date_match + 8)
    df$Clean_Survey_Date[i] <- as.Date(extracted_date, format = "%d-%b-%y")
  }
}

# Create a new column for the cleaned dates
df$Clean_Survey_Date <- NA

# Convert only the valid dates 
date_pattern <- "^\\d{2}-[A-Za-z]{3}-\\d{2}$"
valid_dates <- grepl(date_pattern, df$Survey_response_Date) & nchar(df$Survey_response_Date) == 9
df$Clean_Survey_Date[valid_dates] <- as.Date(df$Survey_response_Date[valid_dates], format = "%d-%b-%y")

# Check the result
head(df$Clean_Survey_Date)

# class of Clean_Date column 
class(df$Clean_Survey_Date)

# date was stored in epoch format "19570"
# convert the numeric values to actual dates and save in new column while assuming those numbers are days since 1970-01-01 (the default for R)
df$survey_date <- as.Date(df$Clean_Survey_Date, origin = "1970-01-01")

# Check the result
head(df$survey_date)
```


```{r}
# cleaning channel name column
## channel name also have wildly different lengths. 
unique(df$channel_name)

# check if channel_name are the same length
channel_length <- unique(nchar(df$channel_name))
print(channel_length)

# Define the valid channel names
valid_channels <- c("Inbound", "Outcall", "Email")

# Function to extract channel names
extract_channel <- function(text) {
  if (is.na(text)) {
    return(NA)
  }
  
  # Split by commas
  parts <- strsplit(as.character(text), ",")[[1]]
  
  # Trim whitespace from each part
  parts <- trimws(parts)
  
  # Check if any part matches valid channels
  matching_channels <- parts[parts %in% valid_channels]
  
  if (length(matching_channels) > 0) {
    return(matching_channels[1])  # Return the first match if multiple exist
  } else {
    return(NA)
  }
}

# Apply the function to create a clean channel column
df$clean_channel_name <- sapply(df$channel_name, extract_channel)

# Check the results
table(df$clean_channel_name, useNA = "ifany")

# Check how many records couldn't be mapped
missing_channels <- sum(is.na(df$clean_channel_name))
percent_missing <- (missing_channels / nrow(df)) * 100
cat("Records without a valid channel name:", missing_channels, 
    "(", format(percent_missing, digits = 2), "%)", "\n")

# re-confirm if column parsed well
unique(df$clean_channel_name)
```


```{r}
# cleaning product category column
unique(df$Product_category)

# check if Product_category are the same length
Product_category_length <- unique(nchar(df$Product_category))
print(Product_category_length)

# Define the valid product category names
valid_product_category <- c("Electronics", "LifeStyle", "Books & General merchandise", "Mobile", "Home", "Furniture", "Home Appliences")

# Function to extract product category names
extract_category <- function(text) {
  if (is.na(text)) {
    return(NA)
  }
  
  # Split by commas
  parts <- strsplit(as.character(text), ",")[[1]]
  
  # Trim whitespace from each part
  parts <- trimws(parts)
  
  # Check if any part matches valid product category
  matching_category <- parts[parts %in% valid_product_category]
  
  if (length(matching_category) > 0) {
    return(matching_category[1])  # Return the first match if multiple exist
  } else {
    return(NA)
  }
}

# Apply the function to create a clean column
# Assuming your data is in a dataframe called df
df$clean_product_category <- sapply(df$Product_category, extract_category)

# Check the results
table(df$clean_product_category, useNA = "ifany")

# Check how many records couldn't be mapped
missing_product_category <- sum(is.na(df$clean_product_category))
percent_missing_product <- (missing_product_category / nrow(df)) * 100
cat("Records without a valid channel:", missing_channels, 
    "(", format(percent_missing_product, digits = 2), "%)", "\n")

# re-confirm in column parsed well
unique(df$clean_product_category)

# rename "home appliences" to "home appliances"
df$clean_product_category[df$clean_product_category == "Home Appliences"] <- "Home Appliances"
```


```{r}
# cleaning agent shift column
unique(df$Agent.Shift)

# Define the valid agent shift names
valid_agent_shifts <- c("Morning", "Afternoon", "Evening", "Split")

# Function to extract agent shifts names
extract_shift <- function(text) {
  if (is.na(text)) {
    return(NA)
  }
  
  # Split by commas
  parts <- strsplit(as.character(text), ",")[[1]]
  
  # Trim whitespace from each part
  parts <- trimws(parts)
  
  # Check if any part matches valid agent shift
  matching_shift <- parts[parts %in% valid_agent_shifts]
  
  if (length(matching_shift) > 0) {
    return(matching_shift[1])  # Return the first match if multiple exist
  } else {
    return(NA)
  }
}

# Apply the function to create a clean column
df$clean_agent_shifts <- sapply(df$Agent.Shift, extract_shift)

# Check the results
table(df$clean_agent_shifts, useNA = "ifany")

# Check how many shifts couldn't be mapped
missing_agent_shifts <- sum(is.na(df$clean_agent_shifts))
percent_missing_shift <- (missing_agent_shifts / nrow(df)) * 100
cat("Records without a valid agent shift value:", missing_agent_shifts, 
    "(", format(percent_missing_shift, digits = 2), "%)", "\n")

# re-confirm if column parsed well
unique(df$clean_agent_shifts) 
```


```{r}
# cleaning tenure bucket column
unique(df$Tenure.Bucket)

# Define the valid tenure names
valid_tenure <- c("On Job Training", ">90", "0-30", "31-60", "61-90")

# Function to extract tenure bucket names
extract_tenure <- function(text) {
  if (is.na(text)) {
    return(NA)
  }
  
  # Split by commas
  parts <- strsplit(as.character(text), ",")[[1]]
  
  # Trim whitespace from each part
  parts <- trimws(parts)
  
  # Check if any part matches valid tenure buckets
  matching_tenure <- parts[parts %in% valid_tenure]
  
  if (length(matching_tenure) > 0) {
    return(matching_tenure[1])  # Return the first match if multiple exist
  } else {
    return(NA)
  }
}

# Apply the function to create a clean column
df$clean_tenure <- sapply(df$Tenure.Bucket, extract_tenure)

# Check the results
table(df$clean_tenure, useNA = "ifany")

# Check how many shifts couldn't be mapped
missing_tenure <- sum(is.na(df$clean_tenure))
percent_missing_tenure <- (missing_tenure / nrow(df)) * 100
cat("Records without a valid tenure:", missing_tenure, 
    "(", format(percent_missing_tenure, digits = 2), "%)", "\n")

# re-confirm if column parsed well
unique(df$clean_tenure)
```


```{r}
# Convert date columns to datetime format
datetime_columns <- c("order_date_time", "Issue_reported.at", "issue_responded", "survey_date")

for (col in datetime_columns) {
  if (col %in% colnames(df)) {
    df[[col]] <- dmy_hm(df[[col]], quiet = TRUE)
  }
}

# Calculate and create response time in minutes
if ("Issue_reported.at" %in% colnames(df) && "issue_responded" %in% colnames(df)) {
  df$Response_time_min <- as.numeric(difftime(df$issue_responded, df$Issue_reported.at, units = "mins"))
}

# Calculate and create response time in hours
if ("Issue_reported.at" %in% colnames(df) && "issue_responded" %in% colnames(df)) {
  df$response_time_hr <- as.numeric(difftime(df$issue_responded, df$Issue_reported.at, units = "hours"))
}
```


## Exploratory Data Analysis and Visualization


```{r}
# Distribution of CSAT scores
CSAT <- df %>%
  ggplot(aes(x=CSAT.Score)) +
  geom_histogram(binwidth=1, fill="#69b3a2", color="#e9ecef", alpha=0.9) +
  labs(title = "Distribution of CSAT Scores", x = "CSAT Score", y = "Count") +
  theme(
    plot.title = element_text(size=15)
  )
CSAT
```


```{r}
# CSAT by channel
CSAT_channel <- df %>%
  ggplot(aes(x = clean_channel_name, y = CSAT.Score, fill = clean_channel_name)) +
  geom_boxplot(alpha = 0.7) +
  stat_summary(fun = mean, geom = "point", shape = 20, size = 4, color = "white", fill = "gray") +
  theme(legend.position = "none") +
  scale_fill_brewer(palette = "Set2") +
  labs(title = "Distribution of CSAT Scores", x = "Channel Name", y = "CSAT Score") +
  theme(
    plot.title = element_text(size = 15),
    axis.text.x = element_text(angle = 45, hjust = 1) # Rotates text
  )
CSAT_channel
```


```{r}
avg_csat <- df %>%
  group_by(clean_product_category) %>%
  summarise(avg_csat = mean(CSAT.Score, na.rm = TRUE)) %>%
  arrange(desc(avg_csat))

CSAT_product_category <- ggplot(avg_csat, aes(x = reorder(clean_product_category, -avg_csat), y = avg_csat, fill = clean_product_category)) +
  geom_bar(stat = "identity", alpha = 0.7) +
  labs(title = "Average CSAT by Product Category", x = "Product Category", y = "Average CSAT Score") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
  scale_fill_viridis_d()  
CSAT_product_category
```


```{r}
# Response time analysis
response_plot <- df %>%
  ggplot(aes(x=response_time_hr)) +
  geom_histogram(aes(y = ..density..), binwidth=30, fill="#69b3a2", color="#e9ecef", alpha=0.9) +
  labs(title = "Distribution of Response time in hours", x = "Response Time in hrs", y = "Count") +
  theme(
    plot.title = element_text(size=15)
  )
response_plot

# Clip to 95th percentile to handle outliers
response_plot_percentile <- ggplot(df, aes(x = pmin(response_time_hr, quantile(response_time_hr, 0.95, na.rm = TRUE)))) +
  geom_histogram(aes(y = ..density..), bins = 20, fill = "steelblue", alpha = 0.7) +
  #geom_density(color = "red") +
  labs(title = "Distribution of Response Time (95th percentile)", 
       x = "Response Time (hrs)", y = "Count") +
  theme_minimal()
response_plot_percentile
```


```{r}
# Response time vs CSAT
response_CSAT <- ggplot(df, aes(x = response_time_hr, y = CSAT.Score)) +
  geom_point(alpha = 0.3, color = "steelblue") +
  geom_smooth(method = "loess", color = "red") +
  labs(title = "Response Time vs CSAT Score", 
       x = "Response Time (hrs)", y = "CSAT Score") +
  theme_minimal()
response_CSAT

# 95th percentile
response_CSAT_percentile <- ggplot(df, aes(x = response_time_hr, y = CSAT.Score)) +
  geom_point(alpha = 0.3, color = "steelblue") +
  geom_smooth(method = "loess", color = "red") +
  labs(title = "Response Time vs CSAT Score", 
       x = "Response Time (hrs)", y = "CSAT Score") +
  theme_minimal() +
  coord_cartesian(xlim = c(0, quantile(df$response_time_hr, 0.95, na.rm = TRUE)))
response_CSAT_percentile
```


```{r}
# CSAT by agent tenure
CSAT_tenure <- df %>%
  ggplot(aes(x = clean_tenure, y = CSAT.Score, fill = clean_tenure)) +
  geom_boxplot(alpha = 0.7) +
  stat_summary(fun = mean, geom = "point", shape = 20, size = 4, color = "white", fill = "gray") +
  theme(legend.position = "none") +
  scale_fill_brewer(palette = "Set2") +
  labs(title = "CSAT Scores by Agent Tenure", x = "Tenure Bucket", y = "CSAT Score") +
  theme(
    plot.title = element_text(size = 15),
    axis.text.x = element_text(angle = 45, hjust = 1) # Rotates text
  )
CSAT_tenure
```


```{r}
# CSAT by shift
CSAT_shift <- df %>%
  ggplot(aes(x = clean_agent_shifts, y = CSAT.Score, fill = clean_agent_shifts)) +
  geom_boxplot(alpha = 0.7) +
  stat_summary(fun = mean, geom = "point", shape = 20, size = 4, color = "white", fill = "gray") +
  theme(legend.position = "none") +
  scale_fill_brewer(palette = "Set2") +
  labs(title = "CSAT Scores by Agent Shift", x = "Agent shift", y = "CSAT Score") +
  theme(
    plot.title = element_text(size = 15),
    axis.text.x = element_text(angle = 45, hjust = 1) # Rotates text
  )
CSAT_shift
```


```{r}
# Average handling time by channel
avg_handling <- df %>%
  group_by(clean_channel_name) %>%
  summarise(avg_handling = mean(response_time_hr, na.rm = TRUE)) %>%
  arrange(desc(avg_handling))

response_channel <- ggplot(avg_handling, aes(x = reorder(clean_channel_name, -avg_handling), y = avg_handling, fill = clean_channel_name)) +
  geom_bar(stat = "identity", alpha = 0.7) +
  labs(title = "Average Response Time by Channel", 
       x = "Channel", y = "Average Response Time (Hours)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_viridis_d() 
response_channel
```


There are some corrupted data in between results of 'Customer_City' but I won't be cleaning this column because its distinct inputs are too much to extract.


```{r}
# distinct cities in df
unique(df$Customer_City) 

# Top 10 cities by number of issues
city_counts <- df %>%
  count(Customer_City) %>%
  arrange(desc(n)) %>%
  head(10)
city_counts

cities <- ggplot(city_counts, aes(x = reorder(Customer_City, -n), y = n, fill = Customer_City)) +
  geom_bar(stat = "identity", alpha = 0.7) +
  labs(title = "Top 10 Cities by Number of Issues", 
       x = "City", y = "Number of Issues") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_viridis_d() 
cities
```


```{r}
# Agent performance analysis 
agent_perf <- df %>%
  group_by(Agent_name) %>%
  summarise(
    CSAT_Score = mean(CSAT.Score, na.rm = TRUE),
    Handling_time = mean(response_time_hr, na.rm = TRUE),
    Ticket_count = n()
  ) %>%
  arrange(desc(CSAT_Score)) %>%
  head(20) 

performance_by_CSAT <- ggplot(agent_perf, aes(x = reorder(Agent_name, -CSAT_Score), y = CSAT_Score, fill = Agent_name)) +
  geom_bar(stat = "identity", alpha = 0.7) +
  labs(title = "Top 20 Agents by CSAT Score", 
       x = "Agent", y = "Average CSAT Score") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
  scale_fill_viridis_d() 
performance_by_CSAT
```


## Feature Engineering


```{r}
# Time-based features
for (col in datetime_columns) {
  if (col %in% colnames(df)) {
    df[[paste0(col, "_hour")]] <- hour(df[[col]])
    df[[paste0(col, "_day")]] <- wday(df[[col]], label = TRUE)
    df[[paste0(col, "_month")]] <- month(df[[col]], label = TRUE)
  }
}

# Load required libraries
library(tidytext)
library(textdata)
library(wordcloud)
library(syuzhet)
library(tm)
```


### Sentiment Analysis


```{r}
# Customer sentiment analysis
# check if customer remarks column is clean
unique(df$Customer.Remarks)

# Initial preprocessing - calculate length of remarks
df$Remarks_length <- nchar(as.character(df$Customer.Remarks))

# Check for missing values
sum(is.na(df$Customer.Remarks))

# Function to clean text
clean_text <- function(text) {
  text <- tolower(text)
  text <- removePunctuation(text)
  text <- removeNumbers(text)
  text <- stripWhitespace(text)
  text <- removeWords(text, stopwords("english"))
  return(text)
}

# Apply text cleaning
df$Clean_Remarks <- sapply(df$Customer.Remarks, clean_text)
```


```{r}
# Tokenization - break text into individual words
remarks_tokens <- df %>%
  select(Unique.id, Clean_Remarks) %>%
  unnest_tokens(word, Clean_Remarks)

# Remove stop words
remarks_tokens <- remarks_tokens %>%
  anti_join(stop_words)

# Word frequency analysis
word_counts <- remarks_tokens %>%
  count(word, sort = TRUE)

# Plot top 20 most frequent words
top_words_plot <- word_counts %>%
  slice_max(n, n = 20) %>%
  ggplot(aes(x = reorder(word, n), y = n, fill = word)) +
  geom_col() +
  coord_flip() +
  labs(x = "Word", y = "Frequency", title = "Top 20 Most Frequent Words in Customer Remarks") +
  scale_fill_viridis_d() 
  
top_words_plot
```


```{r}
# Sentiment analysis using multiple lexicons
# 1. Using AFINN lexicon
afinn_sentiment <- remarks_tokens %>%
  inner_join(get_sentiments("afinn")) %>%
  group_by(Unique.id) %>%
  summarize(sentiment_score = sum(value)) %>%
  ungroup()

# 2. Using Bing lexicon (positive/negative)
bing_sentiment <- remarks_tokens %>%
  inner_join(get_sentiments("bing")) %>%
  count(Unique.id, sentiment) %>%
  pivot_wider(names_from = sentiment, values_from = n, values_fill = 0) %>%
  mutate(sentiment_ratio = (positive - negative)/(positive + negative + 0.001))

# 3. Using NRC lexicon (emotions)
nrc_sentiment <- remarks_tokens %>%
  inner_join(get_sentiments("nrc")) %>%
  count(Unique.id, sentiment) %>%
  pivot_wider(names_from = sentiment, values_from = n, values_fill = 0)

# 4. Using syuzhet package for more nuanced sentiment
syuzhet_scores <- data.frame(Unique.id = df$Unique.id, 
                             syuzhet_score = get_sentiment(df$Customer.Remarks, method = "syuzhet"))
```


### Emotion Analysis 


```{r}
# Direct emotion analysis using the syuzhet package
emotion_scores <- get_nrc_sentiment(df$Customer.Remarks)

# Add the Unique.id to the emotion scores
emotion_scores <- cbind(Unique.id = df$Unique.id, emotion_scores)

# Join with the syuzhet emotion scores
initial_analysis <- syuzhet_scores %>%
  left_join(emotion_scores, by = "Unique.id")
initial_analysis

# Topic modeling using Latent Dirichlet Allocation (LDA)
#install.packages("topicmodels")
library(topicmodels)

# Combine all sentiment analyses
df$Remarks_length <- nchar(as.character(df$Customer.Remarks))
sentiment_combined <- df %>%
  select(Unique.id, Customer.Remarks, Remarks_length) %>%
  left_join(afinn_sentiment, by = "Unique.id") %>%
  left_join(bing_sentiment, by = "Unique.id") %>%
  left_join(nrc_sentiment, by = "Unique.id") %>%
  left_join(syuzhet_scores, by = "Unique.id")


# Create document-term matrix
dtm <- remarks_tokens %>%
  count(Unique.id, word) %>%
  cast_dtm(Unique.id, word, n)

# Determine optimal number of topics
#install.packages("ldatuning")
library(ldatuning)
k_range <- seq(2, 20, by = 1)
topics_fit <- FindTopicsNumber(dtm, topics = k_range, metrics = c("Griffiths2004", "CaoJuan2009", "Arun2010"), method = "Gibbs")

# Plot topic number evaluation
topics_plot <- FindTopicsNumber_plot(topics_fit)

# Set number of topics (adjust based on result of topic number evaluation)
num_topics <- 10  # Adjust as needed

# Fit LDA model
lda_model <- LDA(dtm, k = num_topics, method = "Gibbs", 
                 control = list(seed = 1234, burnin = 1000, iter = 2000, thin = 100))

# Extract topic-word probabilities
topic_terms <- tidy(lda_model, matrix = "beta")

# Get top terms for each topic
top_terms <- topic_terms %>%
  group_by(topic) %>%
  slice_max(beta, n = 10) %>%
  ungroup() %>%
  arrange(topic, -beta)
top_terms

# Visualize topics
topic_plot <- top_terms %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(beta, term, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  scale_y_reordered() +
  labs(title = "Top Terms in Each Topic")
topic_plot

# Assign topics to each document
document_topics <- tidy(lda_model, matrix = "gamma")

# Get primary topic for each document
primary_topics <- document_topics %>%
  group_by(document) %>%
  slice_max(gamma, n = 1) %>%
  ungroup() %>%
  rename(Unique.id = document, primary_topic = topic, topic_probability = gamma)

# Combine topic modeling with sentiment analysis
final_analysis <- sentiment_combined %>%
  left_join(primary_topics, by = "Unique.id")

# Check for duplicates
sum(duplicated(df$Unique.id))

# Check what columns are in final_analysis dataframe
colnames(final_analysis)

# Check emotion score columns
colnames(emotion_scores)

# Create summary statistics
sentiment_summary <- final_analysis %>%
  group_by(primary_topic) %>%
  summarize(
    avg_sentiment = mean(sentiment_score, na.rm = TRUE),
    avg_positive_x = mean(positive.x, na.rm = TRUE),
    avg_negative_x = mean(negative.x, na.rm = TRUE),
    avg_positive_y = mean(positive.y, na.rm = TRUE),
    avg_negative_y = mean(negative.y, na.rm = TRUE),
    avg_length = mean(Remarks_length, na.rm = TRUE),
    count = n()
  ) %>%
  ungroup()
sentiment_summary
```


### Sentiment & Emotion Plots


```{r}
# Create visualizations
# 1. Overall sentiment distribution
sentiment_dist_plot <- ggplot(final_analysis, aes(x = sentiment_score)) +
  geom_histogram(binwidth = 1) +
  labs(title = "Distribution of Customer Sentiment Scores",
       x = "Sentiment Score (AFINN)",
       y = "Count") +
  scale_fill_brewer(palette = "Set5")
sentiment_dist_plot

# 2. Sentiment by topic
sentiment_by_topic_plot <- ggplot(final_analysis, aes(x = factor(primary_topic), y = sentiment_score, fill = factor(primary_topic))) +
  geom_boxplot() +
  scale_fill_brewer(palette = "Set2") +
  labs(title = "Sentiment Distribution by Topic",
       x = "Topic",
       y = "Sentiment Score (AFINN)")
sentiment_by_topic_plot

# 3. Emotion distribution
emotion_columns <- c("anger", "anticipation", "disgust", "fear", "joy", "sadness", "surprise", "trust")
emotion_data <- final_analysis %>%
  select(Unique.id, all_of(emotion_columns)) %>%
  pivot_longer(cols = all_of(emotion_columns), names_to = "emotion", values_to = "score") %>%
  filter(!is.na(score))  %>% # Remove any remaining NA values 
  mutate(emotion = gsub("\\.y$", "", emotion))  # Remove the .y suffix for plotting
emotion_data

#emotion plot
emotion_plot <- ggplot(emotion_data, aes(x = emotion, y = score, fill = emotion)) +
  geom_boxplot() +
  labs(title = "Distribution of Emotions in Customer Remarks",
       x = "Emotion",
       y = "Score") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
emotion_plot

# 4. Word cloud of most common words
set.seed(1234)
wordcloud_plot <- wordcloud(words = word_counts$word,
                            freq = word_counts$n,
                            min.freq = 2,
                            max.words = 100,
                            random.order = FALSE,
                            colors = brewer.pal(8, "Dark2"))
```


## Machine Learning


### Cleaning Corrupted Columns


```{r}
# checking to see category columns imported properly
unique(df$category)
```


```{r}
# checking to see sub category columns imported properly
unique(df$Sub.category)
```


Both category and sub category columns are corrupted. 


```{r}
# cleaning category column
# Define the valid channel names
valid_category <- c("Product Queries", "Order Related", "Returns",  "Cancellation",  
                     "Shopzilla Related", "Payments related", "Refund Related", "Feedback", "Others", 
                    "App/website", "Offers & Cashback", "Onboarding related")

# Function to extract channel names
extract_category <- function(text) {
  if (is.na(text)) {
    return(NA)
  }
  
  # Split by commas
  parts <- strsplit(as.character(text), ",")[[1]]
  
  # Trim whitespace from each part
  parts <- trimws(parts)
  
  # Check if any part matches valid channels
  matching_category <- parts[parts %in% valid_category]
  
  if (length(matching_category) > 0) {
    return(matching_category[1])  # Return the first match if multiple exist
  } else {
    return(NA)
  }
}

# Apply the function to create a clean channel column
df$clean_category <- sapply(df$category, extract_category)

# Check the results
table(df$clean_category, useNA = "ifany")

# Check how many records couldn't be mapped
missing_category <- sum(is.na(df$clean_category))
percent_missing_category <- (missing_category / nrow(df)) * 100
cat("Records without a valid category:", missing_category, 
    "(", format(percent_missing_category, digits = 2), "%)", "\n")

# re-confirm if column parsed well
unique(df$clean_category)
```


```{r}
# cleaning sub category column
# Define the valid sub category names
valid_sub_category <- c("Life Insurance", "Product Specific Information", "Installation/demo", 
                        "Reverse Pickup Enquiry", "Not Needed", "Fraudulent User", "Product Specific Information", 
                        "Exchange / Replacement", "Missing", "General Enquiry", "Return request", "Delayed", 
                        "Service Centres Related", "Order status enquiry", "Return cancellation", "Unable to track", 
                        "Seller Cancelled Order", "Wrong", "Priority delivery", "Refund Related Issues", "Signup Issues", 
                        "Invoice request", "Online Payment Issues", "Technician Visit", "UnProfessional Behaviour", "Damaged", 
                        "Call back request", "Billing Related", "Payment related Queries", "Wrong", "Shopzilla Rewards", "COD Refund Details", 
                        "Wallet related", "PayLater related", "Customer Requested Modifications", "Refund Enquiry", "Account updation", 
                        "Priority delivery", "Issues with Shopzilla App", "Other Cashback", "Shopzila Premium Related", "Seller onboarding", 
                        "Affiliate Offers", "Service Center - Service Denial", "Product related Issues", "Instant discount", "Other Account Related Issues",
                        "Card/EMI", "Order Verification", "Call disconnected", "App/website Related", "Warranty related", "Others", "e-Gift Voucher",
                        "Unable to Login", "Non Order related", "Self-Help", "Payment pending", "Commission related", "Policy Related")

# Function to extract sub category names
extract_sub_category <- function(text) {
  if (is.na(text)) {
    return(NA)
  }
  
  # Split by commas
  parts <- strsplit(as.character(text), ",")[[1]]
  
  # Trim whitespace from each part
  parts <- trimws(parts)
  
  # Check if any part matches valid sub categories
  matching_sub_category <- parts[parts %in% valid_sub_category]
  
  if (length(matching_sub_category) > 0) {
    return(matching_sub_category[1])  # Return the first match if multiples exist
  } else {
    return(NA)
  }
}

# Apply the function to create a clean sub category column
df$clean_sub_category <- sapply(df$Sub.category, extract_sub_category)

# Check the results
table(df$clean_sub_category, useNA = "ifany")

# Check how many records couldn't be mapped
missing_sub_category <- sum(is.na(df$clean_sub_category))
percent_missing_sub_category <- (missing_sub_category / nrow(df)) * 100
cat("Records without a sub category:", missing_sub_category, 
    "(", format(percent_missing_sub_category, digits = 2), "%)", "\n")

# re-confirm if column parsed well
unique(df$clean_sub_category)
```


### CSAT Score Prediction


```{r}
# Select features
categorical_features <- c("clean_channel_name", "clean_category", "clean_sub_category", "clean_product_category", 
                          "Customer_City", "clean_tenure", "clean_agent_shifts")
numeric_features <- c("Item_price", "Response_time_min", 
                      "Remarks_length")

# Add time-based features
for (col in datetime_columns) {
  hour_col <- paste0(col, "_hour")
  day_col <- paste0(col, "_day")
  if (hour_col %in% colnames(df)) {
    numeric_features <- c(numeric_features, hour_col)
  }
  if (day_col %in% colnames(df)) {
    categorical_features <- c(categorical_features, day_col)
  }
}

# Remove any columns that don't exist in the dataset
categorical_features <- categorical_features[categorical_features %in% colnames(df)]
numeric_features <- numeric_features[numeric_features %in% colnames(df)]
```


```{r}
library(caret)
library(recipes)
library(randomForest)
library(pROC)
library(gridExtra)

# Define target variable - let's predict if CSAT is high (≥4) or not
df$High_CSAT <- as.factor(ifelse(df$CSAT.Score >= 4, "Yes", "No"))

# Create feature set and target
features <- c(categorical_features, numeric_features)
X <- df[, features]
y <- df$High_CSAT

# Check for missing values
missing_counts <- sapply(X, function(x) sum(is.na(x)))
print("Missing values per feature:")
print(missing_counts)
print("Percentage of missing values:")
print(100 * missing_counts / nrow(X))

# Split data
set.seed(42)
train_indices <- createDataPartition(y, p = 0.75, list = FALSE)
X_train <- X[train_indices, ]
X_test <- X[-train_indices, ]
y_train <- y[train_indices]
y_test <- y[-train_indices]

# Create preprocessing recipe with imputation
preprocessing_recipe <- recipe(~ ., data = X_train) %>%
  # Impute missing values for numeric features with mean
  step_impute_mean(all_of(numeric_features)) %>%
  # Impute missing values for categorical features with mode
  step_impute_mode(all_of(categorical_features)) %>%
  # Normalize numeric features
  step_normalize(all_of(numeric_features)) %>%
  # One-hot encode categorical features
  step_dummy(all_of(categorical_features), one_hot = TRUE)

# Prepare the preprocessing
prepped_recipe <- prep(preprocessing_recipe)

# Transform the training and test data
X_train_processed <- bake(prepped_recipe, new_data = X_train)
X_test_processed <- bake(prepped_recipe, new_data = X_test)


na_count <- sum(is.na(X_train_processed))
print(paste("Number of NA values in processed training data:", na_count))

X_train_processed <- X_train_processed %>% 
  mutate(across(everything(), ~ifelse(is.na(.), mean(., na.rm = TRUE), .)))

X_train_processed <- na.roughfix(X_train_processed)
X_test_processed <- na.roughfix(X_test_processed)


###### 
# First, let's do a complete check of the processed data
print("Column-wise NA count in processed training data:")
na_cols <- colSums(is.na(X_train_processed))
print(na_cols[na_cols > 0])  # Show only columns with NAs

# Complete manual imputation for any remaining NAs
# This approach imputes each column with its own mean or 0 if all values are NA
X_train_clean <- as.data.frame(X_train_processed)  # Convert to dataframe if it's a tibble
for(col in colnames(X_train_clean)) {
  if(sum(is.na(X_train_clean[[col]])) > 0) {
    # If column has NAs but also some non-NA values
    if(sum(!is.na(X_train_clean[[col]])) > 0) {
      col_mean <- mean(X_train_clean[[col]], na.rm = TRUE)
      X_train_clean[[col]][is.na(X_train_clean[[col]])] <- col_mean
    } else {
      # If column is all NAs, replace with 0
      X_train_clean[[col]][is.na(X_train_clean[[col]])] <- 0
    }
  }
}

# Do the same for test data
X_test_clean <- as.data.frame(X_test_processed)
for(col in colnames(X_test_clean)) {
  if(sum(is.na(X_test_clean[[col]])) > 0) {
    if(sum(!is.na(X_test_clean[[col]])) > 0) {
      col_mean <- mean(X_test_clean[[col]], na.rm = TRUE)
      X_test_clean[[col]][is.na(X_test_clean[[col]])] <- col_mean
    } else {
      X_test_clean[[col]][is.na(X_test_clean[[col]])] <- 0
    }
  }
}

# Verify we have no more NAs
print(paste("NAs remaining in training data:", sum(is.na(X_train_clean))))
print(paste("NAs remaining in test data:", sum(is.na(X_test_clean))))

######
# Check for NAs in the target variable
print(paste("NAs in y_train:", sum(is.na(y_train))))

# Remove NAs from both features and target variables together
complete_indices <- complete.cases(X_train_clean) & !is.na(y_train)
X_train_complete <- X_train_clean[complete_indices, ]
y_train_complete <- y_train[complete_indices]

# Verify dimensions and NA counts
print(paste("Original row count:", nrow(X_train_clean)))
print(paste("Complete row count:", nrow(X_train_complete)))
print(paste("NAs in X_train_complete:", sum(is.na(X_train_complete))))
print(paste("NAs in y_train_complete:", sum(is.na(y_train_complete))))

# Train the model with fully clean data
set.seed(42)
rf_model <- randomForest(
  x = X_train_complete,
  y = y_train_complete,
  ntree = 100
)

# For prediction, make sure test data is also clean
complete_test_indices <- complete.cases(X_test_clean) & !is.na(y_test)
X_test_complete <- X_test_clean[complete_test_indices, ]
y_test_complete <- y_test[complete_test_indices]

# Get raw probabilities from the random forest
y_pred_prob_rf <- predict(rf_model, X_test_complete, type = "prob")[, 2]

# Make predictions
y_pred <- factor(ifelse(y_pred_prob_rf > 0.5, "Yes", "No"), levels = levels(y_test_complete))

# Evaluate
confusion_matrix <- confusionMatrix(y_pred, y_test_complete)
print("Confusion Matrix:")
print(confusion_matrix)

# Calculate ROC curve and AUC for Random Forest
roc_rf <- roc(y_test_complete, y_pred_prob_rf)
auc_rf <- as.numeric(auc(roc_rf))
print(paste("Random Forest AUC:", round(auc_rf, 4)))

# Random Forest Metrics
accuracy_rf <- confusion_matrix$overall["Accuracy"]         # Accuracy
precision_rf <- confusion_matrix$byClass["Pos Pred Value"]  # Precision
recall_rf <- confusion_matrix$byClass["Sensitivity"]        # Recall
f1_rf <- confusion_matrix$byClass["F1"]                     # F1 Score

print("Random Forest Metrics:")
print(paste("Precision:", round(precision_rf, 4)))
print(paste("Recall:", round(recall_rf, 4)))
print(paste("F1 Score:", round(f1_rf, 4)))
```


### Logistic Regression Model 


```{r}
# Train a logistic regression model
lr_model <- glm(y_train_complete ~ ., data = X_train_complete, family = "binomial")

# Summary of the logistic regression model
lr_summary <- summary(lr_model)
lr_summary

# Make predictions with logistic regression
y_pred_prob_lr <- predict(lr_model, X_test_complete, type = "response")
y_pred_lr <- factor(ifelse(y_pred_prob_lr > 0.5, "Yes", "No"), levels = levels(y_test_complete))

# Confusion matrix for logistic regression
confusion_matrix_lr <- confusionMatrix(y_pred_lr, y_test_complete)
print("Logistic Regression Confusion Matrix:")
print(confusion_matrix_lr)

# Logistic Regression Metrics
accuracy_lr <- confusion_matrix_lr$overall["Accuracy"]
precision_lr <- confusion_matrix_lr$byClass["Pos Pred Value"]
recall_lr <- confusion_matrix_lr$byClass["Sensitivity"]
f1_lr <- confusion_matrix_lr$byClass["F1"]

print("Logistic Regression Metrics:")
print(paste("Precision:", round(precision_lr, 4)))
print(paste("Recall:", round(recall_lr, 4)))
print(paste("F1 Score:", round(f1_lr, 4)))

# ROC curve for logistic regression
roc_lr <- roc(y_test_complete, y_pred_prob_lr)
auc_lr <- as.numeric(auc(roc_lr))
print(paste("Logistic Regression AUC:", round(auc_lr, 4)))
```


```{r}
# ----- Visualizations -----

# 1. Plot ROC curves for both models
roc_plot <- ggroc(list(RandomForest = roc_rf, LogisticRegression = roc_lr)) +
  geom_abline(slope = 1, intercept = 1, linetype = "dashed", color = "gray") +
  labs(
    title = "ROC Curves Comparison",
    subtitle = paste(
      "AUC (RF):", round(auc_rf, 3),
      "  AUC (LR):", round(auc_lr, 3)
    ),
    color = "Model"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")
roc_plot

# 2. Variable Importance Plot for Random Forest
var_importance <- as.data.frame(importance(rf_model))
var_importance$Feature <- rownames(var_importance)
var_importance <- var_importance %>%
  arrange(desc(MeanDecreaseGini)) %>%
  head(20)  # Top 20 features

importance_plot <- ggplot(var_importance, aes(x = reorder(Feature, MeanDecreaseGini), y = MeanDecreaseGini)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +
  labs(
    title = "Random Forest Variable Importance",
    x = "Feature",
    y = "Importance (Mean Decrease in Gini)"
  ) +
  theme_minimal()
importance_plot

# 3. Calibration Plot (Predicted vs. Actual)
# Create bins of predicted probabilities
bins <- 10
probabilities_rf <- data.frame(
  predicted_prob = y_pred_prob_rf,
  actual = as.numeric(y_test_complete)
)

probabilities_lr <- data.frame(
  predicted_prob = y_pred_prob_lr,
  actual = as.numeric(y_test_complete)
)

# Function to bin probabilities and compute observed rates
bin_probabilities <- function(prob_df, bins) {
  prob_df$bin <- cut(prob_df$predicted_prob, breaks = seq(0, 1, length.out = bins + 1), include.lowest = TRUE)
  
  binned <- prob_df %>%
    group_by(bin) %>%
    summarize(
      count = n(),
      mean_predicted = mean(predicted_prob),
      mean_actual = mean(actual)
    )
  
  return(binned)
}

binned_rf <- bin_probabilities(probabilities_rf, bins)
binned_lr <- bin_probabilities(probabilities_lr, bins)

# Add model identifier
binned_rf$model <- "Random Forest"
binned_lr$model <- "Logistic Regression"

# Combine for plotting
binned_combined <- rbind(binned_rf, binned_lr)

# Create the calibration plot
calibration_plot <- ggplot() +
  geom_point(data = binned_combined, aes(x = mean_predicted, y = mean_actual, color = model, size = count)) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "gray") +
  geom_line(data = binned_combined, aes(x = mean_predicted, y = mean_actual, color = model, group = model)) +
  labs(
    title = "Calibration Plot - Predicted vs. Actual Probabilities",
    x = "Mean Predicted Probability",
    y = "Observed Frequency",
    size = "Sample Size"
  ) +
  scale_color_manual(values = c("Random Forest" = "blue", "Logistic Regression" = "red")) +
  xlim(0, 1) +
  ylim(0, 1) +
  theme_minimal() +
  theme(legend.position = "bottom")
calibration_plot
```


### Precision Recall Curve


```{r}
# Calculate precision and recall at various thresholds for RF
pr_rf <- data.frame(
  threshold = seq(0, 1, by = 0.01)
)

pr_rf <- pr_rf %>%
  rowwise() %>%
  mutate(
    pred = list(ifelse(y_pred_prob_rf > threshold, TRUE, FALSE)),
    conf_matrix = list(table(Predicted = unlist(pred), Actual = y_test_complete)),
    precision = ifelse(sum(unlist(conf_matrix)[c(2, 4)]) > 0, 
                       unlist(conf_matrix)[4] / sum(unlist(conf_matrix)[c(2, 4)]), 
                       1),
    recall = ifelse(sum(unlist(conf_matrix)[c(3, 4)]) > 0,
                    unlist(conf_matrix)[4] / sum(unlist(conf_matrix)[c(3, 4)]),
                    0)
  ) %>%
  select(threshold, precision, recall) %>%
  mutate(model = "Random Forest")

# Calculate precision and recall at various thresholds for LR
pr_lr <- data.frame(
  threshold = seq(0, 1, by = 0.01)
)

pr_lr <- pr_lr %>%
  rowwise() %>%
  mutate(
    pred = list(ifelse(y_pred_prob_lr > threshold, TRUE, FALSE)),
    conf_matrix = list(table(Predicted = unlist(pred), Actual = y_test_complete)),
    precision = ifelse(sum(unlist(conf_matrix)[c(2, 4)]) > 0, 
                       unlist(conf_matrix)[4] / sum(unlist(conf_matrix)[c(2, 4)]), 
                       1),
    recall = ifelse(sum(unlist(conf_matrix)[c(3, 4)]) > 0,
                    unlist(conf_matrix)[4] / sum(unlist(conf_matrix)[c(3, 4)]),
                    0)
  ) %>%
  select(threshold, precision, recall) %>%
  mutate(model = "Logistic Regression")

# Combine for plotting
pr_combined <- rbind(pr_rf, pr_lr)

# Create the precision-recall curve
pr_plot <- ggplot(pr_combined, aes(x = recall, y = precision, color = model, group = model)) +
  geom_line() +
  labs(
    title = "Precision-Recall Curve",
    x = "Recall",
    y = "Precision",
    color = "Model"
  ) +
  xlim(0, 1) +
  ylim(0, 1) +
  theme_minimal() +
  theme(legend.position = "bottom")
pr_plot
```


### Threshold Optimization 


```{r}
# Find optimal threshold for F1 score for RF
thresholds <- seq(0.1, 0.9, by = 0.05)
f1_scores_rf <- numeric(length(thresholds))

for (i in seq_along(thresholds)) {
  threshold <- thresholds[i]
  y_pred_threshold <- ifelse(y_pred_prob_rf > threshold, TRUE, FALSE)
  confusion <- table(Predicted = y_pred_threshold, Actual = y_test_complete)
  
  # Ensure the confusion matrix contains both classes
  if (all(c(TRUE, FALSE) %in% rownames(confusion)) && 
      all(c(TRUE, FALSE) %in% colnames(confusion))) {
    
    precision <- confusion["TRUE", "TRUE"] / sum(confusion["TRUE", ])
    recall <- confusion["TRUE", "TRUE"] / sum(confusion[, "TRUE"])
    f1_scores_rf[i] <- 2 * (precision * recall) / (precision + recall)
  } else {
    f1_scores_rf[i] <- 0
  }
}

optimal_threshold_index_rf <- which.max(f1_scores_rf)
optimal_threshold_rf <- thresholds[optimal_threshold_index_rf]
optimal_f1_rf <- f1_scores_rf[optimal_threshold_index_rf]

print(paste("Optimal threshold for RF:", optimal_threshold_rf, "with F1 score:", round(optimal_f1_rf, 4)))

# Apply optimal threshold
y_pred_optimal_rf <- ifelse(y_pred_prob_rf > optimal_threshold_rf, TRUE, FALSE)
conf_matrix_optimal_rf <- table(Predicted = y_pred_optimal_rf, Actual = y_test_complete)
print("Confusion Matrix with Optimal Threshold (RF):")
print(conf_matrix_optimal_rf)
```


### Cross_Validation of Models


```{r}
# Function to evaluate model with k-fold cross-validation
cv_model_evaluation <- function(X, y, k = 5) {
  set.seed(42)
  folds <- createFolds(y, k = k, list = TRUE, returnTrain = FALSE)
  
  # Initialize metrics storage
  metrics <- data.frame(
    fold = integer(),
    model = character(),
    accuracy = numeric(),
    precision = numeric(),
    recall = numeric(),
    f1 = numeric(),
    auc = numeric()
  )
  
  for (i in 1:k) {
    # Split data
    test_indices <- folds[[i]]
    train_indices <- setdiff(1:length(y), test_indices)
    
    X_train_fold <- X[train_indices, ]
    y_train_fold <- y[train_indices]
    X_test_fold <- X[test_indices, ]
    y_test_fold <- y[test_indices]
    
    # Train Random Forest
    rf_model_fold <- randomForest(
      x = X_train_fold,
      y = y_train_fold,
      ntree = 50  # Reduced for speed
    )
    
    # Train Logistic Regression
    lr_model_fold <- glm(y_train_fold ~ ., data = X_train_fold, family = "binomial")
    
    # Get predictions
    y_pred_rf <- predict(rf_model_fold, X_test_fold)
    y_pred_prob_rf <- predict(rf_model_fold, X_test_fold, type = "prob")[, 2]
    
    y_pred_prob_lr <- predict(lr_model_fold, X_test_fold, type = "response")
    y_pred_lr <- ifelse(y_pred_prob_lr > 0.5, TRUE, FALSE)
    
    # Calculate metrics for RF
    conf_matrix_rf <- table(Predicted = y_pred_rf, Actual = y_test_fold)
    if (all(dim(conf_matrix_rf) == c(2, 2))) {
      accuracy_rf <- sum(diag(conf_matrix_rf)) / sum(conf_matrix_rf)
      precision_rf <- conf_matrix_rf[2, 2] / sum(conf_matrix_rf[2, ])
      recall_rf <- conf_matrix_rf[2, 2] / sum(conf_matrix_rf[, 2])
      f1_rf <- 2 * (precision_rf * recall_rf) / (precision_rf + recall_rf)
      roc_obj_rf <- roc(y_test_fold, y_pred_prob_rf)
      auc_rf <- auc(roc_obj_rf)
    } else {
      # Handle case where confusion matrix doesn't have all classes
      accuracy_rf <- mean(y_pred_rf == y_test_fold)
      precision_rf <- recall_rf <- f1_rf <- auc_rf <- NA
    }
    
    # Calculate metrics for LR
    conf_matrix_lr <- table(Predicted = y_pred_lr, Actual = y_test_fold)
    if (all(dim(conf_matrix_lr) == c(2, 2))) {
      accuracy_lr <- sum(diag(conf_matrix_lr)) / sum(conf_matrix_lr)
      precision_lr <- conf_matrix_lr[2, 2] / sum(conf_matrix_lr[2, ])
      recall_lr <- conf_matrix_lr[2, 2] / sum(conf_matrix_lr[, 2])
      f1_lr <- 2 * (precision_lr * recall_lr) / (precision_lr + recall_lr)
      roc_obj_lr <- roc(y_test_fold, y_pred_prob_lr)
      auc_lr <- auc(roc_obj_lr)
    } else {
      # Handle case where confusion matrix doesn't have all classes
      accuracy_lr <- mean(y_pred_lr == y_test_fold)
      precision_lr <- recall_lr <- f1_lr <- auc_lr <- NA
    }
    
    # Store results
    metrics <- rbind(metrics, 
                     data.frame(fold = i, model = "Random Forest", 
                                accuracy = accuracy_rf, precision = precision_rf, 
                                recall = recall_rf, f1 = f1_rf, auc = auc_rf),
                     data.frame(fold = i, model = "Logistic Regression", 
                                accuracy = accuracy_lr, precision = precision_lr, 
                                recall = recall_lr, f1 = f1_lr, auc = auc_lr))
  }
  
  # Summarize results
  summary_metrics <- metrics %>%
    group_by(model) %>%
    summarize(
      mean_accuracy = mean(accuracy, na.rm = TRUE),
      mean_precision = mean(precision, na.rm = TRUE),
      mean_recall = mean(recall, na.rm = TRUE),
      mean_f1 = mean(f1, na.rm = TRUE),
      mean_auc = mean(auc, na.rm = TRUE),
      sd_accuracy = sd(accuracy, na.rm = TRUE),
      sd_precision = sd(precision, na.rm = TRUE),
      sd_recall = sd(recall, na.rm = TRUE),
      sd_f1 = sd(f1, na.rm = TRUE),
      sd_auc = sd(auc, na.rm = TRUE)
    )
  
  return(list(fold_metrics = metrics, summary = summary_metrics))
}
# Run cross-validation
cv_results <- cv_model_evaluation(X_train_complete, y_train_complete, k = 5)

# Print cross-validation results
print("Cross-Validation Results:")
print(cv_results$summary)
```


### Final Model Comparison and Reporting 


```{r}
# Extract cross-validation results safely
mean_acc_rf_cv <- cv_results$summary %>% filter(model == "Random Forest") %>% pull(mean_accuracy)
mean_acc_lr_cv <- cv_results$summary %>% filter(model == "Logistic Regression") %>% pull(mean_accuracy)

mean_prec_rf_cv <- cv_results$summary %>% filter(model == "Random Forest") %>% pull(mean_precision)
mean_prec_lr_cv <- cv_results$summary %>% filter(model == "Logistic Regression") %>% pull(mean_precision)

mean_recall_rf_cv <- cv_results$summary %>% filter(model == "Random Forest") %>% pull(mean_recall)
mean_recall_lr_cv <- cv_results$summary %>% filter(model == "Logistic Regression") %>% pull(mean_recall)

mean_f1_rf_cv <- cv_results$summary %>% filter(model == "Random Forest") %>% pull(mean_f1)
mean_f1_lr_cv <- cv_results$summary %>% filter(model == "Logistic Regression") %>% pull(mean_f1)

mean_auc_rf_cv <- cv_results$summary %>% filter(model == "Random Forest") %>% pull(mean_auc)
mean_auc_lr_cv <- cv_results$summary %>% filter(model == "Logistic Regression") %>% pull(mean_auc)

# Create a data frame for model comparison
model_comparison <- data.frame(
  Model = c("Random Forest", "Logistic Regression", 
            "Random Forest (CV)", "Logistic Regression (CV)"),
  Accuracy = c(accuracy_rf, accuracy_lr, mean_acc_rf_cv, mean_acc_lr_cv),
  Precision = c(precision_rf, precision_lr, mean_prec_rf_cv, mean_prec_lr_cv),
  Recall = c(recall_rf, recall_lr, mean_recall_rf_cv, mean_recall_lr_cv),
  F1_Score = c(f1_rf, f1_lr, mean_f1_rf_cv, mean_f1_lr_cv),
  AUC = c(auc_rf, auc_lr, mean_auc_rf_cv, mean_auc_lr_cv)
)

# Print the comparison table
print("Model Comparison:")
print(model_comparison)

# ----- Save the best model -----

# Determine best model based on AUC
if (auc_rf > auc_lr) {
  best_model <- rf_model
  best_model_name <- "Random Forest"
} else {
  best_model <- lr_model
  best_model_name <- "Logistic Regression"
}

# Save the best model
#saveRDS(best_model, paste0("best_model_", best_model_name, ".rds"))
#print(paste("Best model saved:", best_model_name))

# Create a comprehensive comparison plot
# Reshape data for plotting
library(reshape2)
metrics_long <- reshape2::melt(model_comparison, id.vars = "Model", 
                               variable.name = "Metric", value.name = "Value")

# Plot comparison
comparison_plot <- ggplot(metrics_long, aes(x = Model, y = Value, fill = Model)) +
  geom_bar(stat = "identity") +
  facet_wrap(~ Metric, scales = "free_y") +
  labs(title = "Model Performance Comparison", x = NULL) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none")
comparison_plot
```


## Customer Behavior Analysis


```{r}
# Group by customer city and analyze patterns
city_analysis <- df %>%
  group_by(Customer_City) %>%
  summarise(
    Ticket_count = n(),
    Avg_CSAT = mean(CSAT.Score, na.rm = TRUE),
    Avg_Response_Time = mean(response_time_hr, na.rm = TRUE)
  ) %>%
  arrange(desc(Ticket_count)) %>%
  head(10)

print("Top 10 Cities Analysis:")
print(city_analysis)

# Product category analysis
product_analysis <- df %>%
  group_by(clean_product_category) %>%
  summarise(
    Ticket_count = n(),
    Avg_CSAT = mean(CSAT.Score, na.rm = TRUE),
    Avg_Response_Time = mean(response_time_hr, na.rm = TRUE),
    Avg_Item_Price = mean(Item_price, na.rm = TRUE)
  ) %>%
  arrange(desc(Ticket_count))

print("Product Category Analysis:")
print(product_analysis)

# Visualize relationship between item price and CSAT
price_CSAT <- ggplot(df, aes(x = Item_price, y = CSAT.Score)) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = "loess") +
  labs(title = "Item Price vs CSAT Score", 
       x = "Item Price", y = "CSAT Score") +
  theme_minimal() +
  scale_x_continuous(labels = scales::dollar) +
  xlim(0, quantile(df$Item_price, 0.95, na.rm = TRUE))  # Remove outliers for better visualization
price_CSAT
```


## Time-Based Analysis


```{r}
# Create day of week and hour features if not already done
if ("Issue_reported.at" %in% colnames(df)) {
  df$Day_of_week <- wday(df$Issue_reported.at, label = TRUE)
  df$Hour_of_day <- hour(df$Issue_reported.at)
  
  # CSAT by day of week
  day_csat <- df %>%
    group_by(Day_of_week) %>%
    summarise(Avg_CSAT = mean(CSAT.Score, na.rm = TRUE))
  
  CSAT_weekday <- ggplot(day_csat, aes(x = Day_of_week, y = Avg_CSAT, group = 1)) +
    geom_bar(stat = "identity", fill = "steelblue", alpha = 0.7) +
    geom_text(aes(label = round(Avg_CSAT, 2)), vjust = -0.5, size = 3) +
    labs(title = "Average CSAT Score by Day of Week", 
         x = "Day of Week", y = "Average CSAT Score") +
    theme_minimal() +
    scale_fill_brewer(palette = "Set3")
  CSAT_weekday
  
  # CSAT by hour of day
  hour_csat <- df %>%
    group_by(Hour_of_day) %>%
    summarise(Avg_CSAT = mean(CSAT.Score, na.rm = TRUE))
  
  CSAT_hour <- ggplot(hour_csat, aes(x = Hour_of_day, y = Avg_CSAT, group = 1)) +
    geom_line(color = "black", size = 1) +
    geom_point(color = "steelblue", size = 3) +
    labs(title = "Average CSAT Score by Hour of Day", 
         x = "Hour of Day", y = "Average CSAT Score") +
    theme_minimal() +
    scale_x_continuous(breaks = 0:23)
  CSAT_hour
  
  # Volume of tickets by hour and day
  hour_day_volume <- df %>%
    count(Hour_of_day, Day_of_week)
  
  hour_day <- ggplot(hour_day_volume, aes(x = Hour_of_day, y = Day_of_week, fill = n)) +
    geom_tile() +
    scale_fill_gradient(low = "white", high = "steelblue") +
    labs(title = "Ticket Volume by Hour and Day", 
         x = "Hour of Day", y = "Day of Week", fill = "Ticket Count") +
    theme_minimal() +
    scale_x_continuous(breaks = 0:23)
  hour_day
}
```


## Agent Performance Metrics


```{r}
# Create agent performance dashboard
agent_metrics <- df %>%
  group_by(Agent_name) %>%
  summarise(
    Ticket_count = n(),
    Avg_CSAT = mean(CSAT.Score, na.rm = TRUE),
    Avg_Response_Time = mean(response_time_hr, na.rm = TRUE),
    High_CSAT_percentage = mean(as.numeric(as.character(High_CSAT)), na.rm = TRUE) * 100
  ) %>%
  arrange(desc(Ticket_count))

# Only show agents with significant number of tickets (e.g., > 50)
significant_agents <- agent_metrics %>%
  filter(Ticket_count > 50) %>%
  head(20)

print("Top 20 Agents by Ticket Volume (with >50 tickets):")
print(significant_agents)

library(ggrepel)
# Plot agent performance matrix
agent_matrix <- ggplot(significant_agents, 
              aes(x = Avg_Response_Time, y = Avg_CSAT, size = Ticket_count, label = Agent_name)) +
  geom_point(alpha = 0.6, color = "steelblue") +
  geom_text_repel(size = 3, box.padding = 0.5, segment.alpha = 0.6) +
  labs(title = "Agent Performance Matrix", 
       x = "Average Response Time", y = "Average CSAT Score", size = "Ticket Count") +
  theme_minimal()
agent_matrix
```


## Customer Sentiment Analysis


```{r}
# Simple sentiment analysis based on remarks length and CSAT
df$Sentiment_category <- case_when(
  df$CSAT.Score >= 4.5 ~ "Very Positive",
  df$CSAT.Score >= 4 ~ "Positive",
  df$CSAT.Score >= 3 ~ "Neutral",
  df$CSAT.Score >= 2 ~ "Negative",
  TRUE ~ "Very Negative"
)

df$Sentiment_category <- factor(df$Sentiment_category,
                                levels = c("Very Negative", "Negative", "Neutral", "Positive", "Very Positive"))

# Relationship between remarks length and sentiment
sent <- ggplot(df, aes(x = Sentiment_category, y = Remarks_length, fill = Sentiment_category)) +
  geom_boxplot(alpha = 0.7) +
  labs(title = "Relationship Between Customer Remarks Length and Sentiment", 
       x = "Sentiment Category", y = "Remarks Length (characters)") +
  theme_minimal() +
  theme(legend.position = "none")
sent
```


## Summary Findings


```{r}
cat("\n==== SUMMARY OF FINDINGS ====\n")
cat(paste("Total records analyzed:", nrow(df)), "\n")
cat(paste("Average CSAT Score:", round(mean(df$CSAT.Score, na.rm = TRUE), 2)), "\n")
cat(paste("Average Response Time:", round(mean(df$response_time_hr, na.rm = TRUE), 2), "minutes"), "\n")
cat(paste("Percentage of High CSAT (≥4):", round(mean(df$High_CSAT == 1, na.rm = TRUE) * 100, 2), "%"), "\n")

# Top and bottom channels by CSAT
channel_csat <- df %>%
  group_by(channel_name) %>%
  summarise(
    Avg_CSAT = mean(CSAT.Score, na.rm = TRUE),
    Ticket_count = n()
  ) %>%
  arrange(desc(Avg_CSAT))

cat("\nTop 3 Channels by CSAT:\n")
print(head(channel_csat, 3))

cat("\nBottom 3 Channels by CSAT:\n")
print(tail(channel_csat, 3))

# ML Model Performance
cat("\nML Model Performance:\n")
cat(paste("Random Forest Accuracy:", round(accuracy_rf, 4)), "\n")
cat(paste("Random Forest AUC:", round(auc_rf), 4), "\n")
cat(paste("Logistic Regression Accuracy:", round(accuracy_lr, 4)), "\n")
cat(paste("Logistic Regression AUC:", round(auc_lr), 4), "\n")
```


## Recommendations based on analysis


```{r}
cat("\n==== RECOMMENDATIONS ====\n")

# Channel effectiveness
top_channel <- channel_csat$clean_channel_name[1]
worst_channel <- tail(channel_csat$clean_channel_name, 1)[1]
cat(paste("1. Focus on improving", worst_channel, "channel which has the lowest customer satisfaction."), "\n")
cat(paste("2. Study best practices from", top_channel, "channel which has the highest customer satisfaction."), "\n")

# Response time
avg_response_high_csat <- df %>%
  filter(High_CSAT == 1) %>%
  summarise(avg_time = mean(response_time_hr, na.rm = TRUE)) %>%
  pull(avg_time)

cat(paste("3. Aim for response times under", round(avg_response_high_csat, 0), 
          "minutes which are associated with higher CSAT scores."), "\n")

# Agent training
if ("clean_tenure" %in% colnames(df)) {
  tenure_csat <- df %>%
    group_by(clean_tenure) %>%
    summarise(avg_csat = mean(CSAT.Score, na.rm = TRUE)) %>%
    arrange(avg_csat)
  
  lowest_tenure <- tenure_csat$clean_tenure[1]
  cat(paste("4. Provide additional training for agents in the", lowest_tenure, 
            "tenure bucket who have lower average CSAT scores."), "\n")
}

cat("\nAnalysis Complete!\n")
```


```{r}
# Save key data frames for further analysis
saveRDS(agent_metrics, "agent_metrics.rds")
saveRDS(city_analysis, "city_analysis.rds")
saveRDS(product_analysis, "product_analysis.rds")
```

