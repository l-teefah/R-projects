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

Upon observation of the data, I noticed that in some rowsthe data isn't properly inputted so they will be removed.


```{r}
# remove rows that didn't import properly
df <- df %>% 
  slice(-c(15437:15484, 45128, 45176))

# change CSAT to numeric 
df <- df %>% mutate(CSAT.Score=as.numeric(CSAT.Score))
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

CSAT_product_category <- ggplot(avg_csat, aes(x = reorder(clean_product_category, -avg_csat), y = avg_csat)) +
  geom_bar(stat = "identity", fill = "steelblue", alpha = 0.7) +
  labs(title = "Average CSAT by Product Category", x = "Product Category", y = "Average CSAT Score") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
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

#95th percentile
response_CSAT_percentile <- ggplot(df, aes(x = CSAT.Score, y = response_time_hr)) +
  geom_point(alpha = 0.3, color = "steelblue") +
  geom_smooth(method = "loess", color = "red") +
  labs(title = "Response Time vs CSAT Score", 
       x = "Response Time (hrs)", y = "CSAT Score") +
  theme_minimal() +
  xlim(0, quantile(df$response_time_hr, 0.95, na.rm = TRUE))
response_CSAT_percentile
```


```{r}
# CSAT by agent tenure
CSAT_tenure <- ggplot(df, aes(x = clean_tenure, y = CSAT.Score)) +
  geom_boxplot(fill = "steelblue", alpha = 0.7) +
  labs(title = "CSAT Scores by Agent Tenure", x = "Tenure Bucket", y = "CSAT Score") +
  theme_minimal()
CSAT_tenure
```


```{r}
# CSAT by shift
CSAT_shift <- ggplot(df, aes(x = clean_agent_shifts, y = CSAT.Score)) +
  geom_boxplot(fill = "steelblue", alpha = 0.7) +
  labs(title = "CSAT Scores by Agent Shift", x = "Agent Shift", y = "CSAT Score") +
  theme_minimal()
CSAT_shift
```


```{r}
# Correlation heatmap of numeric variables
numeric_df <- df %>% select_if(is.numeric)
correlation <- cor(numeric_df, use = "pairwise.complete.obs")

png("correlation_heatmap.png", width = 12, height = 10, units = "in", res = 300)
corrplot(correlation, method = "color", type = "upper", 
         addCoef.col = "black", number.cex = 0.7,
         tl.col = "black", tl.srt = 45, tl.cex = 0.8,
         title = "Correlation Heatmap", mar = c(0, 0, 2, 0))
dev.off()
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

cities <- ggplot(city_counts, aes(x = reorder(Customer_City, -n), y = n)) +
  geom_bar(stat = "identity", fill = "steelblue", alpha = 0.7) +
  labs(title = "Top 10 Cities by Number of Issues", 
       x = "City", y = "Number of Issues") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
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

performance_by_CSAT <- ggplot(agent_perf, aes(x = reorder(Agent_name, -CSAT_Score), y = CSAT_Score)) +
  geom_bar(stat = "identity", fill = "steelblue", alpha = 0.7) +
  labs(title = "Top 20 Agents by CSAT Score", 
       x = "Agent", y = "Average CSAT Score") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))
performance_by_CSAT

performance_by_response <- ggplot(agent_perf, aes(x = reorder(Agent_name, -CSAT_Score), y = Handling_time)) +
  geom_bar(stat = "identity", fill = "steelblue", alpha = 0.7) +
  labs(title = "Handling Time for Top 20 Agents", 
       x = "Agent", y = "Average Handling Time") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))
performance_by_response
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
  ggplot(aes(x = reorder(word, n), y = n)) +
  geom_col() +
  coord_flip() +
  labs(x = "Word", y = "Frequency", title = "Top 20 Most Frequent Words in Customer Remarks")
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
  geom_histogram(binwidth = 1, fill = "steelblue") +
  labs(title = "Distribution of Customer Sentiment Scores",
       x = "Sentiment Score (AFINN)",
       y = "Count")
sentiment_dist_plot

# 2. Sentiment by topic
sentiment_by_topic_plot <- ggplot(final_analysis, aes(x = factor(primary_topic), y = sentiment_score, fill = factor(primary_topic))) +
  geom_boxplot() +
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
# Print summary statistics
print(sentiment_summary)
```


### Mapping Results to a List


```{r}
# Return comprehensive analysis
nlp_results <- list(
  data = final_analysis,
  summary = sentiment_summary,
  top_words = head(word_counts, 20),
  topics = top_terms,
  plots = list(
    top_words = top_words_plot,
    sentiment_dist = sentiment_dist_plot,
    sentiment_by_topic = sentiment_by_topic_plot,
    emotions = emotion_plot,
    topics = topic_plot
  )
)

# individual components (e.g.,emotion plot) can be accessed like this:
nlp_results$plots$emotion_plot
```


## Machine Learning - CSAT Score Prediction


```{r}
# Define target variable - let's predict if CSAT is high (≥4) or not
df$High_CSAT <- as.factor(ifelse(df$CSAT.Score >= 4, "Yes", "No"))

# checking to see category and sub category columns imported properly
unique(df$category)
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

#view NA rows
na_rows <- subset(df, is.na(clean_sub_category))
print(na_rows)
#tail(na_rows)
```


```{r}
# Select features
categorical_features <- c("clean_channel_name", "clean_category", "clean_sub_category", "clean_product_category", 
                          "Customer_City", "clean_tenure", "clean_agent_shifts")
numeric_features <- c("Item_price", "connected_handling_time", "Response_time_min", 
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
# Create model data frame
model_df <- df %>%
  select(all_of(c(categorical_features, numeric_features, "High_CSAT")))

# Handle missing values again after feature creation
model_df <- model_df %>%
  mutate_if(is.numeric, ~replace_na(., median(., na.rm = TRUE)))

# Convert categorical variables to factors
model_df <- model_df %>%
  mutate_if(is.character, as.factor)
```


```{r}
# Split the data
library(caret)
library(recipes)

# Check class balance -  High_CSAT is imbalanced
table(df$High_CSAT) 
```


I'll create a new df named 'balanced_df' to manually balance df before splitting into train and test model as the results show that High_CSAT table is imbalanced and this may affected the train and test data model if I continue with the original dataframe. 


```{r}
# training data
min_class_size <- min(table(df$High_CSAT))  # Smallest class size 
balanced_df <- df %>%
  group_by(High_CSAT) %>%
  sample_n(min_class_size)  # Sample equal numbers from each class
balanced_df$High_CSAT <- factor(balanced_df$High_CSAT, levels = c("No", "Yes"))

# Check class balance
table(balanced_df$High_CSAT)  # Should be roughly 8978 vs 8978 now

# Split data into train (75%) and test (25%) sets
set.seed(42) #for reproductibility
trainIndex <- createDataPartition(balanced_df$High_CSAT, p = 0.75, list = FALSE)
train_data <- balanced_df[trainIndex, ]
test_data <- balanced_df[-trainIndex, ]

# Handle missing values in training data
train_data <- train_data %>%
  group_by(High_CSAT) %>%
  mutate(across(where(is.numeric), ~replace_na(., median(., na.rm = TRUE)))) %>%
  ungroup()

# Define preprocessing
preProcess_recipe <- recipe(High_CSAT ~ ., data = train_data) %>%
  step_normalize(all_numeric(), -all_outcomes()) %>%
  step_dummy(all_nominal(), -all_outcomes(), one_hot = TRUE)

# Prepare the recipe with training data
prepped_recipe <- prep(preProcess_recipe, training = train_data)

# Apply preprocessing to both training and testing data
train_processed <- bake(prepped_recipe, new_data = train_data)
test_processed <- bake(prepped_recipe, new_data = test_data)
```

