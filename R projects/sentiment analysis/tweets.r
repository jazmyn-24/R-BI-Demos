# ===============================
# Hardened Airline Sentiment Analysis in R
# ===============================

# Load packages
library(tidyverse)
library(tidytext)
library(ggplot2)
library(wordcloud)
library(RColorBrewer)
library(caret)
library(tm)
library(e1071)
library(textdata)

# Step 1: Load dataset safely
airline <- tryCatch({
  read_csv("Tweets.csv")
}, error = function(e) {
  stop("Dataset not found. Make sure Tweets.csv is in your working directory.")
})

# Verify required columns
required_cols <- c("airline_sentiment", "airline", "text")
if (!all(required_cols %in% names(airline))) {
  stop("Dataset missing required columns: airline_sentiment, airline, text")
}

# Step 2: Sentiment distribution
sent_dist <- airline %>% count(airline_sentiment)
if (nrow(sent_dist) > 0) {
  ggplot(sent_dist, aes(x = airline_sentiment, y = n, fill = airline_sentiment)) +
    geom_col(show.legend = FALSE) +
    labs(title = "Sentiment Distribution", x = "Sentiment", y = "Count") +
    theme_minimal()
}

# Step 3: Tokenization
tidy_tweets <- tryCatch({
  airline %>%
    select(airline, airline_sentiment, text) %>%
    unnest_tokens(word, text)
}, error = function(e) {
  tibble()
})

if (nrow(tidy_tweets) == 0) stop("Tokenization failed: no rows produced")

tidy_tweets <- tidy_tweets %>%
  filter(!word %in% stop_words$word,
         str_detect(word, "^[a-z']+$"))

if (nrow(tidy_tweets) == 0) stop("All words removed after cleaning")

# Step 4: Lexicon analysis
bing <- get_sentiments("bing")

bing_sent <- tidy_tweets %>%
  inner_join(bing, by = "word") %>%
  count(airline_sentiment, sentiment)

if (nrow(bing_sent) == 0) {
  warning("No sentiment words matched with Bing lexicon")
} else {
  ggplot(bing_sent, aes(x = airline_sentiment, y = n, fill = sentiment)) +
    geom_col(position = "dodge") +
    labs(title = "Bing Lexicon Sentiment by Airline Sentiment Label")
}

# Step 5: Modeling prep
airline$text <- tolower(airline$text)

corpus <- VCorpus(VectorSource(airline$text))
corpus <- tm_map(corpus, content_transformer(tolower))
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, removeNumbers)
corpus <- tm_map(corpus, removeWords, stopwords("en"))
corpus <- tm_map(corpus, stripWhitespace)

dtm <- DocumentTermMatrix(corpus)
dtm <- removeSparseTerms(dtm, 0.99)

dataset <- as.data.frame(as.matrix(dtm))
dataset$sentiment <- factor(airline$airline_sentiment)

# Check class balance
table(dataset$sentiment)

# Step 6: Train/test split with safety
set.seed(123)
if (length(unique(dataset$sentiment)) < 2) {
  stop("Not enough sentiment classes for modeling")
}
trainIndex <- createDataPartition(dataset$sentiment, p = 0.7, list = FALSE)
train <- dataset[trainIndex, ]
test <- dataset[-trainIndex, ]

# Make sure all factor levels exist in both
train$sentiment <- factor(train$sentiment, levels = levels(dataset$sentiment))
test$sentiment <- factor(test$sentiment, levels = levels(dataset$sentiment))

# Step 7: Naive Bayes model
nb_model <- naiveBayes(sentiment ~ ., data = train)
nb_preds <- predict(nb_model, newdata = test)

print(confusionMatrix(nb_preds, test$sentiment))

# Step 8: Logistic regression model
log_model <- train(sentiment ~ ., data = train,
                   method = "multinom",
                   trace = FALSE)
log_preds <- predict(log_model, newdata = test)
print(confusionMatrix(log_preds, test$sentiment))

# Step 9: Compare models
model_results <- tibble(
  Model = c("Naive Bayes", "Logistic Regression"),
  Accuracy = c(mean(nb_preds == test$sentiment),
               mean(log_preds == test$sentiment))
)

ggplot(model_results, aes(x = Model, y = Accuracy, fill = Model)) +
  geom_col(show.legend = FALSE) +
  ylim(0, 1) +
  labs(title = "Model Accuracy Comparison")
