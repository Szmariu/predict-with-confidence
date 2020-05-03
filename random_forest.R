library(ranger)
library(tidyverse)
library(ggplot2)

# Read processed data
data <- read_csv("data/processed/credit_default_clean.csv")

# Remove id column
data <- data[,-1]

target <- 'didDefault'
data[,target] <- factor(data[[target]])

# Target class frequency
data[,target] %>% table()

# Function to divide df into training, and test sets
index <- function(df=df,pctTrain=0.7)
{
  N <- nrow(df)
  train <- sample(N, pctTrain*N)
  test <- setdiff(seq_len(N),train)
  Ind <- list(train=train,test=test)
  return(Ind)
}
#

set.seed(123)
ind <- index(data, 0.8)
length(ind$train); length(ind$test)

form <- formula(paste0(target, " ~ ."))

# Build the model on training data
rf_fit <- ranger(formula=form, data=na.omit(data[ind$train,]), 
                 probability=TRUE,
                 keep.inbag=TRUE) 

# Generate predictions and standard errors for test set
pred <- predict(rf_fit, data=na.omit(data[ind$test,]), type = "se")

# Merge predictions and true classes
res <- as_tibble(list(pred_prob = pred$predictions[,2], se = pred$se[,2], true =  data[[target]][ind$test]))


# TODO: Titles and labels etc.

ggplot(res) +
  geom_boxplot(aes(x=true, y=pred_prob, fill=true))


ggplot(res) +
  geom_point(aes(x=pred_prob, y=se, color=true))


ggplot(res) +
  geom_histogram(aes(x=se))

# Evaluation
true = as.numeric(res$true)-1

calc_preds <- function(threshold){
  # Calculate class predictions using given threshold
  as.numeric(res$pred_prob > threshold)
}

calc_preds_improved <- function(threshold){
  # Calculate class predictions using given threshold and estimated se
  as.numeric(pmin(res$pred_prob + 2 * res$se, 1.) > threshold)
}


# AUC ROC

tpr <- function(pred, true){
  # True positive rate
  sum(pred == 1 & true == 1)/sum(true == 1)
}

fpr <-  function(pred, true){
  # False positive rate
  sum(pred == 1 & true == 0)/sum(true == 0)
}

threshold_range <- res$pred_prob %>% 
                  round(3) %>% 
                  unique() %>% 
                  sort() %>% 
                  c(0, ., 1)


results <- data.frame(matrix(ncol=2, nrow=length(threshold_range)))
colnames(results) <- c("fpr", "tpr")

for(i in seq_along(threshold_range)){
  preds <- calc_preds(threshold_range[i])
  results$fpr[i] <- fpr(preds, true)
  results$tpr[i] <- tpr(preds, true)
}

plot(results, type = 'l')


results_improved <- data.frame(matrix(ncol=2, nrow=length(threshold_range)))
colnames(results_improved)  <- c("fpr", "tpr")


for(i in seq_along(threshold_range)){
  preds <- calc_preds_improved(threshold_range[i])
  results_improved$fpr[i] <- fpr(preds, true)
  results_improved$tpr[i] <- tpr(preds, true)
}

plot(results, type = 'l')
lines(results_improved, col='red')

roc_auc <- function(results){
  y = results$tpr[2:nrow(results)]
  x = results$fpr[1:(nrow(results)-1)] - results$fpr[2:nrow(results)]
  sum(x*y)
}

roc_auc(results)
roc_auc(results_improved)

# Precision and Recall

recall <- function(pred, true){
  tpr(pred, true)
}

precision <- function(pred, true){
  sum(pred == 1 & true == 1)/sum(pred==1)
}



results <- data.frame(matrix(ncol=2, nrow=length(threshold_range)))
colnames(results) <- c("precision", "recall")

for(i in seq_along(threshold_range)){
  preds <- calc_preds(threshold_range[i])
  results$precision[i] <- precision(preds, true)
  results$recall[i] <- recall(preds, true)
}



results_improved <- data.frame(matrix(ncol=2, nrow=length(threshold_range)))
colnames(results_improved) <- c("precision", "recall")

for(i in seq_along(threshold_range)){
  preds <- calc_preds_improved(threshold_range[i])
  results_improved$precision[i] <- precision(preds, true)
  results_improved$recall[i] <- recall(preds, true)
}


plot(results$recall, results$precision, type='l')
lines(results_improved$recall, results_improved$precision, col='red')


