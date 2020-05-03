library(ranger)
library(tidyverse)
library(ggplot2)
library(class)

# Read processed data
data <- read_csv("data/processed/credit_default_clean.csv")

# Remove id column
data <- data[,-1]

target <- 'didDefault'
data[,target] <- factor(data[[target]])



lr <- glm("didDefault ~ .", data = data, family ="binomial")
lr_prob <- predict(lr, data, type = "response")
lr_prob <- unname(lr_prob)



target_ix = which(colnames(data) == target)

knn_model <- knn(data[,-target_ix], data[,-target_ix], data[[target]], k = 100, prob=T)
knn_prob = attr(knn_model, "prob")


rf <- ranger("didDefault ~ .", data = data, probability=T)

rf_prob <- round(predict(rf ,data)$predictions[,1], 3)

probs <- cbind(lr_prob, rf_prob, knn_prob)

weights <- c(0.1, 0.8, 0.1)

probs <- round(apply(weights * probs, 1, sum),3)


data['default_prob'] = probs
data <- data[,-target_ix]


write_csv(data, "data/processed/credit_default_artificial.csv")
