# Main analysis

library(ranger)
library(tidyverse)
library(ggplot2)

library(ggthemes)
library(ggrepel)
library(cowplot)
library(ggridges)
library(viridis)
library(ggtech) # For the airbnb theme
library(extrafont) # To add the font for Airbnb theme
library(GGally) # Grapihical correlogram

theme_set(theme_airbnb_fancy())
pink = "#FF5A5F"
orange = "#FFB400"
blueGreen = "#007A87"
flesh = "#FFAA91"
purple = "#7B0051"
options(scipen=999) #avoiding e10 notation

# Read processed data
data <- read_csv("data/processed/credit_default_clean.csv")

# Remove id column
data <- data[,-1]

target <- 'didDefault'
data[,target] <- factor(data[[target]])

# This function runs k fold cross validation and append following columns to original data:
# id - row number from data
# pred_prob - OOS predicted probability of default
# se - estimated OOS standard error
rf_cv <- function(data, n_folds=3){
  form <- formula("didDefault ~ .")

  folds <- sample(rep(c(1:n_folds),ceiling(nrow(data)/n_folds)), size = nrow(data))
  
  results <- list()
  
  for(i in 1:n_folds){
    # Build the model on training data
    rf_fit <- ranger(formula=form, 
                     data=na.omit(data[folds != i ,]), 
                     probability=TRUE,
                     keep.inbag=TRUE) 
    
    # Generate predictions and standard errors for test set
    pred <- predict(rf_fit, data=na.omit(data[folds == i,]), type = "se")
    
    results[i] <- list(as_tibble(list(id = which(folds==i), 
                                      pred_prob = pred$predictions[,2], 
                                      se = pred$se[,2])))
    
  }
  results <- do.call(rbind, results)
  results <- results %>% arrange(id)
  results <- cbind(data, results)
  return(results)
}

res <- rf_cv(data, 3)

res$trueResult <- as.integer(res$didDefault) - 1
# Plots

# TODO: Titles and labels etc.



ggplot(res) +
  geom_boxplot(aes(x=didDefault, y=pred_prob, fill=didDefault))


ggplot(res) +
  geom_point(aes(x=pred_prob, y=1-se, color=didDefault))


ggplot(res) +
  geom_jitter(aes(x=didDefault, y= (1 - abs(as.integer(didDefault) - pred_prob - 1)) * (1 - se) , color=didDefault))


ggplot(res) +
  geom_jitter( 
    aes(
      x = cut_interval(LIMIT_BAL, 100),
      y = ( 1 - abs(trueResult - pred_prob) ) * (1 - se)
      )
  )

res %>%
  group_by(cut_interval(LIMIT_BAL, 100)) %>%
  summarise(x = median(LIMIT_BAL), value = median(( 1 - abs(trueResult - pred_prob) ) * (1 - se))) %>%
  ggplot() +
  geom_jitter( 
    aes(
      x = x,
      y = value
    )
  )


res %>%
  group_by(cut_interval(PAY_AMT1, 100)) %>%
  summarise(x = median(PAY_AMT1), value = mean(( 1 - abs(trueResult - pred_prob) ) * (1 - se))) %>%
  ggplot() +
  geom_jitter( 
    aes(
      x = x,
      y = value
    )
  )



res %>%
  group_by(cut_interval(PAY_AMT1, 100)) %>%
  summarise(x = median(PAY_AMT1), value = mean( 1 - se)) %>%
  ggplot() +
  geom_point( 
    aes(
      x = x,
      y = value
    )
  )



( 1 - abs(trueResult - pred_prob) ) * (1 - se)


ggplot(res) +
  geom_histogram(aes(x=se))


ggplot(res) +
  geom_boxplot(aes(x=factor(isFemale), y = se))


ggplot(res) + 
  geom_boxplot(aes(x=cut_number(LIMIT_BAL, 5), y = log(se)))

ggplot(res) +
  geom_jitter(aes(x=LIMIT_BAL, y = se))



# 

res %>%
  ggplot(aes(x = 1-se)) +
  geom_histogram(bins = 60, color = orange, fill = orange, alpha = 0.5) 
  labs(title = 'Distribution of model certainty', x = 'Complexity', y = 'Count')



###################
  
res %>%
    ggplot() +
    geom_point(aes(x = LIMIT_BAL, y = pred_prob))

  
res %>%
    group_by(cut_interval(LIMIT_BAL, 15)) %>%
    summarise(x = mean(LIMIT_BAL), value = median(pred_prob), sd = median(se)) %>%
  ggplot(aes(x = x, y = value)) +
    geom_point() +
    geom_errorbar(aes(ymin = value - sd, ymax = value + sd), width = 10000) 



res %>%
  group_by(education_high_school) %>%
  summarise(x = mean(education_high_school), value = mean(pred_prob), sd = mean(se)) %>%
  ggplot(aes(x = factor(x), y = value)) +
  geom_point() +
  geom_errorbar(aes(ymin = value - sd, ymax = value + sd)) 


#######################


res %>%
  group_by(education_high_school) %>%
  summarise(x = mean(education_high_school), value = mean(pred_prob), sd = mean(se)) %>%
  ggplot(aes(x = factor(x), y = sd)) +
  geom_point() +
  geom_errorbar(aes(ymin = value - sd, ymax = value + sd)) 








devtools::install_github("ricardo-bion/ggtech", 
                         dependencies=TRUE)
# Repeated cv


cv_results <- map(1:3, function(x) rf_cv(data, 3))

for(i in 1:3){
  cv_results[[i]]$rep <- i
}

cv_results <- do.call(rbind, cv_results)

res$prob_bin <- res$pred_prob %>% 
  cut_width(width=0.1,boundary=0)

# Aggregating probability of default, true default rate and se per probability bin

tmp <- cv_results  %>% 
  mutate(prob_bin = cut_width(pred_prob, width=0.1, boundary=0)) %>% 
  group_by(rep, prob_bin) %>% 
  summarise(prob = mean(pred_prob), 
            se = sqrt(mean(se^2)),
            prop = mean(didDefault == 1),
            n = n())

head(tmp)

# Visualizing how well model predicts per probability bin

tmp %>% 
  ggplot(aes(x = prop ,
             y = prob)) +
  geom_point() +
  geom_errorbar(aes(ymin = prob - se, ymax = prob  + se)) +
  geom_abline(slope = 1, lty = 2, color = 'blue') + 
  xlab("Fraction of defaults") + 
  ylab("Preidcted probability of default") +
  ggtitle("OOS probability of default vs True fraction of defaults")


# Probably shouldn't need anything below



# Evaluation
true = as.numeric(res$didDefault == 1)

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


