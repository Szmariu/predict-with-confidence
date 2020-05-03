library(ranger)
library(tidyverse)
library(ggplot2)
library(class)



source_data <- read_csv("data/processed/credit_default_artificial.csv")
set.seed(123)

simulate_dataset <- function(source_data){
  dataset <- source_data
  probs <- dataset$default_prob
  dataset[,'didDefault'] <- as.factor(as.numeric(runif(length(probs)) < probs))
  dataset <- dataset %>% select(-default_prob) 
  dataset
}



target <- 'didDefault'
form <- formula(paste0(target, " ~ ."))

n_folds = 3
n_reps <- 10

outer_result <- list()
for(j in 1:n_reps){
  data <- simulate_dataset(source_data)
  folds <- sample(rep(c(1:n_folds),ceiling(nrow(data)/n_folds)), size = nrow(data))
  
  inner_result <- list()
  for(i in 1:n_folds){
    # Build the model on training data
    rf_fit <- ranger(formula=form, data=na.omit(data[folds != i,]), 
                     probability=TRUE,
                     keep.inbag=TRUE) 
    
    # Generate predictions and standard errors for test set
    pred <- predict(rf_fit, data=na.omit(data[folds == i,]), type = "se")
    
    inner_result[i] <- list(as_tibble(list(id = which(folds==i), 
                                           pred_prob = pred$predictions[,1], 
                                           se = pred$se[,1], 
                                           true = source_data$default_prob[folds == i])))
    
  }
  
  inner_result <- do.call(rbind,inner_result)
  inner_result <- inner_result %>% arrange(id)
  inner_result['rep'] <- j
  outer_result[j] <- list(inner_result)
}

res <- do.call(rbind,outer_result)
res <- res %>% mutate(upper = pred_prob + 2*se,
               lower = pred_prob - 2*se)


res %>% mutate(inside = (true < upper) & (true > lower)) %>% 
  group_by(rep) %>% 
  summarise(inside = mean(inside)) 
  

res %>% mutate(inside = (true < upper) & (true > lower)) %>% 
  group_by(id) %>% 
  summarise(inside = mean(inside), mean_se = mean(se)) %>% 
  ggplot() +
  geom_histogram(aes(x=inside))
