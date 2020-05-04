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

data %>% 
 group_by(education_graduate_school) %>% 
 summarize(n())




target <- 'didDefault'
form <- formula(paste0(target, " ~ ."))

n_folds = 3


run <- function(data){
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
                                      pred_prob = pred$predictions[,1], 
                                      se = pred$se[,1])))
    
  }
  results <- do.call(rbind, results)
  results <- results %>% arrange(id)
  results <- cbind(data, results)
  results
}


n_repeats <- 10


results_full <- list()
results_reduced <- list()

for(i in 1:n_repeats){
  to_drop = 1:nrow(data) %in% sample(which(data$education_graduate_school == 1), 2000)
  results_full[i] <- list(run(data))
  results_reduced[i]  <- list(run(data[!to_drop,]))
}

for(i in 1:n_repeats){
  results_full[[i]]$rep <- i
  results_reduced[[i]]$rep <- i
}


results_full <- do.call(rbind, results_full)
results_reduced <- do.call(rbind, results_reduced)


aggregate_full <- 
results_full %>% 
  group_by(education_graduate_school, rep) %>% 
  summarize(mean_se = mean(se)) %>% 
  mutate(dataset = "full")

aggregate_reduced <- 
results_reduced %>% 
  group_by(education_graduate_school, rep) %>% 
  summarize(mean_se = mean(se)) %>% 
  mutate(dataset = "reduced")


aggregate_res <- rbind(aggregate_reduced, aggregate_full)
aggregate_res$dataset <- factor(aggregate_res$dataset)
aggregate_res$education_graduate_school <- factor(aggregate_res$education_graduate_school)

aggregate_res %>% 
  ggplot() + 
  geom_boxplot(aes(x=education_graduate_school, y = mean_se, fill=dataset))


results_full %>% 
  group_by(rep) %>% 
  summarise(n())

results_reduced %>% 
  group_by(rep) %>% 
  summarise(n())
