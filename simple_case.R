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









############ Uneven groups
# Simple test based on example from the presentation

didDefault <- c(c(rep(1, 165) , rep(0, 10000 - 165)),
                c(rep(1, 57), rep(0, 500 - 57)))


self_employed <- c(rep(0, 10000), rep(1, 500))



data <- tibble(self_employed=factor(self_employed), didDefault = factor(didDefault))


rf <- ranger("didDefault ~ .", data=data, probability = T, keep.inbag=TRUE)

res2 <- predict(rf, data, type ='se')
res2 <- tibble(pred_prob = res2$predictions[,2], se = res2$se[,2])
res2 <- cbind(data ,res2)


plot <- res2 %>% group_by(self_employed) %>% 
  summarise(n_def = sum(didDefault == 1),
    n = n(),
    pd = mean(pred_prob),
            se = sqrt(mean(se^2)),
            pd_low = pd - 1.96 * se,
            pd_up = pd + 1.96 * se) %>%
  ggplot(aes(x = factor(self_employed), y = pd)) +
  geom_point(color = blueGreen, size = 7) +
  geom_errorbar(aes(ymin = pd_low, ymax = pd_up), size = 3, width = 0.1, color = blueGreen, alpha = 0.5) + 
  labs(
    title = 'Probability of default - type of employment',
    subtitle = 'Salary: 10 000, Self employed: 500',
    x = '',
    y = 'Probability of default'
  ) + 
  scale_x_discrete(labels = c('Salary (n = 10 000)', 'Self employed (n = 500)')) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0, 0.16)) 

plot(plot)

plot %>% ggsave('plot01.png', 
                .,
                'png', 
                file.path(getwd(), "plots"),
                width = 20,
                height = 10)



############################


# Simple test based on example from the presentation

didDefault <- c(c(rep(1, 165) , rep(0, 10000 - 165)),
                c(rep(1, 320), rep(0, 9000 - 320)))


self_employed <- c(rep(0, 10000), rep(1, 9000))



data <- tibble(self_employed=factor(self_employed), didDefault = factor(didDefault))


rf <- ranger("didDefault ~ .", data=data, probability = T, keep.inbag=TRUE)

res2 <- predict(rf, data, type ='se')
res2 <- tibble(pred_prob = res2$predictions[,2], se = res2$se[,2])
res2 <- cbind(data ,res2)


plot <- res2 %>% group_by(self_employed) %>% 
  summarise(n_def = sum(didDefault == 1),
            n = n(),
            pd = mean(pred_prob),
            se = sqrt(mean(se^2)),
            pd_low = pd - 1.96 * se,
            pd_up = pd + 1.96 * se) %>%
  ggplot(aes(x = factor(self_employed), y = pd)) +
  geom_point(color = blueGreen, size = 7) +
  geom_errorbar(aes(ymin = pd_low, ymax = pd_up), size = 3, width = 0.1, color = blueGreen, alpha = 0.5) + 
  labs(
    title = 'Probability of default - marital status',
    subtitle = 'Married: 10 000, Single: 9 000',
    x = '',
    y = 'Probability of default'
  ) + 
  scale_x_discrete(labels = c('Married (n = 10 000)', 'Single (n = 9 000)')) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0, 0.042)) 

plot(plot)

plot %>% ggsave('plot02.png', 
                .,
                'png', 
                file.path(getwd(), "plots"),
                width = 20,
                height = 10)




###########################

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



plot <- ggplot(res) +
  geom_jitter(aes(x = pred_prob, y = 1 - se, color = 1 - se), alpha = 0.4) +
  scale_color_gradient(high = orange, low = purple) +
  labs(title = 'Model confidence vs predicted probability of default',
       subtitle = 'Mean probability of default: 26.7%',
       x = 'Probability of default',
       y = 'Model confidence') +
  theme(legend.position = 'none') +
  scale_x_continuous(labels = scales::percent_format(accuracy = 1))

plot(plot)

plot %>% ggsave('plot03.png', 
                .,
                'png', 
                file.path(getwd(), "plots"),
                width = 20,
                height = 10)
>>>>>>> fdeda5f545c760b1beb2d488ae42c658ea6f75a8
