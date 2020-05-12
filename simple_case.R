library(ranger)
library(tidyverse)
library(ggplot2)


# Simple test based on example from the presentation

didDefault <- c(c(rep(1, 100) , rep(0, 10000 - 100)),
                c(rep(1, 50), rep(0, 500 - 50)))


self_employed <- c(rep(0, 10000), rep(1, 500))



data <- tibble(self_employed=factor(self_employed), didDefault = factor(didDefault))


rf <- ranger("didDefault ~ .", data=data, probability = T, keep.inbag=TRUE)

res <- predict(rf, data, type ='se')
res <- tibble(pred_prob = res$predictions[,2], se = res$se[,2])
res <- cbind(data ,res)


tbl <- res %>% group_by(self_employed) %>% 
  summarise(n_def = sum(didDefault == 1),
    n = n(),
    pd = mean(pred_prob),
    se = sqrt(mean(se^2)),
    pd_low_ours = pd - 1.96 * se,
    pd_up_ours = pd + 1.96 * se,
    pd_low_exact = qbinom(0.025, size = n, prob = n_def/n)/n ,
    pd_up_exact = qbinom(1-0.025, size = n, prob = n_def/n)/n) 




