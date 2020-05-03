#### Variables in the dataset
# X1: Amount of the given credit (NT dollar): it includes both the individual consumer credit and his/her family (supplementary) credit.
# X2: Gender (1 = male; 2 = female).
# X3: Education (1 = graduate school; 2 = university; 3 = high school; 4 = others).
# X4: Marital status (1 = married; 2 = single; 3 = others).
# X5: Age (year).
# X6 - X11: History of past payment. We tracked the past monthly payment records (from April to September, 2005) as follows: X6 = the repayment status in September, 2005; X7 = the repayment status in August, 2005; . . .;X11 = the repayment status in April, 2005. The measurement scale for the repayment status is: -1 = pay duly; 1 = payment delay for one month; 2 = payment delay for two months; . . .; 8 = payment delay for eight months; 9 = payment delay for nine months and above.
# X12-X17: Amount of bill statement (NT dollar). X12 = amount of bill statement in September, 2005; X13 = amount of bill statement in August, 2005; . . .; X17 = amount of bill statement in April, 2005.
# X18-X23: Amount of previous payment (NT dollar). X18 = amount paid in September, 2005; X19 = amount paid in August, 2005; . . .;X23 = amount paid in April, 2005.

# Variables - easier to read
# LIMIT_BAL - how much credit they were given
# PAY_2 - in month two, by how many months are they behind with payment. Can be negative if they overpaid
# BILL_ATM2 - in month two, how much total debt is left to pay
# PAY_ATM2 - in month two, how much they have to pay for that month
# didDefault - did they default in the next month? Target variable!

# Libraries
library(tidyverse) # pipes, filtering etc.
library(ggplot2) # plots
library(reshape2) # melting
orange = "#FFB400" # nice color for plotting


data <- read_csv("data/processed/creadit_default_clean.csv")

# Plotting

## Visualize each variable
## Takes a second to display
data %>% 
  melt() %>%
  filter(variable != 'ID') %>%
  ggplot(aes(x = value)) + 
  stat_density(color = orange, fill = orange, alpha = 0.5) + 
  facet_wrap(~variable, scales = "free") + 
  labs(title = 'Dataset overview')

## Visualize the credit variable
data %>%
  ggplot(aes(x = LIMIT_BAL)) +
  stat_density(color = orange, fill = orange, alpha = 0.5) +
  labs(
    title = 'Credit limit in NT dollars',
    x = 'Crediti limit',
    y = 'Density'
  )

