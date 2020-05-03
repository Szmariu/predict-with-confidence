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
library(readxl) # importing excel files
library(ggplot2) # plots
library(reshape2) # melting


# Data import
data <- read_excel('data/raw/default of credit card clients.xls', skip = 1)

# Data wrangling

## Rename target variable
data <- data %>%
  rename('didDefault' = 'default payment next month')

## Remove outliers and incorrect observations
## Overall, about 50% of observations are dropped
## The quality should be much better
## The proportions of the target variable did not change significantly
data <- data %>%
  filter(LIMIT_BAL < 700000,
         EDUCATION > 0, EDUCATION < 4,
         MARRIAGE > 0, MARRIAGE < 3,
         PAY_0 < 4,
         PAY_2 < 4,
         PAY_3 < 4,
         PAY_4 < 4,
         PAY_5 < 4,
         PAY_6 < 4,
         BILL_AMT1 > 0, BILL_AMT1 < 200000,
         BILL_AMT2 > 0, BILL_AMT2 < 200000,
         BILL_AMT3 > 0, BILL_AMT3 < 200000,
         BILL_AMT4 > 0, BILL_AMT4 < 200000,
         BILL_AMT5 > 0, BILL_AMT5 < 200000,
         BILL_AMT6 > 0, BILL_AMT6 < 200000,
         PAY_AMT1 < 10000,
         PAY_AMT2 < 10000,
         PAY_AMT3 < 10000,
         PAY_AMT4 < 10000,
         PAY_AMT5 < 10000,
         PAY_AMT6 < 10000
         )

## Create dummy variables
data <- data %>%
  mutate(
    isFemale = ifelse(SEX == 2, 1, 0),    
    education_high_school = ifelse(EDUCATION == 3, 1, 0), 
    education_university = ifelse(EDUCATION == 2, 1, 0), 
    education_graduate_school = ifelse(EDUCATION == 1, 1, 0),
    isMarried = ifelse(MARRIAGE == 1, 1, 0),
    SEX = NULL, # remove variable
    EDUCATION = NULL,
    MARRIAGE = NULL
  )

data %>% 
  write_csv("data/processed/credit_default_clean.csv")
