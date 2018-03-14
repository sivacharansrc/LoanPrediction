# https://datahack.analyticsvidhya.com/contest/practice-problem-loan-prediction-iii/

### SETTING UP ENVIRONMENT

rm(list = ls())
options(scipen = 999)
library(data.table)
library(summaryR)

### READING DATA ####

train <- fread("./Source/train.csv")
test <- fread("./Source/test.csv")

### DATA ANALYSIS ####

View(t(summaryR(data.frame(train))))

hist(train$ApplicantIncome, xlim = 2)
?hist
