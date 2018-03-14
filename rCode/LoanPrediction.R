# https://datahack.analyticsvidhya.com/contest/practice-problem-loan-prediction-iii/

### SETTING UP ENVIRONMENT

rm(list = ls())
options(scipen = 999)
library(data.table)
library(summaryR)

### DEFINING MODE FUNCTION

### DEFINING MODE FUNCTION 

Mode <- function(x, na.rm = TRUE) {
  if(na.rm){
    x = x[!is.na(x)]
  }
  
  ux <- unique(x)
  return(ux[which.max(tabulate(match(x, ux)))])
}

### DETAILED SUMMARISING OF CATEGORY COLUMNS

summariseCategCols <- function(x,y){
  factCols <- NULL
  df <- NULL
  df1 <- NULL
  z <- x
  x <- as.data.frame(x)
  for (i in 1:ncol(x)) {
    if(class(x[,i]) %in% c("factor","character")){
      factCols <- c(names(x[i]),factCols)
    }
  }
  for (i in 1:length(factCols)){
    colName <- factCols[i]
    respVar <- which(colnames(x) == y)
    if(length(unique(x[[which(colnames(x) == colName)]])) < 51) {
      #df1 <-  z[,.(Total_Sum = sum(z[,respVar], na.rm=T), Total_Count = .N), colName]
      df1 <-  z[,.(Total_Count = .N, Count_Percent = round((.N*100/nrow(z)),0)), colName]
      names(df1)[1] <- "Category_Values"
      df1$Category <- colName
      df1 <- df1[,c(4,1,2,3)]
      df <- rbind(df,df1)
    }
  }
  return(df)
}


### READING DATA #### names(train)

train <- fread("./Source/train.csv", na.strings = c("", " ", "  ", "NA", "N/A"))
test <- fread("./Source/test.csv", na.strings = c("", " ", "  ", "NA", "N/A"))

### DATA ANALYSIS ####

View(t(summaryR(data.frame(train))))

hist(train$ApplicantIncome, xlim = 2)
?aggregate

aggregate(ApplicantIncome~Education, data=train, FUN = length)

hist(train$ApplicantIncome, breaks = 36, col = 'red')

nrow(train[ApplicantIncome > 30000,])

pairs(Self_Employed ~ ., data = train)

unique(train$Gender)

summariseCategCols(train, "Loan_Status")
