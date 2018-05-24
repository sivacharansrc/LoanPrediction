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
  return(df[1:nrow(df)-1,])
}



### READING DATA #### names(train)

train <- fread("./Source/train.csv", na.strings = c("NA", "N/A", "<NA>", ""), strip.white = T)
test <- fread("./Source/test.csv", na.strings = c("NA", "N/A","<NA>", ""), strip.white = T)

### DATA ANALYSIS ####

View(t(summaryR(data.frame(train))))

hist(train$ApplicantIncome)
hist(train$ApplicantIncome[train$ApplicantIncome<2000])
hist(train$ApplicantIncome, breaks = 48, col = 'red')

### SUMMARIZING THE CATEGORICAL VARIABLES ####

summariseCategCols(train, "Loan_Status")

#### FIXING MISSING VALUES ####

test$Loan_Status <- NA
completeData <- rbind(train,test)

### FIXING SELF EMPLOYED

summariseCategCols(completeData, "Loan_Status")

completeData[is.na(Self_Employed), Self_Employed:= "No"]
completeData[is.na(Dependents), Dependents:= "0"]
completeData[is.na(Married), Married:= "Yes"]

summary(completeData)

plot(completeData$LoanAmount, completeData$ApplicantIncome)
pairs()
