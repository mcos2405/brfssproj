# title: "Predicting Cardiovascular Disease"
# author: "Martin O'Sullivan"
# date: "25/02/2020"
# output:
#  pdf_document:
#  toc: true
#  toc_depth: 3
#  number_sections: true

# Note: this script will take around 3 to 4 minutes to run on a computer with 
# 8Gb RAM

## Setting up r including chunk options
knitr::opts_chunk$set(echo = TRUE, fig.pos = 'h',fig.align = 'center', fig.width = 5,fig.height = 3, width =60, tidy = FALSE)

#Installing required packages and loading libraries

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
if(!require(kableExtra)) install.packages("kableExtra", repos = "http://cran.us.r-project.org")
if(!require(tidyr)) install.packages("tidyr", repos = "http://cran.us.r-project.org")
if(!require(stringr)) install.packages("stringr",repos = "http://cran.us.r-project.org")
if(!require(forcats)) install.packages("forcats", repos = "http://cran.us.r-project.org")
if(!require(ggplot2)) install.packages("ggplot2", repos = "http://cran.us.r-project.org")
if(!require(plyr)) install.packages("plyr", repos = "http://cran.us.r-project.org")
library(knitr)
library(dplyr)
library(tidyverse)
library(kableExtra)
library(tidyr)
library(stringr)
library(forcats)
library(ggplot2)
library(data.table)
library(plyr)

## Loading the data
brfss <- readRDS("./brfss.RDS")

# Displaying the dimensions of the dataset
dim(brfss)

## Removing missing values and other unwanted rows
# Converting "don't know" and "refused" to NA
brfss[brfss == 7 | brfss == 9 | brfss == 77 | brfss == 99] <- NA
# Removing all NA
brfss <- na.omit(brfss)

## Combining variables
# Merging cardiac disease and stroke variables
brfss<- brfss %>% 
  mutate(CVD = ifelse(X_MICHD == 1 | CVDSTRK3 == 1, 1, 2))

# Merging heavy-drinking and binge-drinking variables
brfss <- brfss %>% 
  mutate(ALCPROB = ifelse(X_RFBING5 == 1 & X_RFDRHV5 == 1, 1, 2))

# Removing variables which are no longer required
brfss <- subset(brfss, 
                select = -c(X_MICHD, CVDSTRK3, X_RFBING5, X_RFDRHV5))


# Displaying the dimensions of the modified dataset
dim(brfss)


## Allocating value labels to variables
# Simplifying diabetes variable and allocating value labels
brfss <- brfss %>% mutate(DIABETE3 = ifelse(DIABETE3 == 1, "Diabetes", "No Diabetes"))
# Allocating value labels to variables
brfss <- brfss %>% mutate(CVD = ifelse(CVD == 1, "CVD", "No CVD")) %>%
mutate(SEX = ifelse(SEX == 1, "Male", "Female")) %>%
mutate(X_AGE65YR = ifelse(X_AGE65YR == 1, "Age 18 to 64", "Age 65 or older")) %>%
mutate(X_RACEG21 = ifelse(X_RACEG21 == 1, "Non-Hispanic White", "Non-White or Hispanic")) %>%
mutate(X_EDUCAG = ifelse(X_EDUCAG == 1, "Not High School Graduate",
      ifelse(X_EDUCAG == 2, "Graduated High School",
          ifelse(X_EDUCAG == 3, "Attended College", "Graduated College")))) %>%
mutate(HLTHPLN1 = ifelse(HLTHPLN1 == 1, "Health Cover", "No Health Cover")) %>%
mutate(EXERANY2 = ifelse(EXERANY2 == 1, "Exercise", "No Exercise")) %>%
mutate(X_BMI5CAT = ifelse(X_BMI5CAT == 4, "Obese", "Not Obese")) %>%
mutate(X_SMOKER3 = ifelse(X_SMOKER3 == 1, "Smokes Every Day",
      ifelse(X_SMOKER3 == 2, "Smokes Some Days",
          ifelse(X_SMOKER3 == 3, "Former Smoker", "Never Smoked")))) %>%
mutate(ADDEPEV2 = ifelse(ADDEPEV2 == 1, "Depression", "No Depression")) %>%
mutate(X_VEGLT1 = ifelse(X_VEGLT1 == 1, "Vegetables", "No Vegetables")) %>%
mutate(X_FRTLT1 = ifelse(X_FRTLT1 == 1, "Fruit", "No Fruit")) %>%
mutate(X_RFHYPE5 = ifelse(X_RFHYPE5 == 1, "No Blood Pressure", "Blood Pressure")) %>%
mutate(TOLDHI2 = ifelse(TOLDHI2 == 1, "High Cholesterol", "Cholesterol OK")) %>% 
mutate(ALCPROB = ifelse(ALCPROB == 1, "No Alcohol Problem", "Alcohol Problem")) %>%
mutate(CHCKIDNY = ifelse(CHCKIDNY == 1, "Kidney Disease", "No Kidney Disease"))

## Converting all variables to factors
col_names <- names(brfss)
brfss[,col_names] <- lapply(brfss[,col_names] , factor)

# Specifying the order of levels of variables with more than two levels
brfss <- brfss %>% 
mutate(X_EDUCAG = ordered(X_EDUCAG,
  levels = c("Not High School Graduate", "Graduated High School", 
             "Attended College", "Graduated College"))) %>%
mutate(X_SMOKER3 = ordered(X_SMOKER3, 
    levels = c("Smokes Every Day", "Smokes Some Days", 
               "Former Smoker", "Never Smoked")))

# Creating training and validation sets
# Validation set will be 20% of the brfss data
set.seed(1, sample.kind="Rounding")
test_index <- createDataPartition(y = brfss$CVD, times = 1, p = 0.2, list = FALSE)
brfss1 <- brfss[-test_index,]
validation <- brfss[test_index,]

# Displaying the number of rows and columns in the training and validation sets----
dim(brfss1)
dim(validation)

# Table of CVD variable showing percentages
brfss1$CVD %>%
   table() %>%
   prop.table() %>% {. * 100} %>% 
   round(2)

# Taking a random sample of 1000 rows for Chi Square tests
set.seed(1, sample.kind="Rounding")
sample1 <- sample_n(brfss1, 1000)

# Pairwise Chi Square tests between all combinations of variables
combins <- combn(ncol(sample1),2)
adply(combins, 2, function(x) {
  test <- chisq.test(sample1[, x[1]], sample1[, x[2]])

  out <- data.frame("Row" = colnames(sample1)[x[1]]
                    , "Column" = colnames(sample1[x[2]])
                    , "Chi.Square" = round(test$statistic,3)
                    ,  "df"= test$parameter
                    ,  "p.value" = round(test$p.value, 3)
                    )
  return(out)
})  

# Removing unwanted predictor variables from both brfss1 and validation sets
brfss1 <- subset(brfss1, 
                select = -c(X_RACEG21, HLTHPLN1, X_BMI5CAT, X_VEGLT1, X_FRTLT1, ALCPROB, ADDEPEV2, SEX))
validation <- subset(validation, 
                select = -c(X_RACEG21, HLTHPLN1, X_BMI5CAT, X_VEGLT1, X_FRTLT1, ALCPROB, ADDEPEV2, SEX))


## Splitting brfss1 into training and test sets
set.seed(1, sample.kind="Rounding")
train_index <- createDataPartition(y = brfss1$CVD, times = 1, p = 0.2, 
                                  list = FALSE)
brfss_train<- brfss1[-train_index,]
brfss_test<- brfss1[train_index,]

# Displaying dimensions of the training and test sets
dim(brfss_train)
dim(brfss_test)

## Training logistic regression model
train_glm <- train(CVD ~ ., method = "glm", data = brfss_train)
glm_preds <- predict(train_glm, brfss_test)
mean(glm_preds == brfss_test$CVD)
confusionMatrix(reference = brfss_test$CVD, data = glm_preds)
varImp(train_glm)

## Training linear discriminant analysis model
train_lda <- train(CVD ~ ., method = "lda", data = brfss_train)
lda_preds <- predict(train_lda, brfss_test)
mean(lda_preds == brfss_test$CVD)
confusionMatrix(reference = brfss_test$CVD, data = lda_preds)
varImp(train_lda)

## Training quadratic discriminant analysis model
train_qda <- train(CVD ~ ., method = "qda", data = brfss_train)
qda_preds <- predict(train_qda, brfss_test)
mean(qda_preds == brfss_test$CVD)
confusionMatrix(reference = brfss_test$CVD, data = qda_preds)
varImp(train_qda)

## Testing the final model (QDA) on the validation dataset
val_qda <- train(CVD~., method = "qda", data = validation)
val_preds <- predict(val_qda, validation)
mean(val_preds == validation$CVD)
confusionMatrix(reference = validation$CVD, data = val_preds)
varImp(val_qda)

