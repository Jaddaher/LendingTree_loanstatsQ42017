### add libraries
library(plyr)
library(dplyr)
library(tidyr)
library(splines)
library(bitops)
library(caTools)
library(stats)
library(base)
library(ggvis)
library(EDAWR)
library(devtools)
library(ggplot2)
library(base)
####check directory,get and view dataset####
  getwd()
  dataset = read.csv('LoanStats_2017Q4.csv',header=T,sep=',')
  View(dataset)

####CLEANING###
  ##remove unwanted columns#
      dataset =dataset[,c(-(1:3),-5,-11,-(17:21),-23,-(25:27),-29,-30,-36,-38,-40,-(43:145))]
  ##Find/Remove/Replace NA and ""
      which(dataset$term == "")
      which(dataset[6] == "") ### maybe run a loop to check on ""
      #sum(is.na(dataset[5]))  ### run a loop on space
      #dataset= na.omit(dataset)
      dataset = dataset[c(-118649,-118650),]
      dataset$revol_util = ifelse(dataset$revol_util == "",0,dataset$revol_util)

###SPLIT columns/Substitue values###
  dataset = separate(data = dataset,col="term",into= c("term1","term2"),sep=' ')
  dataset = separate(data = dataset,col="issue_d",into= c("issue_d1","issue_2"),sep='-')

  for(i in c("A","B","C","D","E","F","G")){
   dataset$sub_grade = gsub(i,"",dataset$sub_grade)
  }
  for(i in c("s","<"," year","+")){
   dataset$emp_length= gsub(i,"",dataset$emp_length, fixed = TRUE)
  }
  
  dataset$emp_length = gsub("n/a","0",dataset$emp_length)
  dataset$verification_status = gsub("Source Verified","Verified",dataset$verification_status)



####SECOND ROUND CLEANING####
dataset =dataset[,c(-2,-13)]

####New Dataset for Analysis####
dataset_a = dataset
View(dataset_a)

####encoding categorical data#### 
  #for factor columns # dataset_a$grade = factor(dataset_a$grade, levels = c(levels(dataset_a$grade)), labels= c(1:length(levels(dataset_a$grade))))
  #for non-factor columns #dataset_a$verification_status = factor(dataset_a$verification_status, levels = c(levels(as.factor(dataset_a$verification_status))), labels= c(1:length(levels(as.factor(dataset_a$verification_status)))))
  for(i in c(5,8,10,11,12,13)){
    dataset_a[,i] = as.numeric(as.factor(dataset_a[,i]),levels = c(levels(as.factor(dataset_a[,i]))), labels= c(1:length(levels(as.factor(dataset_a[,i])))))
    }


###FORMAT### 
  for(i in c(1:ncol(dataset_a))) {
    dataset_a[,i] <- as.numeric(dataset_a[,i])
  }



###**** SPlit Data into training set and test set ****
set.seed(1234)
split = sample.split(dataset_a$int_rate, SplitRatio = 0.8)
training_set =subset(dataset_a, split == TRUE)
test_set =subset(dataset_a, split == FALSE)

###****Feature scaling**** ONLY numeric, NO factors
#training_set[,c(1:23)] = scale(training_set[,c(1:23)])


### Regressor/ predict with training_set
regressor = lm(formula = (int_rate~. ), data = training_set)
summary(regressor)
y_predict = predict(regressor, newdata = test_set)
summary(y_predict)
View(y_predict)

regressor = lm(formula = (int_rate~.-addr_state), data = training_set)
summary(regressor)
y_predict1 = predict(regressor, newdata = test_set)
summary(y_predict1)
View(y_predict1)

### Regressor/ predict with whole set
regressor = lm(formula = (int_rate~. ), data = dataset_a)
summary(regressor)
y_predict = predict(regressor, newdata = test_set)
summary(y_predict)
View(y_predict)

regressor = lm(formula = (int_rate~.-addr_state), data = dataset_a)
summary(regressor)
y_predict1 = predict(regressor, newdata = test_set)
summary(y_predict1)
View(y_predict1)

y_predict_t = as.data.frame(y_predict)
View(y_predict_t)

