library(caret)
library(randomForest)
library(tidyverse)

# Load dataset
getwd()
setwd('/Users/mayadempsey/Downloads')
data <- read.csv('loan_default_data_set.csv')



## Do EDA and report you findings.
summary(data)
##The data has 21 columns
ncol(data)
##the row count is 20,000 before cleaning the dataset
nrow(data)
view(data)

class(data$Def_ind)


#I decided that observations with any missing values should be removed
data1 <- data %>% filter( !is.na(pct_card_over_50_uti) & !is.na(rep_income))
summary(data1)
any(duplicated(data1))

#3 no duplicates but education is character in this dataset

#4 if someone wanted to remove duplicates in rows then they would use distinct() from dplyr, 
##for columns it would be unique() 

#5
plot(data1$avg_bal_cards,data1$tot_balance,main="C Avg Credit balance vs Total balance",xlab="Avg Credit balance", ylab="Total Balance",col="red")
# it can be seen in the graph that there is a positive linear reltionship between the average credit balance and total balance which would be intutive 


#6 
count(data1,rep_education)
#it looks as though "other" is misrepresented following graduate and then highschool

#7
count(data1,Def_ind)
# this data is far from being balanced, it can be helped by using the AUC/ROC method, or could be helped by using a 
#logisitc regression model

#8
summary(data1$rep_income)
#I would say that the rep income data is approximately normal 

#9
default_rates<-data1%>%
  group_by(rep_education)%>%
  summarize(
    total_count=n(),
    default_count=sum(Def_ind),
    default_rate=mean(Def_ind)*100)
default_rates
show(summary)
## I am not sure where my error is

# Split dataset
set.seed(2)
trainIndex <- createDataPartition(data1$Def_ind, p=0.8, list=FALSE)
train <- data1[trainIndex, ]
test <- data1[-trainIndex, ]

set.seed(2)
split <- sample.split(data1$Def_ind,SplitRatio = 0.8)
train <- subset(data1,split == TRUE)
test <- subset(data1,split == FALSE)


# Train and evaluate KNN
knn_model <- train(Def_ind ~ ., data=train, method='knn', tuneLength=5) # Fit KNN model
pred_knn <- predict(knn_model, test)
print(confusionMatrix(pred_knn, test$Def_ind))
knn_model <- train(Def_ind ~ ., data=train, method='knn', tuneLength=5)
pred_knn <- predict(knn_model, test)
print(confusionMatrix(pred_dt, test$y_variable))
print(confusionMatrix(pred_knn, test$Def_ind))


# Train and evaluate Decision Tree
dt_model <- train(Def_ind ~ ., data=train, method='rpart') # Fit Decision Tree Model
pred_dt <- predict(dt_model, test)
library(rpart)
library(rpart.plot)
data1<-(Def_ind~ tot_balance)




## Proceed with evaluation and interpretation of both models.
