#install packages
install.packages('rpart')
install.packages('rattle')
install.packages('RColorBrewer')
library(rattle)
library(rpart.plot)
library(RColorBrewer)

## read .csv files
test <- read.csv("test.csv", stringsAsFactors = FALSE)
train <- read.csv("train.csv", stringsAsFactors = FALSE)

##create "Survived" column for test dataset with 0 in each rows
test$Survived <- rep(0,418)
submit <- data.frame(PassengerId = test$PassengerId, Survived = test$Survived)

##using train dataset to run the % of survival rate in different varible combination
fit <- rpart(Survived ~ Pclass + 
                 Sex + Age + 
                 SibSp + Parch + 
                 Fare + Embarked, 
             data=train, method="class")
fancyRpartPlot(fit)

##apply the % of survival rate on test data
prediction <- predict(fit, test,type = "class")
submit <- data.frame(PassengerId = test$PassengerId, Survived = prediction)

##save the table as new .csv file
write.csv(submit, file = "first_decision_tree_outcome.csv", row.names = FALSE)