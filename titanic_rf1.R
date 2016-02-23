# Install and load required packages for decision trees and forests
library(rpart)
install.packages('randomForest')
library(randomForest)
install.packages('party')
library(party)

# Load data 
train <- read.csv("train.csv",stringsAsFactors = FALSE)
test <- read.csv("test.csv",stringsAsFactors = FALSE)

# combine two datasets
test$Survived <- NA
combi <- rbind(train, test)

# Convert to a string
combi$Name <- as.character(combi$Name)

# Engineered variable: Title
    ##sliced title
    combi$Title <- sapply(combi$Name, FUN=function(x) {strsplit(x, split='[,.]')[[1]][2]})
    combi$Title <- sub(' ', '', combi$Title)
    ##check how many of each possibile title
    table(combi$Title)
    
    ## Combine small title groups into Mrs and Mr, assuming Rev = Mr;Jonkheer = Mr;
    combi$Title[which(combi$Title %in% c('Mme', 'Mlle'))] <- 'Miss'
    combi$Title[which(combi$Title %in% c('Dona', 'Lady','Ms'))] <- 'Mrs'
    combi$Title[which(combi$Title %in% c('Capt', 'Col', 'Jonkheer', 'Don', 'Major', 'Rev','Sir'))] <- 'Mr'
    combi$Title[which(combi$Title == "Dr" & combi$Sex == "female")] <- 'Mrs'
    combi$Title[which(combi$Title == "Dr" & combi$Sex == "male")] <- 'Mr'
    
    ## Convert to a factor
    combi$Title <- factor(combi$Title)

# Engineered variable: Family size
    combi$FamilySize <- combi$SibSp + combi$Parch + 1

# Engineered variable: Family
    combi$Surname <- sapply(combi$Name, FUN=function(x) {strsplit(x, split='[,.]')[[1]][1]})
    combi$FamilyID <- paste(as.character(combi$FamilySize), combi$Surname, sep="")
    combi$FamilyID[combi$FamilySize <= 2] <- 'Small'
    # Delete erroneous family IDs
    famIDs <- data.frame(table(combi$FamilyID))
    famIDs <- famIDs[famIDs$Freq <= 2,]
    combi$FamilyID[combi$FamilyID %in% famIDs$Var1] <- 'Small'
    # Convert to a factor
    combi$FamilyID <- factor(combi$FamilyID)

# Fill in Age 263 NAs 1) by using median age 2) by using rpart to predict age
    summary(combi$Age)
    ##1 update unknown age by median age
    title.age <- aggregate(combi$Age, by = list(combi$Title), FUN = function(x) median(x,na.rm = T))
    combi[is.na(combi$Age), "Age"] <- apply(combi[is.na(combi$Age),], 1, function(x) title.age[title.age[,1]==x["Title"],2])
    ##2 update unknown age by using rpart
    #Agefit <- rpart(Age ~ Pclass + Sex + SibSp + Parch + Fare + Embarked + Title + FamilySize, 
    #                data=combi[!is.na(combi$Age),], method="anova")
    #combi$Age[is.na(combi$Age)] <- predict(Agefit, combi[is.na(combi$Age),])

# Check what else might be missing
summary(combi)
# Fill in Embarked blanks
    summary(combi$Embarked)
    which(combi$Embarked == '')
    combi$Embarked[c(62,830)] = "S"
    combi$Embarked <- factor(combi$Embarked)
# Fill in Fare NAs
    summary(combi$Fare)
    which(is.na(combi$Fare))
    combi$Fare[1044] <- median(combi$Fare, na.rm=TRUE)

# New factor for Random Forests, only allowed <32 levels, so reduce number
    combi$FamilyID2 <- combi$FamilyID
    # Convert back to string
    combi$FamilyID2 <- as.character(combi$FamilyID2)
    combi$FamilyID2[combi$FamilySize <= 3] <- 'Small'
    # And convert back to factor
    combi$FamilyID2 <- factor(combi$FamilyID2)

# Split back into test and train sets
    train <- combi[1:891,]
    test <- combi[892:1309,]
#set.seed(123)
#inTrain <- createDataPartition(train$Survived, p = 0.8)[[1]]

#Linear model
#fit.1 <- glm(Survived ~ Pclass+Sex+Age+SibSp+Parch+Embarked+Title, 
#             data=train[inTrain,], family=binomial(("logit")))
#summary(fit.1)
# Build Random Forest Ensemble
set.seed(415)
fit <- randomForest(as.factor(Survived) ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + Title + FamilySize + FamilyID2,
                    data=train, importance=TRUE, ntree=2000)
# Look at variable importance
varImpPlot(fit)
# Now let's make a prediction and write a submission file
Prediction <- predict(fit, test)
submit <- data.frame(PassengerId = test$PassengerId, Survived = Prediction)
write.csv(submit, file = "firstforest.csv", row.names = FALSE)

#plot the importance of each feature
imp <- importance(fit, type = 1)
fi <- data.frame(Feature = row.names(imp), importance = imp[,1])
library(ggplot2)
p <- ggplot(fi, aes(x = reorder(Feature, importance), y = importance)) +
    + geom_bar(stat = "identity", fill = "pink") + coord_flip() + theme_light(base_size = 20) + ggtitle("random forest feature importance") + theme(plot.title = element_text(size=18))
ggsave("Feature_Importance_titanic_rf1.png",p)
