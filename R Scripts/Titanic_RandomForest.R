######## SCRIPT DOWNLOADED FROM KAGGLE ########
# https://www.kaggle.com/benhamner/titanic/random-forest-benchmark-r/code

#Install Packages (IF NEEDED)
install.packages("ggplot2","randomForest","randomForest","caret","e1071")

#Load Libraries
library(ggplot2)
library(randomForest)


#Replicate Results
set.seed(1)
load("../Datasets/titanic3.sav")
titanic3 <- as.data.frame(titanic3)

#sample train and test 70/30
indx <- sample(1:nrow(titanic3),size=nrow(titanic3)*0.7,replace=F)
train <- titanic3[indx,]
test  <- titanic3[-indx,]

#Kick the data into shape (wrapped in a function)
extractFeatures <- function(data) {
  
  #extract features
  features <- c("pclass",
                "age",
                "sex",
                "parch",
                "sibsp",
                "fare",
                "embarked")
  fea <- data[,colnames(data) %in% features]
  
  #fix up missing values
  fea$age[is.na(fea$age)] <- -1
  fea$fare[is.na(fea$fare)] <- median(fea$fare, na.rm=TRUE)
  fea$embarked[is.na(fea$embarked)] <- "Southampton"
  
  #coerce to factors
  fea$sex      <- as.factor(fea$sex)
  fea$embarked <- as.factor(fea$embarked)
  return(fea)
}

#Check if working
View(extractFeatures(train))

#Grow the Random Forest
rf <- randomForest(extractFeatures(train), 
                   as.factor(train$survived), 
                   ntree=100, importance=TRUE)

#Prediction on Training & Test
train$psurv <- predict(rf)
test$psurv <- predict(rf, extractFeatures(test))

#Confusion Matrix
library(caret)
library(e1071)

confusionMatrix(train$psurv, train$survived, positive="1")
confusionMatrix(test$psurv, test$survived, positive="1")

#Feature Importance
imp <- importance(rf, type=1)
featureImportance <- data.frame(Feature=row.names(imp), Importance=imp[,1])

quartz(height=7,width=7)
#windows(height=7,width=7)
ggplot(featureImportance, aes(x=reorder(Feature, Importance), y=Importance)) +
        geom_bar(stat="identity", fill="#53cfff") +
        coord_flip() + 
        theme_light(base_size=20) +
        xlab("") +
        ylab("Importance") + 
        ggtitle("Random Forest Feature Importance\n") +
        theme(plot.title=element_text(size=18))
