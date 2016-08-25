######## RPART - TITANIC SCRIPT ########

#Load Libraries
library(rpart)

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
                "embarked",
                "survived")
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
fit <- rpart(survived ~ pclass + sex + age + sibsp + parch + fare + embarked, 
             data=extractFeatures(train),
             method="class",
             control = rpart.control(cp = 0.00001))

quartz(height=7,width=7)
par(xpd=NA)
plot(fit,branch=1,uniform=TRUE)
text(fit,use.n=TRUE,pretty=TRUE)

#Prediction on Training & Test
train$psurv <- predict(fit,type="class")
test$psurv <- predict(fit, extractFeatures(test),type="class")

#Confusion Matrix
library(caret)
library(e1071)

confusionMatrix(train$psurv, train$survived, positive="1")
confusionMatrix(test$psurv, test$survived, positive="1")


#Pruning the Tree
plotcp(fit)

#From the Help File
#"A good choice of cp for pruning is often the leftmost value for which the mean lies below the horizontal line"
printcp(fit)

pfit <- prune(fit,cp=fit$cptable[3,1])

quartz(height=7,width=7)
par(xpd=NA)
plot(pfit,branch=1,uniform=TRUE)
text(pfit,use.n=TRUE,pretty=TRUE)

test$psurv.prune <- predict(pfit, extractFeatures(test),type="class")
conf.prune <- confusionMatrix(test$psurv.prune, test$survived, positive="1")
conf.prune