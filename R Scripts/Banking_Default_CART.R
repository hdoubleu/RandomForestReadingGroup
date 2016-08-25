######## RPART - BANKING DEFAULT SCRIPT ########

#Load Libraries
library(rpart)

#Replicate Results
set.seed(1)
load("../Datasets/bankdefault.dat")
head(dat)

#table of obs over year
table(dat$year,dat$default)

#sample train and test 70/30
indx <- sample(1:nrow(dat),size=nrow(dat)*0.1,replace=F)
train <- dat[indx,]
test  <- dat[-indx,]

#Kick the data into shape (wrapped in a function)
extractFeatures <- function(data) {
    
  #coerce to factors
  data$year      <- as.factor(data$year)
  data$default   <- as.factor(data$default)
  return(data)
}

#Check if working
View(extractFeatures(train)[1:100,])

#Grow the Random Forest
fit <- rpart(default ~ creditScore + houseAge + yearsEmploy + ccDebt + year, 
             data=extractFeatures(train),
             method="class",
             control = rpart.control(cp = 0.00001))

quartz(height=7,width=7)
par(xpd=NA)
plot(fit,branch=1,uniform=TRUE)
#text(fit,use.n=TRUE,pretty=TRUE)

#Prediction on Training & Test
test$psurv <- predict(fit, extractFeatures(test),type="class")

#Confusion Matrix
library(caret)
library(e1071)

#LOL!
confusionMatrix(test$psurv, test$default, positive="1")

#Pruning the Tree
plotcp(fit)

#From the Help File
#"A good choice of cp for pruning is often the leftmost value for which the mean lies below the horizontal line"
printcp(fit)

pfit <- prune(fit,cp=fit$cptable[which.min(fit$cptable[,4]),1])

quartz(height=7,width=7)
par(xpd=NA)
plot(pfit,branch=1,uniform=TRUE)
text(pfit,use.n=TRUE,pretty=TRUE)

test$psurv.prune <- predict(pfit, extractFeatures(test),type="class")
confusionMatrix(test$psurv.prune, test$default, positive="1")
