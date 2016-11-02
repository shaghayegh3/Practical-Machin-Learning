library(rpart)
library(caret)
library(rattle)
library(randomForest)
library(RColorBrewer)
library(knitr)
library(rpart.plot)
set.seed(1200)

setwd("E:/Shaghayegh/Coursera")
training <- read.csv("pml-training.csv")
testing <- read.csv("pml-testing.csv")

#data cleaning
#  remove NA,remove low variance variables

training <- training[, colSums(is.na(training)) == 0]

nzv <- nearZeroVar(training, saveMetrics=TRUE)
training <- training[,nzv$nzv==FALSE]

dim(training)

# Split data
train <- createDataPartition(training$classe,p=0.6,list=FALSE)
trainset <- training[train, ]
testset <- training[-train, ]


trainset <- trainset[c(-1)]
clean1 <- colnames(trainset)
clean2 <- colnames(trainset[, -58]) #remove classe column
testset <- testset[clean1]
testing <- testing[clean2]

for (i in 1:length(testing) ) {
  for(j in 1:length(trainset)) {
    if( length( grep(names(trainset[i]), names(testing)[j]) ) == 1)  {
      class(testing[j]) <- class(trainset[i])
    }      
  }      
}
testing <- rbind(trainset[2, -58] , testing)
testing <- testing[-1,]

# To get the same class between testing and myTraining
testing <- rbind(myTraining[2, -58] , testing)
testing <- testing[-1,]
      
# Decision tree prediction
#cvfold <- trainControl(method = "cv", number = 5)
model1 <- rpart(classe ~ ., data=trainset, method="class")
fancyRpartPlot(model1)
predict1 <- predict(model1, testset, type = "class")
(conf1 <- confusionMatrix(predict1, testset$classe))
(accuracy1<- conf1$overall[1])


# Random Forest prediction
#cvfold <- trainControl(method = "cv", number = 5)
model2 <- randomForest(classe ~ ., data=trainset, method="class")

predict2 <- predict(model2, testset, type = "class")
(conf2 <- confusionMatrix(predict2, testset$classe))
(accuracy2<- conf2$overall[1])

predict(model2, testing, type = "class")

##Prediction on testing set

predict(model2, testing, type = "class")



