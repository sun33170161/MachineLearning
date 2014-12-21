library(caret)

#Load data
training <- read.csv(file = "data/pml-training.csv", na.strings=c("NA", "", "#DIV/0!"))
testing <- read.csv(file = "data/pml-testing.csv", na.strings=c("NA", "", "#DIV/0!"))
summary(training)

#Remove unrelated features
trainingN <- subset(training, select = 
                      -c(X, user_name,
                         raw_timestamp_part_1, raw_timestamp_part_2, cvtd_timestamp,
                         new_window, num_window)                    
)

#Remove varible with too much na
var.bad <- function(x){sum(is.na(x)*1)>length(x)/2}
matrix.littleNA <- function(m){
  threshold = dim(m)[1]/2
  r = c()
  for( v in 1:dim(m)[2]){
    if(sum(is.na(m[, v])*1) < threshold) r[length(r)+1] = v
  }
  r
}
trainingN <- subset(trainingN, select = matrix.littleNA(trainingN)) 
summary(trainingN)

#Model building using random forest and cross validation
set.seed(16888)
inTrain <- createDataPartition(y=trainingN$classe, p=0.1, list=FALSE)
training.rf <- trainingN[inTrain, ]
testing.rf <- trainingN[-inTrain, ]
modFit <- train(classe ~ ., method="rf", trControl=trainControl(method="cv"), data=training.rf)
varImp(modFit)
print(modFit$finalModel)
print(modFit$resample)
plot(modFit)

#Check the accuracy
testing.predict <- predict(modFit, testing.rf)
confusionMatrix(testing.rf$classe, testing.predict)

#Prediction for the testing data
head(testing)
testing[, "classe"] = predict(modFit, testing)
testing[, c("problem_id", "classe")]