
library(caTools) # split data as TEST and TRAIN
library(gmodels) # CrossTable analysis
library(corrplot)
library(caret)


setwd("/Users/sandeeptalapati/Desktop/airlinedata")
dataset = read.csv(file="/Users/sandeeptalapati/Desktop/airlinedata/voice.csv")
head(dataset)
str(dataset)
dataset$label <- as.character(dataset$label)
dataset$label[which(dataset$label=="male")]<-1
dataset$label[which(dataset$label=="female")]<-0
dataset$label <- as.numeric(dataset$label)
head(dataset)

LogRegFunc <- function(dataset)
{
  set.seed(777)
  data_split = sample.split(dataset$label, SplitRatio = 0.80)

  TrainSet = subset(dataset,data_split == TRUE)
  TestSet = subset (dataset, data_split == FALSE)

  nrow(TrainSet)
  nrow(TestSet)

  control <- trainControl(method="cv", number=12)
  metric <- "Accuracy"
  TrainSet$label <- as.factor(TrainSet$label)
  TestSet$label <- as.factor(TestSet$label)
  log_model <- train(label~., data=TrainSet, method="glm", metric=metric, trControl=control)
  predict_data <- predict(log_model, TestSet)
  acc <- confusionMatrix(predict_data, TestSet$label)$overall[1]
  print(acc)
  CrossTable(TestSet$label, predict_data, prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE, dnn = c("actual gender","predicted gender"))
}

LogRegFunc(dataset)

# correlation matrix - plot to find out correlation between featuresCompute the correlation matrix that describes the dependence between all predictors and identify the predictors that are highly correlated. Plot the correlation matrix 
crmat <- cor(dataset)
corrplot(crmat, method="circle")

#Based on correlation remove those predictors that are correlated 
dt <- within(dataset, rm(Q75,kurt,maxdom,centroid))
head(dt)

# Predicting the output after removing the unwanted features --> Calling prediction function
LogRegFunc(dt)

