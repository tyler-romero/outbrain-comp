library(data.table)
library(ggplot2)
library(dplyr)
library(MLmetrics)
library(leaps)
library(glmnet)

#input_directory <- ".\\input"
#setwd(input_directory)

#----------- Threshold Helper -----------------
predictThreshGlm <- function(prediction, K) {
  return(as.numeric(prediction > K))
}


#----------- Get the training and testing data  ------------
brv <- fread("brv.csv")
brv <- select(brv, -c(V1))
brv$clicked <- as.factor(brv$clicked)

#split data
set.seed(851)
train.ind = sample(nrow(brv), 4*round(nrow(brv)/5)) #80% of data is for training, 20% for test
brv.train = brv[train.ind,]
brv.test = brv[-train.ind,]


#----------- Choose Covariates for Logistic Regression ------------
#Remove HUGE factor covariates
brv.train <- select(brv.train, -publish_time, -geo_location)

# 1. Forward and Backwards Stepwise Selection
min.model <- glm(factor(clicked) ~ 1, data = brv.train, family = binomial())
biggest <- formula(glm(factor(clicked) ~ .:., brv.train, family = binomial()))
fwd.model <- step(min.model, direction='forward', scope=biggest)


#----------- Get Testing Error and Precision -------------
glm.m1 <- fwd.model
rawPre <- predict(glm.m1, brv.test, type="response")

threshold <- 0.27
pre1 <- predictThreshGlm(rawPre, K=threshold)

cm <- as.matrix(ConfusionMatrix(y_pred = pre1, y_true = brv.test$clicked))
if(!("0" %in% colnames(cm))){
  cm <- cbind(c(0,0),cm)
} else if(!("1" %in% colnames(cm))) {
  cm <- cbind(cm, c(0,0))
}
print(cm)

tn <- cm[1]
fn <- cm[2]
fp <- cm[3]
tp <- cm[4]
tpr <- tp/(tp+fn)
fpr <- fp/(fp+tn)
zeroOne <- ZeroOneLoss(pre1, brv.test$clicked)
precision = tp/(tp+fp)

print("Zero-One Testing Error:")
print(zeroOne)

print("Test Set Precision")
print(precision)





