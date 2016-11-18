library(data.table)
library(ggplot2)
library(dplyr)
library(e1071)
library(cvTools)
library(MLmetrics)

#input_directory <- ".\\input"
#setwd(input_directory)

#----------- Helper Functions ------------
#Prediction Threshold for naive bayes
predictThresh <- function(prediction, K) {
  df <- as.data.frame(prediction)
  return(as.numeric(df[2]/df[1] > K))
}
predictThreshGlm <- function(prediction, K) {
  return(as.numeric(prediction > K))
}

#-----------------------------------------


brv <- fread("brv.csv")
brv <- select(brv, -c(V1))
brv$clicked <- as.factor(brv$clicked)

#split data
set.seed(851)
train.ind = sample(nrow(brv), 4*round(nrow(brv)/5)) #80% of data is for training, 20% for test
brv.train = brv[train.ind,]
brv.test = brv[-train.ind,]

f1 <- factor(clicked) ~ platform + geo_location + traffic_source + factor(weekDay) + loadTimestamp +
  display_id + document_id + ad_id

glmROC <- function() {
  glm.m1 <- glm(f1, data = brv.train, family = binomial())
  rawPre <- predict(binomial.glm1, brv.train, type="response")
  glm.roc <- data.frame()
  
  for(i in seq(0, 1, 0.01)){
    pre1 <- predictThreshGlm(rawPre, K=i)
    cm <- as.matrix(ConfusionMatrix(y_pred = pre1, y_true = brv.train$clicked))
    if(!("0" %in% colnames(cm))) cm <- cbind(c(0,0),cm)
    else if(!("1" %in% colnames(cm))) cm <- cbind(cm, c(0,0))
    print(cm)
    tn <- cm[1]
    fn <- cm[2]
    fp <- cm[3]
    tp <- cm[4]
    tpr <- tp/(tp+fn)
    fpr <- fp/(fp+tn)
    zeroOne <- ZeroOneLoss(pre1, brv.train$clicked)
    newRow <- data.frame(thresh=i, tn=tn, fp=fp, fn=fn, tp=tp, tpr=tpr, fpr=fpr, zo=zeroOne)
    
    glm.roc <- bind_rows(glm.roc, newRow)
  }
  return(glm.roc)
}
#glm.roc <- glmROC()
#loss.plot <- ggplot(glm.roc, aes(thresh, zo)) + geom_point() + xlim(0,1)
#print(loss.plot)
#roc.plot <- ggplot(glm.roc, aes(fpr, tpr)) + geom_point() + xlim(0,1) + ylim(0,1) +geom_abline(slope=1, intercept=0, color="blue")
#print(roc.plot)


naivebayesROC <- function() {
  naiveBayes.m1 <- naiveBayes(f1, data=brv.train)
  rawPre <- predict(naiveBayes.m1, brv.train, type = "raw")
  bayes.roc <- data.frame()
  
  for(i in seq(0.2, 0.8, 0.005)){
    pre1 <- predictThresh(rawPre, K=i)
    cm <- as.matrix(ConfusionMatrix(y_pred = pre1, y_true = brv.train$clicked))
    if(!("0" %in% colnames(cm))) cm <- cbind(c(0,0),cm)
    else if(!("1" %in% colnames(cm))) cm <- cbind(cm, c(0,0))
    print(cm)
    tn <- cm[1]
    fn <- cm[2]
    fp <- cm[3]
    tp <- cm[4]
    tpr <- tp/(tp+fn)
    fpr <- fp/(fp+tn)
    zeroOne <- ZeroOneLoss(pre1, brv.train$clicked)
    newRow <- data.frame(thresh=i, tn=tn, fp=fp, fn=fn, tp=tp, tpr=tpr, fpr=fpr, zo=zeroOne)
    
    bayes.roc <- bind_rows(bayes.roc, newRow)
  }
  return(bayes.roc)
}
#bayes.roc <- naivebayesROC()
#loss.plot <- ggplot(bayes.roc, aes(thresh, zo)) + geom_point() + xlim(0,1)
#print(loss.plot)
#roc.plot <- ggplot(bayes.roc, aes(fpr, tpr)) + geom_point() + xlim(0,1) + ylim(0,1) +geom_abline(slope=1, intercept=0, color="blue")
#print(roc.plot)

svmROC <- function() {
  svm.roc <- data.frame()
  
  for(i in seq(4.5, 5, 0.1)){
    svm.m1 <- svm(f1, data=brv.train, class.weights = c("0"=1, "1"=i))
    pre1 <- predict(svm.m1, brv.train)
    cm <- as.matrix(ConfusionMatrix(y_pred = pre1, y_true = brv.train$clicked))
    if(!("0" %in% colnames(cm))) cm <- cbind(c(0,0),cm)
    else if(!("1" %in% colnames(cm))) cm <- cbind(cm, c(0,0))
    print(cm)
    tn <- cm[1]
    fn <- cm[2]
    fp <- cm[3]
    tp <- cm[4]
    tpr <- tp/(tp+fn)
    fpr <- fp/(fp+tn)
    zeroOne <- ZeroOneLoss(pre1, brv.train$clicked)
    newRow <- data.frame(thresh=i, tn=tn, fp=fp, fn=fn, tp=tp, tpr=tpr, fpr=fpr, zo=zeroOne)
    
    svm.roc <- bind_rows(svm.roc, newRow)
  }
  return(svm.roc)
}
svm.roc <- svmROC()
loss.plot <- ggplot(svm.roc, aes(thresh, zo)) + geom_point() + xlim(0,1)
print(loss.plot)
roc.plot <- ggplot(svm.roc, aes(fpr, tpr)) + geom_point() + xlim(0,1) + ylim(0,1) + geom_abline(slope=1, intercept=0, color="blue")
print(roc.plot)



