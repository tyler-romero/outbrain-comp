library(data.table)
library(ggplot2)
library(dplyr)
library(e1071)
library(cvTools)
library(MLmetrics)
library(leaps)
library(glmnet)

#input_directory <- ".\\input"
#setwd(input_directory)

#----------- Get the training data  ------------
brv <- fread("brv.csv")
brv <- select(brv, -c(V1))
brv$clicked <- as.factor(brv$clicked)

#split data
set.seed(851)
train.ind = sample(nrow(brv), 4*round(nrow(brv)/5)) #80% of data is for training, 20% for test
brv.train = brv[train.ind,]
brv.test = brv[-train.ind,]
train.ind = sample(nrow(brv.train), 4*round(nrow(brv.train)/5)) #20% of data for cv hold out
brv.train = brv.train[train.ind,]
brv.holdout = brv.train[-train.ind,]


#----------- Helper Functions ------------
#Prediction Threshold for naive bayes
predictThresh <- function(prediction, K) {
  df <- as.data.frame(prediction)
  return(as.numeric(df[2]/df[1] > K))
}

predictThreshGlm <- function(prediction, K) {
  return(as.numeric(prediction > K))
}

# http://stackoverflow.com/questions/30566788/legend-label-errors-with-glmnet-plot-in-r
# prints the legend in the coefficients plot (for lasso and ridge)
lbs_fun <- function(legend_location, fit, ...) {
  L <- length(fit$lambda)
  x <- log(fit$lambda[L])
  y <- fit$beta[, L]
  labs <- names(y)
  text(x, y, labels=labs, ...)
  # legend(legend_location, legend=labs, col=1:6, lty=1)  # <- labels are hard to see, hence commented out
}

#----------- Compare Between Model Classes: ROC Plots ------------
f1 <- factor(clicked) ~ platform + geo_location + traffic_source + factor(weekDay) + loadTimestamp +
  display_id + document_id + ad_id

glmROC <- function() {
  glm.m1 <- glm(f1, data = brv.train, family = binomial())
  rawPre <- predict(glm.m1, brv.train, type="response")
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
  loss.plot <- ggplot(glm.roc, aes(thresh, zo)) +
    geom_point(aes(color=glm.roc$thresh)) +
    xlim(0,1) + ylim(0,1) +
    labs(color='Threshold') 
  print(loss.plot)
  roc.plot <- ggplot(glm.roc, aes(fpr, tpr)) +
    geom_point(aes(color=glm.roc$thresh)) +
    xlim(0,1) + ylim(0,1) +
    geom_abline(slope=1, intercept=0, color="blue") +
    labs(color='Threshold') 
  print(roc.plot)
  return(glm.roc)
}
#glm.roc <- glmROC()


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
  
  loss.plot <- ggplot(bayes.roc, aes(thresh, zo)) +
    geom_point(aes(color=bayes.roc$thresh)) +
    xlim(0,1) + ylim(0,1) + 
    labs(color='Threshold') 
  print(loss.plot)
  roc.plot <- ggplot(bayes.roc, aes(fpr, tpr)) +
    geom_point(aes(color=bayes.roc$thresh)) +
    xlim(0,1) + ylim(0,1) +
    geom_abline(slope=1, intercept=0, color="blue") +
    labs(color='Threshold') 
  print(roc.plot)
  return(bayes.roc)
}
#bayes.roc <- naivebayesROC()


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
  
  loss.plot <- ggplot(svm.roc, aes(thresh, zo)) +
    geom_point(aes(color=svm.roc$thresh)) +
    xlim(0,1) + ylim(0,1) +
    labs(color='Threshold') 
  print(loss.plot)
  roc.plot <- ggplot(svm.roc, aes(fpr, tpr)) +
    geom_point(aes(color=svm.roc$thresh)) +
    xlim(0,1) + ylim(0,1) +
    geom_abline(slope=1, intercept=0, color="blue") + 
    labs(color='Threshold') 
  print(roc.plot)
  return(svm.roc)
}
#svm.roc <- svmROC()

#The AOC of the ROC plots indicate that Logistic Regression and SVMs work better than Naive Bayes
#in this context. SVMs take prohibitively long to train on this data with more than
#a few covariates, so we will take Logisitic Regression as the best model class for
#this application

#We can also use ROC plots and Loss vs. Threshold plots to tune our threshold
#for this application, as we will see later.


#----------- Choose Covariates for Logistic Regression ------------
#Remove HUGE factor covariates
brv.train <- select(brv.train, -publish_time, -geo_location)

# 1. Forward and Backwards Stepwise Selection
min.model <- glm(factor(clicked) ~ 1, data = brv.train, family = binomial())
biggest <- formula(glm(factor(clicked) ~ .:., brv.train, family = binomial()))
fwd.model <- step(min.model, direction='forward', scope=biggest)
print(summary(fwd.model))


# 2. Lasso and Ridge Regularization
X <- model.matrix(factor(clicked) ~ ., brv.train)
y <- brv.train$clicked
grid=10^seq(2, -2, length = 100)  # choosing lambda in the range of -2 to 10

ridge = glmnet(X, y, alpha=0, lambda=grid, family="binomial")   # alpha=0 -> Ridge
plot(ridge, xvar="lambda", col=1:dim(coef(ridge))[1]) # Get the plot of coefficients w.r.t. lambda
lbs_fun('topright', ridge)

lasso = glmnet(X, y, alpha=1, lambda=grid, family="binomial")   # alpha=1 -> Lasso
plot(lasso, xvar="lambda", col=1:dim(coef(lasso))[1], xlim = c(-4.6, -3.5)) # Get the plot of coefficients w.r.t. lambda
lbs_fun('topright', lasso)

# - Forward stepwise selection finds that the model with the lowest AIC is one that 
#contains platform, source_id, publisher_id, display_id, ad_id, category_confidence_level,
#traffic_source, and several interaction terms.
# - Backwards stepwise selection immediately quits and returns clicked ~ 1.
# - Ridge Regularization shows that platformmobile, topic_confidence_level,
#category_confidence_level, traffic_sourcesearch, and traffic_sourcesocial are
#the most relevant covariates.
# - Lasso Regularization immediately pushes all coefficients to 0, except for 
#the coefficient for platformmobile.

#Based on the results above, we will go with the covariates returned by
#forward stepwise selection, because this model returns the lowest AIC, and
#it contains covariates that make sense intuitively.

#----------- Threshold for Logistic Regression: More ROC ------------
#Since we could achieve a zero-one loss of .2 simply by predicting 0 (no click),
#we believe it is important to examine confusion matricies for different thresholds.

#Since the goal is to predict when an Ad will be clicked on, it is not a big deal if
#we have false positives, as long as we have true positives to go along with them.
#Therefore, our goal is to optimize for precision, rather than zero-one loss

glmROCThresh <- function() {
  glm.m1 <- fwd.model
  rawPre <- predict(glm.m1, brv.train, type="response")
  glm.roc <- data.frame()
  for(i in seq(0, 1, 0.001)){
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
  loss.plot <- ggplot(glm.roc, aes(thresh, zo)) +
    geom_point(aes(color=glm.roc$thresh)) +
    xlim(0,1) + ylim(0,1) +
    labs(color='Threshold') 
  print(loss.plot)
  roc.plot <- ggplot(glm.roc, aes(fpr, tpr)) +
    geom_point(aes(color=glm.roc$thresh)) +
    xlim(0,1) + ylim(0,1) +
    geom_abline(slope=1, intercept=0, color="blue") +
    labs(color='Threshold') 
  print(roc.plot)
  return(glm.roc)
}
glm_final.roc <- glmROCThresh()

glm_final.roc <- mutate(glm_final.roc, precision = tp/(tp+fp))

precision.plot <- ggplot(glm_final.roc, aes(thresh, precision)) +
  geom_point()
print(precision.plot)
sensitivity.plot <- ggplot(glm_final.roc, aes(thresh, tpr)) +
  geom_point()
print(sensitivity.plot)

#We can achieve a precision of 0.5, for certain thresholds around 0.29, but this still results
#in very few positives total. Our chart of sensativity shows that our true positive rate
#is well below 0.1.

#In addition, we worry that a threshold that achieves a precision of 0.5 is subject to overfitting.
#Therefore, we will cross validate these results on our hold out set
glmROCHoldout <- function() {
  glm.m1 <- fwd.model
  rawPre <- predict(glm.m1, brv.holdout, type="response")
  glm.roc <- data.frame()
  for(i in seq(0.25, 0.3, 0.0001)){
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
glm_holdout.roc <- glmROCThresh()
glm_holdout.roc <- mutate(glm_holdout.roc, precision = tp/(tp+fp))

holdout.precision.plot <- ggplot(glm_holdout.roc, aes(thresh, precision)) +
  geom_point()
print(holdout.precision.plot)
holdout.sensitivity.plot <- ggplot(glm_holdout.roc, aes(thresh, tpr)) +
  geom_point() + xlim(.25,.3) + ylim(0,.1)
print(holdout.sensitivity.plot)
holdout.zo.plot <- ggplot(glm_holdout.roc, aes(thresh, zo)) +
  geom_point()
print(holdout.zo.plot)




