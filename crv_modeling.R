library(data.table)
library(ggplot2)
library(dplyr)
library(MLmetrics)
library(cvTools)
library(glmnet)

# FUNCTION DEFINITIONS
# ----------------------------------------
# based on a threshold, decide which elements to call '0' and '1'
makePrediction = function(x, thresh = 0.5) {
  # @param x: This is the value/probability that your predictor outputs
  # @param thresh: the threshold that decides whether you classify as 1 or 0
  # Outputs 1 or 0 depending on whether value is more than thresh or not
  # make a prediction on the probability of y, return 1 if greater than thresh, 0 otherwise
  if (x >= thresh)
    return(1)
  else
    return(0)
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

# Now will find the ROC by changing thresholds
findCM = function(y_true, y, thresh) {
  # apply this function to the predicted values, and return a numeric vector
  y_observed <- as.numeric(lapply(y_pred, function(x) {makePrediction(x, thresh = thresh)}))
  
  # return confusion matrix
  cm <- ConfusionMatrix(y_observed, y_true)
  # print(cm)
  return(cm)
}
# ----------------------------------------

# Manually set input directory
crv.master <- fread("crv_train.csv")
crv.master <- select(crv.master, -V1)   # remove the index column which automatically gets added

# Two step model will be used: 
# 1. Does person spend time > 0 on page
# 2. How much time does person spend on page

# preprocessing step: add new column which is a binarized version (1 if nonzero) of time on page
crv.master <- mutate(crv.master, notBounced = ifelse(timeOnPage > 0, 1, 0))

# PART 1: FINDING THE BEST MODEL FOR PREDICTING WHETHER PEOPLE STAY ON PAGE OR NOT
# TODO: Running lasso/ridge
# the left out covariates take the longest time (-publish_time,-uuid, geo_location)
# timeOnPage is removed because it would be cheating
X <- select(crv.master, -uuid, -publish_time, -geo_location, -timeOnPage)  # this is our actual p+1 covariates set

# Divide into training and CV datasets
samp.ind = sample(nrow(X), 0.8*nrow(X))   # sample n row indices at random
train_set = X[samp.ind,]
test_set = X[-samp.ind,]

# Choose covariates through a variety of methods
# A. Forward stepwise selection - on logistic regression
# min.model <- lm(notBounced ~ 1, data=train_set)
# biggest <- formula(lm(notBounced ~ .:., train_set)) # first and second order terms
min.model <- glm(notBounced ~ 1, data = train_set)
biggest <- formula(glm(notBounced ~ .:., train_set, family = binomial()))
fwd.model = step(min.model, direction='forward', scope=biggest)
summary(fwd.model)

# B. Backward stepwise selection
lmfull1 <- lm(notBounced ~ ., data=train_set)
lmfull2 <- lm(notBounced ~ .:., data=train_set)
bkwd.model = step(lmfull2, direction='backward')

# C. Identification of best covariates using regsubsets
library(leaps)
allsubsets <- regsubsets(notBounced ~ ., data=train_set, nbest=10)

# D. Regression with regularization
X <- model.matrix(notBounced ~ ., train_set)
y <- train_set$notBounced
grid=10^seq(2, -2, length = 100)  # choosing lambda in the range of -2 to 10
ridge.mod=glmnet (X, y, alpha=0, lambda=grid)   # alpha=0, hence ridge model is fit
plot(ridge.mod, xvar="lambda", col=1:dim(coef(ridge.mod))[1], ylim = c(-0.3, 0)) # Get the plot of coefficients w.r.t. lambda
lbs_fun('topright', ridge.mod)

# Find training error and CV error for the top 10 most promising models
# create list of models, iterate over them
# manually enter preferred models
m3 <- notBounced ~ document_id + platform + display_id
m4 <- notBounced ~ document_id + platform + display_id + document_id:display_id
m5 <- notBounced ~ document_id + platform + display_id + topic_id + 
  document_id:display_id
m6 <- notBounced ~ document_id + platform + display_id + topic_id + 
  publisher_id + document_id:display_id
m7 <- notBounced ~ document_id + platform + display_id + topic_id + 
  publisher_id + document_id:display_id + document_id:topic_id
m8 <- notBounced ~ document_id + platform + display_id + topic_id + 
  publisher_id + topic_confidence_level + category_confidence_level + 
  document_id:display_id + document_id:topic_id + platform:topic_confidence_level + 
  document_id:topic_confidence_level + document_id:category_confidence_level
m9 <- notBounced ~ .
m10 <- notBounced ~ document_id + platform + display_id + topic_id + 
  publisher_id + topic_confidence_level + category_confidence_level + 
  document_id:display_id + document_id:topic_id + platform:topic_confidence_level + 
  document_id:topic_confidence_level + document_id:category_confidence_level + 
  platform:category_confidence_level + publisher_id:category_confidence_level + 
  publisher_id:topic_confidence_level + topic_id:category_confidence_level + 
  topic_id:publisher_id
m1 <- notBounced ~ document_id + platform + display_id + topic_id + 
  publisher_id + topic_confidence_level + monthDay + traffic_source
m2 <- notBounced ~ document_id + platform + display_id + topic_id + 
  publisher_id + topic_confidence_level
m3 <-notBounced ~ document_id + platform + display_id + topic_id + 
  publisher_id
models_list <- list(m1, m2, m3, m4, m5, m6, m7, m8, m9, m10)

y_true <- train_set$notBounced
j = 0
for (m in models_list) {
  # iterate over all models, train
  binary_timeOnPage <- glm(m, data = train_set)  
  y_pred_train <- predict(binary_timeOnPage, train_set, type = "response")  # these are probabilities
  
  # Find training error and testing error for each of these models
  # Convert to 0 and 1 based on threshold level 0.5 (as dataframe, then convert back into numeric vector)
  Y_train <- select(mutate(as.data.frame(y_pred_train), predicted = ifelse(y_pred_train > 0.5, 1, 0)), predicted)
  training_error <- sum(abs(Y_train - train_set$notBounced))/nrow(train_set)
  cat('model', j, 'training error:', training_error, '\n')
  
  y_pred_test <- predict(binary_timeOnPage, test_set, type = "response")
  Y_test <- select(mutate(as.data.frame(y_pred_test), predicted = ifelse(y_pred_test > 0.5, 1, 0)), predicted)
  testing_error <- sum(abs(Y_test - test_set$notBounced))/nrow(test_set)
  cat('model', j, 'testing error:', testing_error, '\n')
  
  
  # Now that models are fit, we will generate an ROC curve to compare them
  # generate threshold values in range(-1, 1) and plot roc
  thresh_range <- seq(0,1,0.01)
  
  # generate variables which will hold the TPR and FPR values that we will plot for the ROC curve
  tpr <- rep(0, length(thresh_range))
  fpr <- rep(0, length(thresh_range))
  for (i in 1:length(thresh_range)) {
    # cm <- findCM(y_pred = y_pred_train, thresh = thresh_range[i])
    # extract values of confusion matrix
    # tn = cm[1]
    # fn = cm[2]
    # fp = cm[3]
    # tp = cm[4]
    
    # find the TPR and FPR
    # tpr[i] <- tp/(tp + fn)
    # fpr[i] <- fp/(fp + tn)
  }
  # plot the ROC curve
  # g <- g + geom_line(aes_string(x=fpr, y=tpr)) + xlim(0,1) + ylim(0,1) + geom_abline(slope = 1, intercept = 0, color='blue')
  j = j + 1
}

# Now find the plot of False negative rate vs. lambda
thresh_range <- seq(0,1,0.01)
binary_timeOnPage <- glm(m9, data = train_set)  # best model
y_pred <- predict(binary_timeOnPage, test_set, type = "response") # predicted response
y_truth <-  as.numeric(test_set$notBounced)
fnr <- rep(0, length(thresh_range))
y_zeroOne_bestModel <- rep(0, length(thresh_range))
for (i in 1:length(thresh_range)) {
  # find FNR and 0-1 loss on test set only
  y_zeroOne_bestModel[i] <- mean(as.numeric(lapply(y_pred, function(x) {makePrediction(x, thresh = thresh_range[i])})))
  cm <- findCM(y = y_pred, y_true = y_truth, thresh = thresh_range[i])
  # extract values of confusion matrix
  tn = cm[1]
  fn = cm[2]
  fp = cm[3]
  tp = cm[4]
  
  # find the TPR and FPR
  fnr[i] <- fn/(tp + fn)
}
# plot the FPR vs lambda curve  and the 0-1 loss vs lambda curve
ggplot() + geom_line(aes(x=thresh_range, y=fnr, color = 'FNR')) + geom_line(aes(x=thresh_range, y=y_zeroOne_bestModel, color='0-1 LOSS')) + xlim(0,1) + ylim(0,1)

# PART 2: 
# linear regression on the same matrix for those values for which the time on page != 0
# Given that time on page != 0, what is the time on page?
# Use the 'Oracle' of part 1 as the input for training
timePage.train <- filter(crv.master, notBounced == 1)
timePage.train <- select(timePage.train, -notBounced)  # get rid of the variable as it is not being used
# y_true <- as.numeric(timePage.train$timeOnPage)

# train linear regression models on it
X_linear <- select(timePage.train, -uuid, -publish_time, -geo_location)   # the left out covariates take the longest time (-publish_time,-uuid, geo_location)
# fit <- glmnet(X_linear, y_true, family="gaussian", alpha=0, lambda=0.001)
lm_ols <- lm(formula = timeOnPage ~ ., data = X_linear)
# print(summary(lm1))
# lm_ridge <- lm.ridge(formula = timeOnPage ~ ., data = X_linear, lambda = 0.5)
# Using glmnet from http://www-bcf.usc.edu/~gareth/ISL/ISLR%20Sixth%20Printing.pdf
# create new X and y for training
X <- model.matrix(timeOnPage ~ ., X_linear)
y <- X_linear$timeOnPage

grid=10^seq(10,-2, length = 100)  # choosing lambda in the range of -2 to 10
ridge.mod=glmnet (X,y,alpha=0, lambda=grid)   # alpha=0, hence ridge model is fit
plot(ridge.mod, xvar="lambda", col=1:dim(coef(ridge.mod))[1]) # Get the plot of coefficients w.r.t. lambda
lbs_fun(ridge.mod)

grid=10^seq(10,-2, length = 100)
lasso.mod = glmnet (X,y,alpha=1, lambda=grid)   # alpha=1, hence lasso model is fit
plot(lasso.mod, xvar="lambda", col=1:dim(coef(lasso.mod))[1])  # Get the 
lbs_fun(lasso.mod)

