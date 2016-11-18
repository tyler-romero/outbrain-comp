library(data.table)
library(ggplot2)
library(dplyr)
library(MLmetrics)
library(cvTools)
library(glmnet)

# FUNCTION DEFINITION
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
lbs_fun <- function(fit, ...) {
  L <- length(fit$lambda)
  x <- log(fit$lambda[L])
  y <- fit$beta[, L]
  labs <- names(y)
  text(x, y, labels=labs, ...)
  legend('topleft', legend=labs, col=1:6, lty=1)
}

# Manually set input directory
crv.master <- fread("crv_train.csv")
crv.master <- select(crv.master, -V1)   # remove the index column which automatically gets added

# Look into biglm for huge datasets
# Two step model will be used: 
# 1. Does person spend time > 0 on page
# 2. How much time does person spend on page

# preprocessing step: add new column which is a binarized version (1 if nonzero) of time on page
crv.master <- mutate(crv.master, notBounced = ifelse(timeOnPage > 0, 1, 0))

# PART 1: FINDING THE BEST MODEL FOR PREDICTING WHETHER PEOPLE STAY ON PAGE OR NOT
# TODO: Running lasso/ridge
# the left out covariates take the longest time (-publish_time,-uuid, geo_location)
# timeOnPage is removed because it would be cheating
X <- select(crv.master, -uuid, -publish_time, -geo_location, -timeOnPage)

# Divide into training and CV datasets
samp.ind = sample(nrow(X), 0.8*nrow(X))   # sample n row indices at random
train_set = X[samp.ind,]
test_set = X[-samp.ind,]

# Forward stepwise selection
min.model <- lm(notBounced ~ 1, data=train_set)
biggest <- formula(lm(y~., train_set))
fwd.model = step(min.model, direction='forward', scope=biggest)

# make predictions using trained classifier
factor <- notBounced ~ . 
binary_timeOnPage <- glm(factor, data = X)
y_pred <- predict(binary_timeOnPage, crv.train)  # these are probabilities
y_true <- crv.train$notBounced

# Now will find the ROC by changing thresholds
findCM = function(thresh) {
  # apply this function to the predicted values, and return a numeric vector
  y_observed <- as.numeric(lapply(y_pred, function(x) {makePrediction(x, thresh = thresh)}))
  
  # return confusion matrix
  cm <- ConfusionMatrix(y_observed, y_true)
  # print(cm)
  return(cm)
}

# generate threshold values in range(-1, 1) and plot roc
thresh_range <- seq(0,1,0.01)

# generate variables which will hold the TPR and FPR values that we will plot for the ROC curve
tpr <- rep(0, length(thresh_range))
fpr <- rep(0, length(thresh_range))
for (i in 1:length(thresh_range)) {
  cm <- findCM(thresh = thresh_range[i])
  # extract values of confusion matrix
  tn = cm[1]
  fn = cm[2]
  fp = cm[3]
  tp = cm[4]
  
  # find the TPR and FPR
  tpr[i] <- tp/(tp + fn)
  fpr[i] <- fp/(fp + tn)
}

# plot the ROC curve
ggplot() + geom_point(aes(x=fpr, y=tpr)) + xlim(0,1) + ylim(0,1) + geom_abline(slope = 1, intercept = 0, color='blue')

# Find in-sample cross validation error
# lm1.cv <- cvFit(lm1, data=crv.train, y=crv.train$timeOnPage, K=10)
# print(lm1.cv)

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

