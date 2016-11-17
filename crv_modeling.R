library(data.table)
library(ggplot2)
library(dplyr)
library(MLmetrics)
library(cvTools)

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

# Manually set input directory
crv.train <- fread("crv_train.csv")
crv.train <- select(crv.train, -V1)   # remove the index column which automatically gets added

# Look into biglm for huge datasets
# Two step model will be used: 
# 1. Does person spend time > 0 on page
# 2. How much time does person spend on page

# preprocessing step: add new column which is a binarized version (1 if nonzero) of time on page
crv.train <- mutate(crv.train, notBounced = ifelse(timeOnPage > 0, 1, 0))

# PART 1: FINDING THE BET MODEL FOR PREDICTING WHETHER PEOPLE STAY ON PAGE OR NOT
X <- select(crv.train, -uuid, -publish_time, -geo_location)   # the left out covariates take the longest time (-publish_time,-uuid, geo_location)
factor <- notBounced ~ . 
binary_timeOnPage <- glm(factor, data = X)

# make predictions using trained classifier
y_pred <- predict(binary_timeOnPage, crv.train)  # these are probabilities
y_true <- crv.train$notBounced

# Now will find the ROC by changing thresholds
findROC = function(thresh = 0.5) {
  # apply this function to the predicted values, and return a numeric vector
  y_observed <- as.numeric(lapply(y_pred, function(x) {makePrediction(x)}))
  
  # Generate confusion matrix
  cm <- ConfusionMatrix(y_observed, y_true)
  # print(cm)
}

# lm1.cv <- cvFit(lm1, data=crv.train, y=crv.train$timeOnPage, K=10)
# print(lm1.cv)