library(data.table)
library(ggplot2)
library(dplyr)
library(MLmetrics)
library(cvTools)
library(glmnet)
# Testing the continuous response variable model on the test data
# FUNCTION DEFINITIONS
# --------------------------------------------------------------------------------
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

# Now will find the Confusion Matrix by using a fixed threshold
findCM = function(y_true, y, thresh) {
  # apply this function to the predicted values, and return a numeric vector
  y_observed <- as.numeric(lapply(y_pred, function(x) {makePrediction(x, thresh = thresh)}))
  
  # return confusion matrix
  cm <- ConfusionMatrix(y_observed, y_true)
  # print(cm)
  return(cm)
}
# --------------------------------------------------------------------------------
# FIRST, read in and prepare the training data and re-train the best model
# --------------------------------------------------------------------------------
## Part 1: All covariates, lambda = 0.41
Part1_bestModel <- notBounced ~ .
lambda = 0.41

crv.master <- fread("crv_train.csv")
crv.master <- select(crv.master, -V1)   # remove the index column which automatically gets added

# preprocessing step: add new column which is a binarized version (1 if nonzero) of time on page
crv.master <- mutate(crv.master, notBounced = ifelse(timeOnPage > 0, 1, 0))

# the left out covariates take the longest time (-publish_time,-uuid, geo_location)
# timeOnPage is removed because it would be cheating
X_train <- select(crv.master, -uuid, -publish_time, -geo_location, -timeOnPage)  # this is our actual p+1 covariates set

# training the model
binary_timeOnPage <- glm(Part1_bestModel, data = X_train)  

# SECOND, reading in and converting the data into the format usable by the models
testData <- fread("crv_test.csv")
testData <- select(testData, -V1)  # remove the index column which automatically gets added

# adding the notBounced column
testData <- mutate(testData, notBounced = ifelse(timeOnPage > 0, 1, 0))

# Finally, prepare the design matrix X
X_test <- select(testData, -uuid, -publish_time, -geo_location, -timeOnPage)

# Running the model on the test data
y_pred <- predict(binary_timeOnPage, X_test, type = "response") # predicted response
y_truth <-  as.numeric(X_test$notBounced)
y_zeroOne <- mean(as.numeric(lapply(y_pred, function(x) {makePrediction(x, thresh = lambda)})))
cm <- findCM(y = y_pred, y_true = y_truth, thresh = lambda)
# get values of precision
tn = cm[1]
fn = cm[2]
fp = cm[3]
tp = cm[4]
print("Confusion Matrix:")
print(cm)
cat("FNR = ", fn/(tp + fn))

y_pred_train <- predict(binary_timeOnPage, X_train, type = "response")  # these are probabilities
Y_train <- select(mutate(as.data.frame(y_pred_train), predicted = ifelse(y_pred_train > 0.5, 1, 0)), predicted)
training_error <- sum(abs(Y_train - X_train$notBounced))/nrow(X_train)
cat('training error:', training_error, '\n')

y_pred_test <- predict(binary_timeOnPage, X_test, type = "response")
Y_test <- select(mutate(as.data.frame(y_pred_test), predicted = ifelse(y_pred_test > 0.5, 1, 0)), predicted)
testing_error <- sum(abs(Y_test - X_test$notBounced))/nrow(X_test)
cat('testing error:', testing_error, '\n')


# --------------------------------------------------------------------------------
## Part 2: 
# --------------------------------------------------------------------------------
# Our winning model...
Part2_bestModel <- timeOnPage ~ display_id + document_id + traffic_source +
category_confidence_level +
  platform + loadTimestamp + topic_id + topic_confidence_level +
  display_id:document_id + display_id:traffic_source +
  document_id:platform +
  display_id:platform + display_id:category_confidence_level +
  traffic_source:category_confidence_level + document_id:traffic_source
+
  document_id:loadTimestamp + document_id:category_confidence_level +
  platform:loadTimestamp + display_id:topic_id + document_id:topic_id +
  document_id:topic_confidence_level

# Generate the training set and re-train the model
timePage.train <- filter(crv.master, notBounced == 1)
timePage.train <- select(timePage.train, -notBounced)

X_linear_train <- select(timePage.train, -uuid, -publish_time, -geo_location)
y_linear_train <- X_linear_train$timeOnPage

# Prepare the test data
X_linear_test <- filter(testData, notBounced == 1)
y_linear_test <- X_linear_test$timeOnPage

# Train and make predictions!
lm_timeOnPage <- lm(Part2_bestModel, data = X_linear_train)  

# Find training error and testing error for each of these models
# Convert to 0 and 1 based on threshold level 0.5 (as dataframe, then convert back into numeric vector)
y_pred_train <- predict(lm_timeOnPage, X_linear_train, type = "response")  # these are probabilities
training_error <- sqrt(mean((y_pred_train - y_linear_train)^2))
cat('training error:', training_error, '\n')

y_pred_test <- predict(lm_timeOnPage, X_linear_test, type = "response")
testing_error <- sqrt(mean((y_pred_test - y_linear_test)^2))
cat('testing error:', testing_error, '\n')

# --------------------------------------------------------------------------------