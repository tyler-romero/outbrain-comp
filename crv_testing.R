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
binary_timeOnPage <- glm(m, data = X_train)  

# SECOND, reading in and converting the data into the format usable by the models
testData <- fread("crv_test.csv")
testData <- select(testData, -V1)  # remove the index column which automatically gets added

# adding the notBounced column
testData <- mutate(testData, notBounced = ifelse(timeOnPage > 0, 1, 0))

# Finally, prepare the design matrix X
X_test <- select(testData, -uuid, -publish_time, -geo_location, -timeOnPage)


# Running the model on the test data

# --------------------------------------------------------------------------------
## Part 2: 
# --------------------------------------------------------------------------------
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

# --------------------------------------------------------------------------------