library(data.table)
library(ggplot2)
library(dplyr)
library(MLmetrics)
library(leaps)
library(glmnet)
library(boot)
library(MASS)

input_directory <- ".\\input"
setwd(input_directory)

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
brv.test <- select(brv.test, -publish_time, -geo_location)

#Generate model using forward stepwise selection
min.model <- glm(factor(clicked) ~ 1, data = brv.train, family = binomial())
biggest <- formula(glm(factor(clicked) ~ .:., brv.train, family = binomial()))
fwd.model <- step(min.model, direction='forward', scope=biggest)


#----------- Get Testing Error and Precision -------------
glm.train <- fwd.model
rawPre <- predict(glm.train, brv.test, type="response")

threshold <- 0.27
pre1 <- predictThreshGlm(rawPre, K=threshold)

cm <- as.matrix(ConfusionMatrix(y_pred = pre1, y_true = brv.test$clicked))
if(!("0" %in% colnames(cm))){
  cm <- cbind(c(0,0),cm)
} else if(!("1" %in% colnames(cm))) {
  cm <- cbind(cm, c(0,0))
}
fp <- cm[3]
tp <- cm[4]

zeroOne <- ZeroOneLoss(pre1, brv.test$clicked)
precision = tp/(tp+fp)

print("Zero-One Testing Error:")
print(zeroOne)

print("Test Set Precision")
print(precision)

#Formula chosen by forward stepwise selection
fss <- factor(clicked) ~ platform + source_id + publisher_id + 
  display_id + ad_id + category_confidence_level + traffic_source + 
  platform:publisher_id + display_id:ad_id + platform:category_confidence_level + 
  ad_id:category_confidence_level + platform:source_id + publisher_id:ad_id + 
  publisher_id:category_confidence_level


glm.test <- glm(formula = fss, family = binomial(), data = brv.test)

glm.all <- glm(formula = factor(clicked) ~ 1 + ., family = binomial(), data = brv.test)

print("Coefficients of glm on training data")
print(summary(glm.train))
print("Coefficients of glm on testing data")
print(summary(glm.test))
print("All available coefficients on training data")
print(summary(glm.all))


#--------- Bootstrapping Estimation of Coefficient Confidence Intervals -------------

#Boostrap estimation of regression coefficient confidence intervals
#From http://www.statmethods.net/advstats/bootstrapping.html
bs <- function(formula, data, indices) {
  d <- data[indices,] # allows boot to select sample 
  fit <- glm(formula, family = binomial(), data = d)
  return(coef(fit)) 
}

results <- boot(data = brv.train, statistic = bs, R = 10, formula =  fss)

print(fss)
print(results)

# get 95% confidence intervals 
for(i in 1:20) {
  print(i)
  print(boot.ci(results, type="norm", index=i))
}

#---------- Correlation Tests -----------------------
tbl <- table(brv$platform, brv$traffic_source)
chi1 <- chisq.test(tbl, simulate.p.value = TRUE)


tbl <- table(brv$weekDay, brv$traffic_source)
chi2 <- chisq.test(tbl, simulate.p.value = TRUE)


tbl <- table(brv$geo_location, brv$clicked)
chi3 <- chisq.test(tbl)





