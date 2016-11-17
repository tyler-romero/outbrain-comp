library(data.table)
library(ggplot2)
library(dplyr)
library(e1071)
library(cvTools)
library(MLmetrics)

#input_directory <- ".\\input"
#setwd(input_directory)



brv <- fread("brv.csv")
brv <- select(brv, -c(V1))
brv$clicked <- as.factor(brv$clicked)

#split data
set.seed(851)
train.ind = sample(nrow(brv), 4*round(nrow(brv)/5)) #80% of data is for training, 20% for test
brv.train = brv[train.ind,]
brv.test = brv[-train.ind,]

f1 <- factor(clicked) ~ display_id + ad_id + document_id + platform + geo_location +
  traffic_source + factor(weekDay) + hour + topic_id*topic_confidence_level +
  source_id + publisher_id + category_id*category_confidence_level


binomial.glm1 <- glm(f1, data = brv.train, family = binomial())


predictThresh <- function(prediction, K) {
  df <- as.data.frame(prediction)
  return(as.numeric(df[2]/df[1] > K))
}
naiveBayes.m1 <- naiveBayes(f1, data=brv.train)
pre <- predictThresh(predict(naiveBayes.m1, brv.train, type = "raw"), K=4)
print(ZeroOneLoss(pre, brv.train$clicked))


svm.m1 <- svm(f1, data=brv.train, class.weights = c("0"=0.2, "1"=0.8))
pre <- predict(svm.m1, brv.train)

