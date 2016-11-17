library(data.table)
library(ggplot2)
library(dplyr)
library(cvTools)

#input_directory <- ".\\input"
#setwd(input_directory)

crv <- fread("crv.csv")

#split data
set.seed(193)
train.ind = sample(nrow(crv), 4*round(nrow(crv)/5)) #80% of data is for training, 20% for test
crv.train = crv[train.ind,]
crv.test = crv[-train.ind,]

lm1 <- lm(timeOnPage ~
            document_id + platform + traffic_source + weekDay +
            hour + topic_id*topic_confidence_level + category_id*category_confidence_level,
            data=crv.train)
lm1.cv <- cvFit(lm1, data=crv.train, y=crv.train$timeOnPage, K=10)
print(lm1.cv)

lm2 <- lm(timeOnPage ~ traffic_source + platform, data=crv.train)
lm2.cv <- cvFit(lm1, data=crv.train, y=crv.train$timeOnPage, K=10)
print(lm2.cv)