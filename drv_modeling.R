library(data.table)
library(ggplot2)
library(dplyr)
library(cvTools)

#input_directory <- ".\\input"
#setwd(input_directory)

brv <- fread("brv.csv")
brv <- select(brv, -c(V1))

#split data
set.seed(851)
train.ind = sample(nrow(brv), 4*round(nrow(brv)/5)) #80% of data is for training, 20% for test
brv.train = brv[train.ind,]
brv.test = brv[-train.ind,]


binomial.glm1 <- glm(clicked ~ platform + traffic_source + weekDay + hour + topic_id + source_id, data=brv.train, family=binomial())


binomial.glm1 <- glm(clicked ~ platform + traffic_source + factor(weekDay) + hour + topic_id + source_id, data=brv.train, family=binomial())

