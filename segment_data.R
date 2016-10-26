library(data.table)

input_directory <- "C:\\Users\\Tyler\\Documents\\My Documents\\MS&E226\\outbrain\\input"
setwd(input_directory)
clicks_train <- fread("clicks_train.csv")

# set random number generator seed for reproducibility
set.seed(1492)

# create training and test set
train.ind = sample(nrow(clicks_train), 4*round(nrow(clicks_train)/5)) #80% of data is for training, 20% for test
clicks_train.train = clicks_train[train.ind,]
clicks_train.test = clicks_train[-train.ind,]

write.csv(clicks_train.train, file = "training_click.csv")
write.csv(clicks_train.test, file = "testing_clicks.csv")