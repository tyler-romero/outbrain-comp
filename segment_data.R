<<<<<<< HEAD
# Install development version of data.table - REQUIRED for fwrite
# TODO: Put check for already being installed
install.packages("data.table", repos = "https://Rdatatable.github.io/data.table", type = "source")
=======
>>>>>>> dev
library(data.table)

# TODO: Directory should not be hardcoded! Rather, will define a folder structure in Readme.md and put data in a local folder
input_directory <- "C:\\Users\\Tyler\\Documents\\My Documents\\MS&E226\\outbrain\\input"
setwd(input_directory)
clicks_train <- fread("clicks_train.csv")

# set random number generator seed for reproducibility
set.seed(1492)

# create training and test set
train.ind = sample(nrow(clicks_train), 4*round(nrow(clicks_train)/5)) #80% of data is for training, 20% for test
clicks_train.train = clicks_train[train.ind,]
clicks_train.test = clicks_train[-train.ind,]

<<<<<<< HEAD
fwrite(clicks_train.train, "training_clicks.csv")
fwrite(clicks_train.test, "testing_clicks.csv")
=======
write.csv(clicks_train.train, file = "training_click.csv")
write.csv(clicks_train.test, file = "testing_clicks.csv")
>>>>>>> dev
