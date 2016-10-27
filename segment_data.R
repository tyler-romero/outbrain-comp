library(data.table)

#Manually et working directory to source file location
#Input directory is a subdirectory of the directory that contains the source file
input_directory <- ".//"
# setwd(input_directory)
clicks_train <- fread("clicks_train.csv")

# set random number generator seed for reproducibility
set.seed(1492)

# create training and test set
train.ind = sample(nrow(clicks_train), 4*round(nrow(clicks_train)/5)) #80% of data is for training, 20% for test
clicks_train.train = clicks_train[train.ind,]
clicks_train.test = clicks_train[-train.ind,]

write.csv(clicks_train.train, file = "training_click.csv")
write.csv(clicks_train.test, file = "testing_clicks.csv")

#####################################################

page_views <- fread("page_views_sample.csv")
events <- fread("events.csv")
colnames(page_views)[[3]] <- "loadTimestamp"
colnames(events)[[4]] <- "eventTimestamp"
events$platform <- as.integer(events$platform)
page_views$platform <- as.integer(page_views$platform)

page_events <- merge(page_views, events, by = c("uuid", "document_id", "platform"))
page_events <- mutate(page_events, timeOnPage = eventTimestamp - loadTimestamp)
page_events$traffic_source <- as.integer(page_events$traffic_source)

rm(page_views)
rm(events)

convert_platform <- function(x) {
  if(x == 1) return(as.factor("desktop"))
  if(x == 2) return(as.factor("mobile"))
  if(x == 3) return(as.factor("tablet"))
}

convert_traffic <- function(x) {
  if(x == 1) return(as.factor("internal"))
  if(x == 2) return(as.factor("search"))
  if(x == 3) return(as.factor("social"))
}

page_events$platform <- sapply(page_events$platform, convert_platform)
page_events$traffic_source <- sapply(page_events$traffic_source, convert_traffic)

set.seed(142)
train.ind = sample(nrow(page_events), 4*round(nrow(page_events)/5)) #80% of data is for training, 20% for test
page_events.train = page_events[train.ind,]
page_events.test = page_events[-train.ind,]

write.csv(page_events.train, file = "page_events_train.csv")
write.csv(page_events.test, file = "page_events_test.csv")