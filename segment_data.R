library(data.table)
library(dplyr)

#Manually et working directory to source file location
#Input directory is a subdirectory of the directory that contains the source file
input_directory <- ".\\input"
#setwd(input_directory)
clicks <- fread("clicks_train.csv")
clicks <- slice(clicks, 1:(nrow(clicks)/2))

# set random number generator seed for reproducibility
set.seed(1492)

# create training and test set
clicked_on <- filter(clicks, clicks$clicked == 1) %>%
  select(display_id)
train.ind = sample(nrow(clicked_on), 4*round(nrow(clicked_on)/5)) #80% of data is for training, 20% for test

clicked_on.train = clicked_on[train.ind,]
clicked_on.test = clicked_on[-train.ind,]

clicks.train <- merge(clicked_on.train, clicks, by = c("display_id"))
clicks.test <- merge(clicked_on.test, clicks, by = c("display_id"))

write.csv(clicks.train, file = "training_clicks.csv")
write.csv(clicks.test, file = "testing_clicks.csv")

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