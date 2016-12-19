# Unifies data with the other tables, then segments into training and test set
library(data.table)
library(ggplot2)
library(dplyr)

# Set working directory to the one where your data (csv files) are stored

#================= Continuous Response Variable =========================
page_views <- fread("page_views_sample.csv")
events <- fread("events.csv")
colnames(page_views)[[3]] <- "loadTimestamp"
colnames(events)[[4]] <- "eventTimestamp"
events$platform <- as.integer(events$platform)
page_views$platform <- as.integer(page_views$platform)

page_events <- merge(page_views, events, by = c("uuid", "document_id", "platform"))
# add new covariate which is the time on page (difference between timestamps)
page_events <- mutate(page_events, timeOnPage = eventTimestamp - loadTimestamp)
page_events$traffic_source <- as.integer(page_events$traffic_source)

# remove loaded tables as we don't need them (memory save)
rm(page_views)
rm(events)

# convert integer categories to string for ease of interpretation as well as avoiding errors later
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

# Sample only 10,000 rows from original training data
set.seed(129)
n = 10000   # rowcount
samp.ind = sample(nrow(page_events), n)   # sample n row indices at random
page_events.samp = page_events[samp.ind,] 
rm(page_events)

# Clean data
# remove geo_location.y as it is a repeat
# remove eventTimeStamp as it is an Oracle (it's the same as our continuous response in essence)
formatTime <- function(epoch) {
  as.POSIXlt(as.POSIXct(round(epoch/100), origin="2016-06-13"))
}
page_events.samp <- mutate(page_events.samp,
                           weekDay = wday(formatTime(loadTimestamp)),
                           monthDay = mday(formatTime(loadTimestamp)),
                           hour = hour(formatTime(loadTimestamp)),
                           min = formatTime(loadTimestamp)$min,
                           sec = formatTime(loadTimestamp)$sec)
page_events.samp <- select(page_events.samp, -c(geo_location.y, eventTimestamp))
page_events.samp <- rename(page_events.samp, geo_location = geo_location.x)

# Unify those rows with the other relational data tables
#!!! There are multiple topics per document, this selects only the topic given the most confidence
doc_topics <- fread("documents_topics.csv") %>%
  rename(topic_confidence_level = confidence_level) %>%
  group_by(document_id) %>%
  mutate(r = min_rank(desc(topic_confidence_level))) %>%
  filter(r == 1) %>%
  select(-c(r))

#!!! Two categories per document, this selects only the category given the most confidence
doc_categories <- fread("documents_categories.csv") %>%
  rename(category_confidence_level = confidence_level) %>%
  group_by(document_id) %>%
  mutate(r = min_rank(desc(category_confidence_level))) %>%
  filter(r == 1) %>%
  select(-c(r))

doc_meta <- fread("documents_meta.csv")

page_events.samp <- merge(page_events.samp, doc_topics, by = c("document_id"), all.x = TRUE)
page_events.samp <- merge(page_events.samp, doc_meta, by = c("document_id"), all.x = TRUE)
page_events.samp <- merge(page_events.samp, doc_categories, by = c("document_id"), all.x = TRUE)

page_events.samp <- page_events.samp[complete.cases(page_events.samp),]

# write the file to disk, crv == continuous reponse variable
write.csv(page_events.samp, file = "crv.csv")

# remove unnecessary files, free memory
rm(doc_meta)
rm(doc_topics)
rm(doc_categories)
rm(page_events.samp)

#================== Discrete Response Variable ========================
clicks <- fread("clicks_train.csv")
page_events.samp <- fread("crv.csv")

training_clicks <- clicks
clicked_on <- filter(training_clicks, training_clicks$clicked == 1) %>% select(display_id)

#Sample 10000 random rows from original training data
set.seed(675)
samp.ind = sample(nrow(clicked_on), n)
clicked_on = clicked_on[samp.ind,]

training_clicks.samp <- merge(clicked_on, training_clicks, by = c("display_id"))
# training_clicks.samp <- merge(training_clicks.samp, promoted_content, by = c("ad_id"), all.x = TRUE)

#Remove a variable that we wouldnt have access to in the real world
page_events.samp <- select(page_events.samp, -c(timeOnPage))

#Merge brv with our existing crv data
training_clicks.samp <- merge(training_clicks.samp, page_events.samp, by = c("display_id"))
training_clicks.samp <- select(training_clicks.samp, -c(V1, uuid))

write.csv(training_clicks.samp, file = "brv.csv")

# remove unused
rm(clicked_on)
rm(training_clicks.samp)
rm(page_events.samp)