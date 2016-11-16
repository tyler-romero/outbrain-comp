#Gets a subset of the training data, cleans it, and unifies it with other data tables
library(data.table)
library(ggplot2)
library(dplyr)

#input_directory <- ".\\input"
#setwd(input_directory)

#================= Continuous Random Variable =========================
page_events <- fread("page_events_train.csv")

#Sample only 1000 rows from original training data
set.seed(129)
n = 1000
samp.ind = sample(nrow(page_events), n)
page_events.samp = page_events[samp.ind,]
rm(page_events)

#Clean data
page_events.samp <- select(page_events.samp, -c(V1, geo_location.y))
page_events.samp <- rename(page_events.samp, geo_location = geo_location.x)

#Unify those rows with the other relational data tables

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
promoted_content <- fread("promoted_content.csv")

page_events.samp <- merge(page_events.samp, doc_topics, by = c("document_id"), all.x = TRUE)
page_events.samp <- merge(page_events.samp, doc_meta, by = c("document_id"), all.x = TRUE)
page_events.samp <- merge(page_events.samp, doc_categories, by = c("document_id"), all.x = TRUE)
page_events.samp <- merge(page_events.samp, promoted_content, by = c("document_id"), all.x = TRUE)

#================== Discrete Random Variable ========================
training_clicks <- fread("training_clicks.csv")
clicked_on <- filter(training_clicks, training_clicks$clicked == 1) %>% select(display_id)

#Sample only 1000 rows from original training data
set.seed(675)
samp.ind = sample(nrow(clicked_on), n)
clicked_on = clicked_on[samp.ind,]

training_clicks.samp <- merge(clicked_on, training_clicks, by = c("display_id"))


