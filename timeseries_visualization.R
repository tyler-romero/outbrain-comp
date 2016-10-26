library(data.table)
library(ggplot2)
library(dplyr)

# TODO: Directory should not be hardcoded! Rather, will define a folder structure in Readme.md and put data in a local folder
input_directory <- "C:\\Users\\Tyler\\Documents\\My Documents\\MS&E226\\outbrain\\input"
current_directory <- dirname(parent.frame(2)$ofile)
setwd(input_directory)

page_views <- fread("page_views_sample.csv")
events <- fread("events.csv")
colnames(page_views)[[3]] <- "loadTimestamp"
colnames(events)[[4]] <- "eventTimestamp"

view_info <- merge(page_views, events, by = c("uuid", "document_id"))
view_info <- mutate(view_info, timeOnPage = eventTimestamp - loadTimestamp)

#hist1 <- ggplot(data=view_info, aes(view_info$timeOnPage)) + 
#  geom_histogram(binwidth = 1000)
#print(hist1)

hist2 <- ggplot(data=view_info, aes(view_info$timeOnPage)) + 
  geom_histogram() +
  xlim(0,1000) +
  ylim(0,1000)
print(hist2)

