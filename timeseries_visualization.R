library(data.table)
library(ggplot2)
library(dplyr)

#Manually Set working directory to source file location
#Input directory is a subdirectory of the directory that contains the source file
input_directory <- ".\\input"
setwd(input_directory)

page_events <- fread("page_events_train.csv")
page_views <- fread("page_views_sample.csv")

page_clicks_count <- page_events %>% group_by(document_id) %>% summarise(nclicks = n())
page_views_count <- page_views %>% group_by(document_id) %>% summarise(nviews = n())
document_stats <- merge(page_clicks_count, page_views_count, by="document_id")

hist1 <- ggplot(data=page_events, aes(page_events$timeOnPage)) + 
  geom_histogram() +
  xlim(0,1000) +
  ylim(0,1000) +
  xlab("Time on page")
print(hist1)

bar1 <- ggplot(data=page_events, aes(page_events$platform)) + 
  geom_bar() + 
  xlab("Platform")
print(bar1)

bar2 <- ggplot(data=page_events, aes(page_events$traffic_source)) + 
  geom_bar() + 
  xlab("Traffic Source")
print(bar2)

page_events_by_country <- mutate(page_events, country = substring(page_events$geo_location.y, 1,2)) %>% group_by(country) %>% filter(n() > 100)

bar3 <- ggplot(data=page_events_by_country, aes(as.factor(page_events_by_country$country))) + 
  geom_bar() + 
  xlab("Countries (with more than 100 data points)")
print(bar3)

scatter1 <- ggplot(data=document_stats, aes(log(nviews), log(nclicks))) +
  geom_point() +
  xlab("Number of Views - Log Scale") + 
  ylab("Number of Adv. Clicks - Log Scale")
print(scatter1)

