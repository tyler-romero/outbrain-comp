library(data.table)
library(ggplot2)
library(dplyr)

#Manually Set working directory to source file location
#Input directory is a subdirectory of the directory that contains the source file
input_directory <- ".\\input"
setwd(input_directory)

page_events <- fread("page_events_train.csv")
page_views <- fread("page_views_sample.csv")
#clicks_train <- fread("training_click.csv")

page_clicks_count <- page_events %>% group_by(document_id, platform) %>% summarise(nclicks = n())
page_views_count <- page_views %>% group_by(document_id, platform) %>% summarise(nviews = n())
document_stats <- merge(page_clicks_count, page_views_count, by=c("document_id"))
#adv_count <- clicks_train %>% group_by(ad_id) %>% summarise(nviews = n())
#user_count <- page_views %>% group_by(uuid) %>% summarise(n = n())


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

bar3 <- ggplot(data=page_events_by_country, aes(as.factor(page_events_by_country$country), fill = factor(platform))) + 
  geom_bar() + 
  xlab("Countries (with more than 100 data points)") +
  scale_y_log10()
print(bar3)

without_us <- filter(page_events_by_country, country != 'US')
bar4 <- ggplot(data=without_us, aes(as.factor(without_us$country))) + 
  geom_bar() + 
  xlab("Countries (with more than 100 data points)")
print(bar4)


scatter1 <- ggplot(data=document_stats, aes(nviews, nclicks)) +
  xlab("Number of Views - Log Scale") + 
  ylab("Number of Adv. Clicks - Log Scale")
print(scatter1)


hist1 <- ggplot(data=user_count, aes(n)) + 
  geom_histogram(bins = 10) +
  xlab("Number of times user appears") +
  ylab("Count (log scale)") +
  scale_y_log10() +
  xlim(0,10)
print(hist1)
