library(data.table)
library(ggplot2)
library(dplyr)

# TODO: Directory should not be hardcoded! Rather, will define a folder structure in Readme.md and put data in a local folder
input_directory <- "C:\\Users\\Tyler\\Documents\\My Documents\\MS&E226\\outbrain\\input"
setwd(input_directory)

page_events <- fread("page_events_train.csv")


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

bar2 <- ggplot(data=page_events_by_country, aes(as.factor(page_events_by_country$country))) + 
  geom_bar() + 
  xlab("Countries (with more than 100 data points)")
print(bar2)

#TODO:
# Add plots to write-up
# Make basic observations about the plots
# Keep in mind that all of the plots count loaded pages, not unique users


