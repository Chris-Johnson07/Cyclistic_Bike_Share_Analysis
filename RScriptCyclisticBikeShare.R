#prepare packages for use
library(tidyverse)
library(lubridate)

#import last 12 months of Cyclistic rider data
may2023<- read_csv('Raw Data/Case Study 1 Dataset/202305-divvy-tripdata/202305-divvy-tripdata.csv')
apr2023<- read_csv('Raw Data/Case Study 1 Dataset/202304-divvy-tripdata/202304-divvy-tripdata.csv')
mar2023<- read_csv('Raw Data/Case Study 1 Dataset/202303-divvy-tripdata/202303-divvy-tripdata.csv')
feb2023<- read_csv('Raw Data/Case Study 1 Dataset/202302-divvy-tripdata/202302-divvy-tripdata.csv')
jan2023<- read_csv('Raw Data/Case Study 1 Dataset/202301-divvy-tripdata/202301-divvy-tripdata.csv')
dec2022<- read_csv('Raw Data/Case Study 1 Dataset/202212-divvy-tripdata/202212-divvy-tripdata.csv')
nov2022<- read_csv('Raw Data/Case Study 1 Dataset/202211-divvy-tripdata/202211-divvy-tripdata.csv')
oct2022<- read_csv('Raw Data/Case Study 1 Dataset/202210-divvy-tripdata/202210-divvy-tripdata.csv')
sep2022<- read_csv('Raw Data/Case Study 1 Dataset/202209-divvy-tripdata/202209-divvy-publictripdata.csv')
aug2022<- read_csv('Raw Data/Case Study 1 Dataset/202208-divvy-tripdata/202208-divvy-tripdata.csv')
jul2022<- read_csv('Raw Data/Case Study 1 Dataset/202207-divvy-tripdata/202207-divvy-tripdata.csv')
jun2022<- read_csv('Raw Data/Case Study 1 Dataset/202206-divvy-tripdata/202206-divvy-tripdata.csv')

#combining months into a 12-month data frame to analyze data as a whole
alltrips <- rbind(may2023,
                  apr2023,
                  mar2023,
                  feb2023,
                  jan2023,
                  dec2022,
                  nov2022,
                  oct2022,
                  sep2022,
                  aug2022,
                  jul2022,
                  jun2022)

#adding column to show the length of each ride
alltrips$ride_length <- alltrips$ended_at-alltrips$started_at
alltrips$ride_length <- as.difftime(alltrips$ride_length, units = 'mins')

#adding column to show which day of the week each ride was on
alltrips$day_of_week <- as.Date(alltrips$started_at)

#formatting 'day of the week' column to suit our needs
alltrips$day_of_week <- format(as.Date(alltrips$day_of_week),'%A')

#removing unnecessary columns
alltrips <- subset(alltrips, select = -c(start_lat, start_lng, end_lat, end_lng))

#inspecting alltrips data frame for irregularities or inconsistencies
str(alltrips)

#check how many observations there are between members and casual riders
table(alltrips$member_casual)
#Note: there are 1,049,628 more member observations than casual riders

#inspecting rideable_type dispersion
table(alltrips$rideable_type)

#now inspecting rideable_type dispersion by member/casual rider
table(alltrips$member_casual,alltrips$rideable_type)
#Note: there are no observations for docked_bikes for members
#Note: members show an equal share of rides for classic and electric bikes while
#casual riders prefer electric bikes 

#inspecting ride_length column
mean(alltrips$ride_length) #average ride length
median(alltrips$ride_length) #midpoint of all ride lengths
max(alltrips$ride_length) #longest ride
min(alltrips$ride_length) #shortest ride
#Note: some rides appear to be in the negative

#duplicating alltrips dataframe but removing ride lengths less than 0 secs
alltripsV2 <- alltrips[!(alltrips$ride_length < 0),]

#changing ride_length from seconds to minutes
units(alltripsV2$ride_length) <- "mins"

#inspecting ride_length column from new dataframe
mean(alltripsV2$ride_length) #average ride length
median(alltripsV2$ride_length) #midpoint of all ride lengths
max(alltripsV2$ride_length) #longest ride
min(alltripsV2$ride_length) #shortest ride

#now inspecting characteristics by member/casual rider
aggregate(alltripsV2$ride_length,list(alltripsV2$member_casual), FUN=mean)
aggregate(alltripsV2$ride_length,list(alltripsV2$member_casual), FUN=median)
aggregate(alltripsV2$ride_length,list(alltripsV2$member_casual), FUN=max)
aggregate(alltripsV2$ride_length,list(alltripsV2$member_casual), FUN=min)
#note the maximum ride lengths indicate errors with some rides lasting several days

#Let's remove rides greater than 3 hours
alltripsV2 <- alltripsV2[!(alltripsV2$ride_length > 180),]
#Note: by now we have removed 19,327 observations from our original dataframe
# amounting to 0.3% of total observations removed

#now let's reinspect ride length characteristics by member/casual rider
aggregate(alltripsV2$ride_length,list(alltripsV2$member_casual), FUN=mean)
aggregate(alltripsV2$ride_length,list(alltripsV2$member_casual), FUN=median)
aggregate(alltripsV2$ride_length,list(alltripsV2$member_casual), FUN=max)
aggregate(alltripsV2$ride_length,list(alltripsV2$member_casual), FUN=min)

#order day of week column
alltripsV2$day_of_week <- ordered(alltripsV2$day_of_week, levels = 
                                    c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))
#inspecting number of rides by day of week
table(alltripsV2$day_of_week)

#inspecting number of rides by member/casual then by day of week
table(alltripsV2$member_casual, alltripsV2$day_of_week)
#Note: while members tend to favor weekday rides, casual riders tend to favor weekends

#average ride length by member/casual rider then by day of the week
aggregate(alltripsV2$ride_length ~ alltripsV2$member_casual + 
            alltripsV2$day_of_week, FUN = mean)

#number of rides and average ride length grouped by member/casual rider and day of the week
alltripsV2 %>% mutate(weekday = wday(started_at, label = TRUE)) %>% 
  group_by(member_casual, weekday) %>% 
  summarise(number_of_rides = n(), average_duration = mean(ride_length)) %>% 
  arrange(member_casual, weekday)

#visualization of number of rides by member/casual riders and by day of the week
alltripsV2 %>% mutate(weekday = alltripsV2$day_of_week) %>% 
  group_by(member_casual, weekday) %>% 
  summarise(number_of_rides = n()) %>% 
  arrange(member_casual, weekday) %>% 
  ggplot(aes(x = weekday, y = number_of_rides, fill = member_casual)) +
  geom_col(position = 'dodge') +
  labs(x = 'Day of the Week',y = 'Number of Rides')
ggsave('number_of_rides.png')

#visualization of average ride length by member/casual riders and by day of the week
alltripsV2 %>% mutate(weekday = alltripsV2$day_of_week) %>% 
  group_by(member_casual, weekday) %>% 
  summarise(average_duration_in_minutes = mean(ride_length)) %>% 
  arrange(member_casual, weekday) %>% 
  ggplot(aes(x = weekday, y = average_duration_in_minutes, fill = member_casual)) + 
  geom_col(position = 'dodge') +
  labs(x = 'Day of the Week',y = 'Average Duration in Minutes')
ggsave("average_duration.png")

#visualization of the type of bike used by member/casual riders
alltripsV2[alltripsV2$member_casual == 'member',] %>% group_by(rideable_type) %>% 
  summarise(perc = n()/nrow(alltripsV2[alltripsV2$member_casual == 'member',])*100) %>% 
  arrange(desc(rideable_type)) %>% 
  mutate(ypos = cumsum(perc)-0.5*perc) %>% 
  ggplot(aes(x='',y=perc,fill=rideable_type)) +
  geom_bar(stat='identity',width=1,color='white') +
  coord_polar('y',start=0) +
  theme_void() +
  geom_text(aes(y=ypos,label=paste(round(perc,1),'%'))) +
  labs(title = 'Bike Type Dispersion for Members')

alltripsV2[alltripsV2$member_casual == 'casual',] %>% group_by(rideable_type) %>% 
  summarise(perc = n()/nrow(alltripsV2[alltripsV2$member_casual == 'casual',])*100) %>% 
  arrange(desc(rideable_type)) %>% 
  mutate(ypos = cumsum(perc)-0.5*perc) %>% 
  ggplot(aes(x='',y=perc,fill=rideable_type)) +
  geom_bar(stat='identity',width=1,color='white') +
  coord_polar('y',start=0) +
  theme_void() +
  geom_text(aes(y=ypos,label=paste(round(perc,1),'%'))) +
  labs(title = 'Bike Type Dispersion for Casual Riders')

#now let's inspect the hours of the day riders start their trips at
alltripsV2$start_hour <- as.POSIXct(alltripsV2$started_at)
str(alltripsV2)

alltripsV2$start_hour <- format(alltripsV2$start_hour, format = '%H')
str(alltripsV2)

table(alltripsV2$start_hour)

#let's visualize the number of rides started for each hour of the day
#here we can see member riders have a trend. A peak at 8am and a peak at 5pm
#this is consistent with a typical 8am-5pm workday indicating some members ride
#to and from work. Casual riders show a partial similar trend to this at 5pm.
alltripsV2 %>% group_by(member_casual, start_hour) %>% 
  summarise(number_of_rides = n()) %>% 
  arrange(member_casual, start_hour) %>% 
  ggplot(aes(x = start_hour, y = number_of_rides, group = member_casual)) +
  geom_line(aes(color = member_casual)) +
  geom_point(aes(color = member_casual)) +
  labs(x = 'Hour of the Day', y = 'Number of Rides')
ggsave("rides_by_the_hour")

#let's visualize number of rides for each month by member/casual riders
#here we can see both members and casual riders prefer riding during warmer
#months and peak during summer months.
alltripsV2$month <- as.POSIXct(alltripsV2$started_at)
str(alltripsV2)

alltripsV2$month <- format(alltripsV2$month, format = '%b')
str(alltripsV2)

alltripsV2$month <- ordered(alltripsV2$month, levels = 
                              c('Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec'))

alltripsV2 %>% group_by(member_casual,month) %>% 
  summarise(number_of_rides = n()) %>% 
  arrange(member_casual,month) %>% 
  ggplot(aes(x = month, y = number_of_rides, group = member_casual)) +
  geom_line(aes(color = member_casual)) +
  geom_point(aes(color = member_casual)) +
  labs(x = 'Month', y = 'Number of Rides')

#now that we've inspected our data we can start answering our question: "How do annual
#members and casual riders differ?"
#From our research I found two major areas members and casual riders differ:
#- Days of the week
#- Bike type

#When we looked at days of the week, there is a clear difference in number of total rides
# between annual members and casual riders. While annual members prefer weekdays,
# casual members tend to prefer weekends
alltripsV2 %>% mutate(weekday = alltripsV2$day_of_week) %>% 
  group_by(member_casual, weekday) %>% 
  summarise(number_of_rides = n()) %>% 
  arrange(member_casual, weekday) %>% 
  ggplot(aes(x = weekday, y = number_of_rides, fill = member_casual)) +
  geom_col(position = 'dodge') +
  labs(x = 'Day of the Week',y = 'Number of Rides')


#When we looked at types of bikes members and casual riders preferred, we saw a clear
# difference between. While annual members showed 0 observations for Docked Bikes and
# an even split of rides between classic bikes and electric bikes, Casual members
# showed a significant amount of rides for docked bikes (6% of total rides), a decrease in classic bikes,
# and an increase in electric bikes.
table(alltrips$member_casual,alltrips$rideable_type)

#visualization of the type of bike used by member/casual riders
alltripsV2[alltripsV2$member_casual == 'member',] %>% group_by(rideable_type) %>% 
  summarise(perc = n()/nrow(alltripsV2[alltripsV2$member_casual == 'member',])*100) %>% 
  arrange(desc(rideable_type)) %>% 
  mutate(ypos = cumsum(perc)-0.5*perc) %>% 
  ggplot(aes(x='',y=perc,fill=rideable_type)) +
  geom_bar(stat='identity',width=1,color='white') +
  coord_polar('y',start=0) +
  theme_void() +
  geom_text(aes(y=ypos,label=paste(round(perc,1),'%'))) +
  labs(title = 'Bike Type Dispersion for Members')

alltripsV2[alltripsV2$member_casual == 'casual',] %>% group_by(rideable_type) %>% 
  summarise(perc = n()/nrow(alltripsV2[alltripsV2$member_casual == 'casual',])*100) %>% 
  arrange(desc(rideable_type)) %>% 
  mutate(ypos = cumsum(perc)-0.5*perc) %>% 
  ggplot(aes(x='',y=perc,fill=rideable_type)) +
  geom_bar(stat='identity',width=1,color='white') +
  coord_polar('y',start=0) +
  theme_void() +
  geom_text(aes(y=ypos,label=paste(round(perc,1),'%'))) +
  labs(title = 'Bike Type Dispersion for Casual Riders')