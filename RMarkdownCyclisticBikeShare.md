---
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE, message = FALSE)
```

# Cyclistic Bike Share Analysis

Chris Johnson 2023-12-05

## Overview

This is a capstone project from Google's Data Analytics Certification on Coursera. In the following presentation we will go over the six steps in data analysis: Ask, Prepare, Process, Analyze, Share, and Act. This project is based on the following practice scenario.

**From the scenario**

*"You are a junior data analyst working in the marketing analyst team at Cyclistic, a bike-share company in Chicago. The director of marketing believes the company's future success depends on maximizing the number of annual memberships. Therefore, your team wants to understand how casual riders and annual members use Cyclistic bikes differently. From these insights, your team will design a new marketing strategy to convert casual riders into annual members. But first, Cyclistic executives must approve your recommendations, so they must be backed up with compelling data insights and professional data visualizations."*

In 2016, Cyclistic launched a successful bike-share offering. Since then, the program has grown to a fleet of 5,824 bicycles that are geotracked and locked into a network of 692 stations across Chicago. The bikes can be unlocked from one station and returned to any other station in the system anytime. Until now, Cyclistic's marketing strategy relied on building general awareness and appealing to broad consumer segments. One approach that helped make these things possible was the flexibility of its pricing plans: single-ride passes, full-day passes, and annual memberships. Customers who purchase single-ride or full-day passes are referred to as casual riders. Customers who purchase annual memberships are Cyclistic members. Cyclistic's finance analysts have concluded that annual members are much more profitable than casual riders. Although the pricing flexibility helps Cyclistic attract more customers, Moreno believes that maximizing the number of annual members will be key to future growth. Rather than creating a marketing campaign that targets all-new customers, Moreno believes there is a very good chance to convert casual riders into members. She notes that casual riders are already aware of the Cyclistic program and have chosen Cyclistic for their mobility needs. Moreno has set a clear goal: Design marketing strategies aimed at converting casual riders into annual members. In order to do that, however, the marketing analyst team needs to better understand how annual members and casual riders differ, why casual riders would buy a membership, and how digital media could affect their marketing tactics. Moreno and her team are interested in analyzing the Cyclistic historical bike trip data to identify trends.

## Ask

In the **Ask** step we will look at the guiding questions for our project and define what problem we are trying to solve.

**From the scenario**

Three questions will guide the future marketing program:

-   How do annual members and casual riders use Cyclistic bikes differently?

-   Why would casual riders buy Cyclistic annual memberships?

-   How can Cyclistic use digital media to influence casual riders to become members?

Moreno has assigned you the first question to answer: How do annual members and casual riders use Cyclistic bikes differently?

## Prepare

In the **Prepare** step we will identify the dataset provided and answer some questions to put context behind it and determine its quality.

-   *Where is your data located?* The data was provided by the company and I have stored it locally on my computer for better security.

-   *How is the data organized?* The company has provided the most recent 12 months of data, each month having its own data file to download. For each of these files I used the naming convention "Company Name" + "Data Type" + "Year and Month". For example: "Cyclistic_tripdata_202210.txt". Additionally, I structured the folder names as Cyclistic Bike Share Project/Raw Data for the initial raw data I downloaded and a separate folder "Project Data" for processed data files.

-   *Are there issues with bias or credibility in this data? Does your data ROCCC?* This data is first party data and is ROCCC (Reliable, Original, Comprehensive, Current, and Cited)

-   *How are you addressing licensing, privacy, security, and accessibility?* The company has provided a license for the data, the data does not provide personal information from any of the users, and the data is being stored locally for analysis.

-   *How does it help you answer your question?* The credibility of this data helps strengthen the validity of any findings made during analysis

## Process

In the **Process** step we look at how the data is structured and identify any needs for cleaning.

First we will setup our code for analysis

```{r echo=TRUE, message=FALSE, warning=FALSE}
#prepare packages for use 

library(tidyverse) 
library(lubridate)

```

Next we will load in our data

```{r echo=TRUE, message=FALSE, warning=FALSE}

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

```

Now we will put the data together and begin adding and subtracting columns to allow our data to show only useful information

```{r echo=TRUE, message=FALSE, warning=FALSE}
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

#removing unnecessary columns (while location data is often useful, we will not be using it for this project)
alltrips <- subset(alltrips, select = -c(start_lat, start_lng, end_lat, end_lng))

#inspecting alltrips data frame for irregularities or inconsistencies
str(alltrips)

```


## Analyze

In the **Analyze** step we will perform calculations and aggregations to seek out trends or discoveries.

```{r eval=TRUE, echo=TRUE, message=FALSE, warning=FALSE}

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
```

```{r echo=TRUE, message=FALSE, warning=FALSE}

#inspecting ride_length column
mean(alltrips$ride_length) #average ride length
median(alltrips$ride_length) #midpoint of all ride lengths
max(alltrips$ride_length) #longest ride
min(alltrips$ride_length) #shortest ride
#Note: some rides appear to be in the negative

```

```{r}
#duplicating alltrips dataframe but removing ride lengths less than 0 secs
alltripsV2 <- alltrips[!(alltrips$ride_length < 0),]

#changing ride_length from seconds to minutes
units(alltripsV2$ride_length) <- "mins"

#inspecting ride_length column from new dataframe
mean(alltripsV2$ride_length) #average ride length
median(alltripsV2$ride_length) #midpoint of all ride lengths
max(alltripsV2$ride_length) #longest ride
min(alltripsV2$ride_length) #shortest ride


```

```{r}
#now inspecting characteristics by member/casual rider
aggregate(alltripsV2$ride_length,list(alltripsV2$member_casual), FUN=mean)

```
```{r}
aggregate(alltripsV2$ride_length,list(alltripsV2$member_casual), FUN=median)

```
```{r}
aggregate(alltripsV2$ride_length,list(alltripsV2$member_casual), FUN=max)
#note the maximum ride lengths indicate errors with some rides lasting several days
```
```{r}
aggregate(alltripsV2$ride_length,list(alltripsV2$member_casual), FUN=min)

```
```{r}
#Let's remove rides greater than 3 hours. It is my opinion that anything greater 
# than this risks including errors made by riders
alltripsV2 <- alltripsV2[!(alltripsV2$ride_length > 180),]
#Note: by now we have removed 19,327 observations from our original dataframe
# amounting to 0.3% of total observations

```

```{r}
#now let's reinspect ride length characteristics by member/casual rider
aggregate(alltripsV2$ride_length,list(alltripsV2$member_casual), FUN=mean)
```
```{r}
aggregate(alltripsV2$ride_length,list(alltripsV2$member_casual), FUN=median)
```
```{r}
aggregate(alltripsV2$ride_length,list(alltripsV2$member_casual), FUN=max)
```
```{r}
aggregate(alltripsV2$ride_length,list(alltripsV2$member_casual), FUN=min)
```

```{r}
#order day of week column
alltripsV2$day_of_week <- ordered(alltripsV2$day_of_week, levels = 
                                    c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))
#inspecting number of rides by day of week
table(alltripsV2$day_of_week)
```

```{r}
#inspecting number of rides by member/casual then by day of week
table(alltripsV2$member_casual, alltripsV2$day_of_week)
#Note: while members tend to favor weekday rides, casual riders tend to favor weekends
```

```{r}
#average ride length by member/casual rider then by day of the week
aggregate(alltripsV2$ride_length ~ alltripsV2$member_casual + 
            alltripsV2$day_of_week, FUN = mean)
```

```{r message=FALSE}
#number of rides and average ride length grouped by member/casual rider and day of the week
alltripsV2 %>% mutate(weekday = wday(started_at, label = TRUE)) %>% 
  group_by(member_casual, weekday) %>% 
  summarise(number_of_rides = n(), average_duration = mean(ride_length)) %>% 
  arrange(member_casual, weekday)
```

```{r message=FALSE}
#visualization of number of rides by member/casual riders and by day of the week
alltripsV2 %>% mutate(weekday = alltripsV2$day_of_week) %>% 
  group_by(member_casual, weekday) %>% 
  summarise(number_of_rides = n()) %>% 
  arrange(member_casual, weekday) %>% 
  ggplot(aes(x = weekday, y = number_of_rides, fill = member_casual)) +
  geom_col(position = 'dodge') +
  labs(x = 'Day of the Week',y = 'Number of Rides')
```
```{r message=FALSE}
#visualization of average ride length by member/casual riders and by day of the week
alltripsV2 %>% mutate(weekday = alltripsV2$day_of_week) %>% 
  group_by(member_casual, weekday) %>% 
  summarise(average_duration_in_minutes = mean(ride_length)) %>% 
  arrange(member_casual, weekday) %>% 
  ggplot(aes(x = weekday, y = average_duration_in_minutes, fill = member_casual)) + 
  geom_col(position = 'dodge') +
  labs(x = 'Day of the Week',y = 'Average Duration in Minutes')
```
```{r message=FALSE}
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
```
```{r message=FALSE}
#now let's inspect the hours of the day riders start their trips at
alltripsV2$start_hour <- as.POSIXct(alltripsV2$started_at)
alltripsV2$start_hour <- format(alltripsV2$start_hour, format = '%H')

table(alltripsV2$start_hour)
```
```{r message=FALSE}
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
```
```{r message=FALSE}
#let's visualize number of rides for each month by member/casual riders
#here we can see both members and casual riders prefer riding during warmer
#months and peak during summer months.
alltripsV2$month <- as.POSIXct(alltripsV2$started_at)
alltripsV2$month <- format(alltripsV2$month, format = '%b')

alltripsV2$month <- ordered(alltripsV2$month, levels = 
                              c('Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec'))

alltripsV2 %>% group_by(member_casual,month) %>% 
  summarise(number_of_rides = n()) %>% 
  arrange(member_casual,month) %>% 
  ggplot(aes(x = month, y = number_of_rides, group = member_casual)) +
  geom_line(aes(color = member_casual)) +
  geom_point(aes(color = member_casual)) +
  labs(x = 'Month', y = 'Number of Rides')
```
## Share

In the **Share** step we communicate with our stakeholders our findings in presentation form whether that is on Powerpoint or some other presentation platform. Below you'll see a basic outline of a presentation with the findings made from the analysis and answers to the original project questions.

**Overview**. The questions we want to answer are *"how do members and casual riders differ in their use of the company's bike share service"* and *"what are our recommendations based on our analysis."* In order to sufficiently answer these questions we first wanted to establish the validity and integrity of the data provided, we then want to comb through the dataset for any abnormalities, after which we will then show how the analysis process took place and finally explain what the analysis process found.

**Presentation.** During a real presentation I would go over a lot of what has already been presented but also include the following analysis.

**Analysis.** During our inspections we looked over various ways in which both annual members and casual riders use Cyclistic Bikes. To answer our questions, the areas we want to highlight are the bike types each type of rider uses and which days of the week each type of rider prefers.

```{r message=FALSE}
#When we looked at types of bikes members and casual riders preferred, we saw a clear
# difference between them. While annual members showed 0 observations for Docked Bikes and
# an even split of rides between classic bikes and electric bikes, Casual members
# showed a significant amount of rides for docked bikes (6% of total rides), less rides on classic bikes,
# and more rides on electric bikes.
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
```
```{r message=FALSE}
#When we looked at days of the week, there is a clear difference in number of total rides
# between annual members and casual riders. While annual members prefer weekdays,
# casual members tend to prefer weekends
table(alltripsV2$member_casual, alltripsV2$day_of_week)

alltripsV2 %>% mutate(weekday = alltripsV2$day_of_week) %>% 
  group_by(member_casual, weekday) %>% 
  summarise(number_of_rides = n()) %>% 
  arrange(member_casual, weekday) %>% 
  ggplot(aes(x = weekday, y = number_of_rides, fill = member_casual)) +
  geom_col(position = 'dodge') +
  labs(x = 'Day of the Week',y = 'Number of Rides')
```



Therefore we can say annual members and casual riders use Cyclistic Bikes differently in two major ways: In the type of bike they use, and in their preference of weekdays vs weekends.



