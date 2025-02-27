---
title: "Cyclistic Bike Share Analysis"
author: "Chris Johnson"
date: "2022-12-28"
output: github_document
---

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

```{r message=FALSE, warning=FALSE}
#set working directory
setwd("~/R Scripts/Google_Capstone_Project_CyclisticBikes")

#prepare packages for use
library(tidyverse)
library(lubridate)
```

Next we will load in our data

```{r message=FALSE, warning=FALSE, paged.print=FALSE}
#import last 12 months of Cyclistic rider data
nov2021<- read_csv('Project_Data/Cyclistic_tripdata_202111.csv')
dec2021<- read_csv('Project_Data/Cyclistic_tripdata_202112.csv')
jan2022<- read_csv('Project_Data/Cyclistic_tripdata_202201.csv')
feb2022<- read_csv('Project_Data/Cyclistic_tripdata_202202.csv')
mar2022<- read_csv('Project_Data/Cyclistic_tripdata_202203.csv')
apr2022<- read_csv('Project_Data/Cyclistic_tripdata_202204.csv')
may2022<- read_csv('Project_Data/Cyclistic_tripdata_202205.csv')
jun2022<- read_csv('Project_Data/Cyclistic_tripdata_202206.csv')
jul2022<- read_csv('Project_Data/Cyclistic_tripdata_202207.csv')
aug2022<- read_csv('Project_Data/Cyclistic_tripdata_202208.csv')
sep2022<- read_csv('Project_Data/Cyclistic_tripdata_202209.csv')
oct2022<- read_csv('Project_Data/Cyclistic_tripdata_202210.csv')
```

Now we will put the data together and begin adding and subtracting columns to allow our data to show only useful information

```{r echo=TRUE, message=FALSE, warning=FALSE}
#combining months into a 12-month data frame to analyze data as a whole
alltrips <- rbind(nov2021,
                  dec2021,
                  jan2022,
                  feb2022,
                  mar2022,
                  apr2022,
                  may2022,
                  jun2022,
                  jul2022,
                  aug2022,
                  sep2022,
                  oct2022)

#adding column to show the length of each ride
alltrips$ride_length <- alltrips$ended_at-alltrips$started_at

#adding column to show which day of the week each ride was on
alltrips$day_of_week <- as.Date(alltrips$started_at)

#formatting 'day of the week' column to suit our needs
alltrips$day_of_week <- format(as.Date(alltrips$day_of_week),'%A')

#removing unnecessary columns
alltrips <- subset(alltrips, select = -c(start_lat, start_lng, end_lat, end_lng))

#inspecting alltrips data frame for irregularities or inconsistencies
str(alltrips)
```

## Analyze

In the **Analyze** step we will perform calculations and aggregations to seek out trends or discoveries.

```{r}
#check how many observations there are between members and casual riders
table(alltrips$member_casual)
#Note: there are 1,049,628 more member observations than casual riders
```

```{r}
#inspecting rideable_type dispersion
table(alltrips$rideable_type)
```

```{r}
#now inspecting rideable_type dispersion by member/casual rider
table(alltrips$member_casual,alltrips$rideable_type)
```

```{r}
#inspecting ride_length column
mean(alltrips$ride_length) #average ride length
median(alltrips$ride_length) #midpoint of all ride lengths
max(alltrips$ride_length) #longest ride
min(alltrips$ride_length) #shortest ride
#Note: some ride lengths appear to be in the negative
```

```{r}
#duplicating alltrips dataframe but removing ride lengths less than 0 secs
alltripsV2 <- alltrips[!(alltrips$ride_length < 0),]
#Note: 112 rows removed leaving 5,755,782 rows
```

```{r}
#inspecting ride_length column from new dataframe
mean(alltripsV2$ride_length) #average ride length
median(alltripsV2$ride_length) #midpoint of all ride lengths
max(alltripsV2$ride_length) #longest ride
min(alltripsV2$ride_length) #shortest ride
```

```{r}
#now inspecting characteristics by member/casual rider
aggregate(alltripsV2$ride_length,list(alltripsV2$member_casual), FUN=mean)
aggregate(alltripsV2$ride_length,list(alltripsV2$member_casual), FUN=median)
aggregate(alltripsV2$ride_length,list(alltripsV2$member_casual), FUN=max)
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
```

```{r}
#average ride length by member/casual rider then by day of the week
aggregate(alltripsV2$ride_length ~ alltripsV2$member_casual + 
            alltripsV2$day_of_week, FUN = mean)
```

```{r}
#number of rides and average ride length grouped by member/casual rider and day of the week
alltripsV2 %>% mutate(weekday = wday(started_at, label = TRUE)) %>% 
  group_by(member_casual, weekday) %>% 
  summarise(number_of_rides = n(), average_duration = mean(ride_length)) %>% 
  arrange(member_casual, weekday)
```

```{r message=FALSE, warning=FALSE}
#visualization of number of rides by member/casual riders and by day of the week
alltripsV2 %>% mutate(weekday = alltripsV2$day_of_week) %>% 
  group_by(member_casual, weekday) %>% 
  summarise(number_of_rides = n()) %>% 
  arrange(member_casual, weekday) %>% 
  ggplot(aes(x = weekday, y = number_of_rides, fill = member_casual)) +
  geom_col(position = 'dodge')
```

```{r}
#visualization of average ride length by member/casual riders and by day of the week
alltripsV2 %>% mutate(weekday = alltripsV2$day_of_week) %>% 
  group_by(member_casual, weekday) %>% 
  summarise(average_duration_in_minutes = mean(ride_length)/60) %>% 
  arrange(member_casual, weekday) %>% 
  ggplot(aes(x = weekday, y = average_duration_in_minutes, fill = member_casual)) +
  geom_col(position = 'dodge')
```

```{r}
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

```{r}
#now let's inspect the hours of the day riders start their trips at
alltripsV2$start_hour <- as.POSIXct(alltripsV2$started_at)
str(alltripsV2)

alltripsV2$start_hour <- format(alltripsV2$start_hour, format = '%H')
str(alltripsV2)

table(alltripsV2$start_hour)
```

```{r}
#let's visualize the number of rides started for each hour of the day
alltripsV2 %>% group_by(member_casual, start_hour) %>% 
  summarise(number_of_rides = n()) %>% 
  arrange(member_casual, start_hour) %>% 
  ggplot(aes(x = start_hour, y = number_of_rides, group = member_casual)) +
  geom_line(aes(color = member_casual)) +
  geom_point(aes(color = member_casual))
```

```{r}
#let's visualize number of rides for each month by member/casual riders
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
  geom_point(aes(color = member_casual))
```

## Share

In the **Share** step we communicate with our stakeholders our findings in presentation form whether that is on Powerpoint or some other presentation platform. Below you'll see a basic outline of a presentation with the findings made from the analysis and answers to the original project questions.

**Overview**. The questions we want to answer are *"how do members and casual riders differ in their use of the company's bike share service"* and *"what are our recommendations based on our analysis."* In order to sufficiently answer these questions we first wanted to establish the validity and integrity of the data provided, we then want to comb through the dataset for any abnormalities, after which we will then show how the analysis process took place and finally explain what the analysis process found.

**Presentation.** During a real presentation I would go over a lot of what has already been presented but also include the following analysis.

**Analysis.** Our analysis found some note-worthy behaviors in both members and casual riders that can help us understand our customers as well as help us specifically target casual riders.

```{r echo=FALSE, message=FALSE, warning=FALSE}

alltripsV2 %>% mutate(weekday = alltripsV2$day_of_week) %>% 
  group_by(member_casual, weekday) %>% 
  summarise(average_duration_in_minutes = mean(ride_length)/60) %>% 
  arrange(member_casual, weekday) %>% 
  ggplot(aes(x = weekday, y = average_duration_in_minutes, fill = member_casual)) +
  geom_col(position = 'dodge') +
  labs(x = 'Day of the Week',y = 'Average Duration in Minutes')
```

```{r echo=FALSE, message=FALSE, warning=FALSE}
alltripsV2 %>% mutate(weekday = alltripsV2$day_of_week) %>% 
  group_by(member_casual, weekday) %>% 
  summarise(number_of_rides = n()) %>% 
  arrange(member_casual, weekday) %>% 
  ggplot(aes(x = weekday, y = number_of_rides, fill = member_casual)) +
  geom_col(position = 'dodge') +
  labs(x = 'Day of the Week',y = 'Number of Rides')
```

In these two histograms, we see that:

-   number of rides for casual riders peaks during the weekend, favoring Saturday the most

-   casual riders more than double their average ride length when compared to the average ride length of members

-   casual riders prefer longer rides during the weekends.

```{r echo=FALSE, message=FALSE, warning=FALSE}
alltripsV2 %>% group_by(member_casual, start_hour) %>% 
  summarise(number_of_rides = n()) %>% 
  arrange(member_casual, start_hour) %>% 
  ggplot(aes(x = start_hour, y = number_of_rides, group = member_casual)) +
  geom_line(aes(color = member_casual)) +
  geom_point(aes(color = member_casual)) +
  labs(x = 'Hour of the Day', y = 'Number of Rides')
```

In this line graph we see that, like members, number of rides for casual riders peaks at 5pm.

```{r echo=FALSE, message=FALSE, warning=FALSE}
alltripsV2 %>% group_by(member_casual,month) %>% 
  summarise(number_of_rides = n()) %>% 
  arrange(member_casual,month) %>% 
  ggplot(aes(x = month, y = number_of_rides, group = member_casual)) +
  geom_line(aes(color = member_casual)) +
  geom_point(aes(color = member_casual)) +
  labs(x = 'Month', y = 'Number of Rides')
```

In this line graph we see that number of rides for members steadily increases during summer months and remains high until after October. Although casual riders show a relatively similar trend, the number of rides for casual riders sharply increases during summer months but quickly decreases after August.

## Act

This data has allowed us to identify a few key insights into some aspects of how members and casual riders use Cyclistic's bikes but it does not give us insight into how they became members, why they chose Cyclistic Bike-Share, or for what purpose they used Cyclistic Bike-Share. For this reason our recommendations, with the business task in mind, can only help us identify which casual riders our marketing team should focus on. It is clear from our findings that the top 3 groups of casual riders we should focus on is:

-   Casual riders who bike-share during the summer

-   Casual riders who bike-share on the weekends

-   Casual riders who bike-share around 5pm
