setwd("~/Uni Temp Work/Lecture 1")
New_York <- read.csv("2016_Green_Taxi_Trip_Data.csv")
library(ggplot2)
library(tidyverse)

## Clean + Convert Date-Time
New_York <- New_York[, c(1:16,18:21)]
New_York$Store_and_fwd_flag <- as.factor(New_York$Store_and_fwd_flag)
New_York$ï..VendorID <- as.factor(New_York$ï..VendorID)
New_York$Pickup_Datetime <- as.POSIXlt(New_York$lpep_pickup_datetime, format = "%m/%d/%Y %I:%M:%S %p")
New_York$Dropoff_Datetime <- as.POSIXlt(New_York$Lpep_dropoff_datetime, format = "%m/%d/%Y %I:%M:%S %p")
New_York <- New_York[, c(1,4:22)]
Save_History <- New_York
New_York <- New_York[1:7087649,]
New_York$Pickup_longitude <- as.numeric(New_York$Pickup_longitude)

## Getting rid of errenous recordins
New_York <- New_York[New_York$Fare_amount >= 0 & New_York$Extra >= 0 & New_York$MTA_tax >= 0 & New_York$Tip_amount >= 0 & 
                       New_York$Tolls_amount >= 0 & New_York$improvement_surcharge >= 0 & New_York$Total_amount >= 2.5,]  
summary(New_York)

### Trip_Distance: Mean/Median -- 2.80/1.84 
### Passenger_Count:  Mean/Median -- 1.357/1 
### Total_Amount: Mean/Median --  14.65/11.30

##Save Backup 
Temp <- New_York

## EDA
### Distance
ggplot(Temp, aes(x = Trip_distance))+
  geom_histogram(binwidth = 0.3, aes(fill = ..count..)) +
  scale_fill_gradient( name = "Frequency",
                       low = "orange",
                       high = "darkred", 
                       na.value = "grey50",
                       guide = "colourbar",
                       labels = c("0", "40K", "80k","120k","160k")) +
  coord_cartesian(xlim = c(0,10)) +
  ggtitle("Histogram for Trip Distance") +
  xlab("Trip Distance (miles)") + 
  ylab("Frequency of Trip Distance") +
  theme_bw()

ggplot(Temp, aes(x = Pickup_Datetime))+
  geom_histogram(binwidth = 0.3, aes(fill = ..count..)) +
  scale_fill_gradient( name = "Frequency",
                       low = "orange",
                       high = "darkred", 
                       na.value = "grey50",
                       guide = "colourbar",
                       labels = c("0", "40K", "80k","120k","160k")) +
  coord_cartesian(xlim = c(0,10)) +
  ggtitle("Histogram for Pickup Time") +
  xlab("Pick-up Time") + 
  ylab("Frequency of Pick up Time") +
  theme_bw()



## Time 

scale_colour_viridis(discrete = TRUE) +
  ylab("All motor Vehicles") +
  xlab("Road Code") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = -90, size=10)) +
  theme(axis.text.y = element_text(size=10)) +
  theme(axis.title.x=element_text(size=12, face = "bold")) +
  theme(axis.title.y=element_text(size=12, face = "bold")) +
  theme(legend.title=element_blank()) +
  theme(legend.position = "none")


### Task 1: Origin Destination and Distance analysis 


### Task 2: Average Speed 


### Task 3: Tip and Fare Amount 


### Task 4: Economic Analysis 


### Task 5: Geo-Spatial Analysis 


### Task 6: Capacity Analysis 



summary(New_York)
