setwd("~/Uni Temp Work/Lecture 1")
New_York <- read.csv("2016_Green_Taxi_Trip_Data.csv")

update.packages(ask = FALSE)
install.packages("spatialEco")
install.packages("mapview")
install.packages('darksky')
install.packages("osmdata")

library(Hmisc)
library(osmdata)
library(darksky)
library(ggplot2)
library(tidyverse)
library(rgdal)
library(rgeos)
library(tmap)
library(leaflet)
library(RColorBrewer)
library(sp)
library(spatialEco)
library(mapview)
library(sf)
library(dplyr)
library(tidyr)

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
New_York <- New_York[New_York$Pickup_latitude > 39 & New_York$Pickup_longitude > -78 & New_York$Pickup_longitude < -72,]

summary(New_York)

### Trip_Distance: Mean/Median -- 2.80/1.84 
### Passenger_Count:  Mean/Median -- 1.357/1 
### Total_Amount: Mean/Median --  14.65/11.30


## Add TripTime 
Temp$Duration <- difftime(Temp$Dropoff_Datetime, Temp$Pickup_Datetime, units = "mins")
Temp$Speed <- Temp$Trip_distance / (as.numeric(Temp$Duration)/60)


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

pickUpHour = hour(Temp$Pickup_Datetime)
tripDistance.median = aggregate(Trip_distance ~ pickUpHour, FUN = median, data = greenTrip)
tripDistance.mean = aggregate(Trip_distance ~ pickUpHour, FUN = mean, data = greenTrip)


#Get DarkSKy Data for 2016
## Secret Key 


## For R to make calls to daksy it needs to know secret key.
## This is set in your R environment file (.Renviron)

#The R environmnet file stores system varialbes and can e used to store API keys
#that will be available from one session to another. 

#Set keys:
Sys.setenv("DARKSKY_API_KEY" = "Put your Darksky API key here")
#Check Key 
Sys.getenv("DARKSKY_API_KEY")

## Get Data
## From 1451624400 until 1483246800

x = 1451624400
time2 <- c()

while (x < 1483246800){
  x <- x + 86400
  time2 <- c(time2, x)
}
time2

result <- c()
for (i in 1:366){
  result[i] <- get_forecast_for(latitude = 40.730610, longitude = -73.935242, time2[i], language = "en",
                                exclude = "minutely, currently, daily")
}

## Clean Lists 
test <- result

## Step 1 make all of the dataframes consistent 
#result1[i] <- data.frame(test[i])[, c("time","summary","precipProbability","apparentTemperature","humidity","windSpeed")]}
result2 <- result 

## Step 3 use rbind to join all the dataframes 
df <- data.frame(0,0,0,0,0,0)
names(df) <- c("time","summary","precipProbability","apparentTemperature","humidity","windSpeed")
df <- df[-1,]
for (i in 1:length(result2)) {
  df <- rbind(df, result2[[i]])
}

## Step 1 Make Month, Day, Hour from pick-up Date Time 
Temp$Pickup_Datetime
Temp$day <- weekdays(as.Date(Temp$Pickup_Datetime))
Temp$hour = format(as.POSIXct(Temp$Pickup_Datetime,format = "%m/%d/%Y %I:%M:%S %p"),"%H")
Temp$time = format(as.POSIXct(Temp$Pickup_Datetime,format = "%m/%d/%Y %I:%M:%S %p"),"%m/%d/%Y %H")
df$time = format(as.POSIXct(df$time,format = "%Y-%d-%m %H:%M:%S"),"%m/%d/%Y %H")

final <- merge(Temp, df, by="time")
tail(final)



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
### Derive speed 
### Maximum speed limit in NYC is 50 mph and in the USA is 137 mph. 

final$Duration <- difftime(final$Dropoff_Datetime, final$Pickup_Datetime, units = "mins")
final$Speed <- final$Trip_distance / (as.numeric(final$Duration)/60)
speedSubset = subset(final, (is.finite(final$Speed) & !is.na(final$Speed) & final$Speed <= 140))
head(final)
summary(final)

head(final)


#### Divide up locations into postcodes: 
## QGIS for R 



output_areas <- readOGR(".", "ZIP_CODE_040114")
demographic <- read_csv("Demographic_Statistics_By_Zip_Code.csv")
head(demographic)
summary(output_areas@data)
output_areas@data <- output_areas@data[, c(1,3,4,5,7)]
demographic <- demographic[, c(1,4,6,14,18,20,22)]
output_areas@data$ZIPCODE <- as.numeric(output_areas@data$ZIPCODE)

#Join data to the shapefile: 
geo_spatial <- merge(output_areas, demographic, by.x = "ZIPCODE", by.y="JURISDICTION NAME")

#Make Quick Maps
summary(geo_spatial@data)
row.names(geo_spatial@data) <- NULL 
geo_spatial@data$POP_DENSITY <- log(geo_spatial@data$POPULATION / geo_spatial@data$AREA)
summary(geo_spatial@data)
geo_spatial@data$PO_NAME <- as.factor(geo_spatial@data$PO_NAME)
geo_spatial@data$COUNTY <- as.factor(geo_spatial@data$COUNTY)

display.brewer.all()
tm_shape(geo_spatial) + tm_fill("POPULATION", style = "pretty", palette =  "-RdPu",  title = "Population") +
  tm_borders(alpha = .4) + 
  tm_compass() + 
  tm_layout(title = "New York Post Codes by \nPopulation")
getwd()
## Look at later: 

writeOGR(geo_spatial, dsn = "C:/Users/44792/Documents/Uni Temp Work/Big Data", layer = "Demographics_Shapefile")

mapview(geo_spatial, locations_sf)
?mapview

### Point Data 


plot(final$Pickup_longitude, final$Pickup_latitude)
final <- final[final$Pickup_latitude > 40 & final$Pickup_longitude > -78 & final$Pickup_longitude < -73.4,]
final1 <- final[1:3000000,]
plot(final1$Pickup_longitude, final1$Pickup_latitude)

locations_sf <- final1 %>% 
  st_as_sf(coords = c("Pickup_longitude", "Pickup_latitude")) %>% 
          st_set_crs(4269) %>% 
          st_transform(2263)


mapview(locations_sf)
mapview(final2)
mapview(geo_spatial)
mapview(postcodes_sf)

postcodes_sf <- st_as_sf(geo_spatial, crs = "NAD83")
st_crs(postcodes_sf)
st_crs(locations_sf)
st_crs(locations_sf) == st_crs(postcodes_sf)

joined <- st_join(locations_sf, postcodes_sf, join = st_within)
head(joined)

#### Regression:
#### Getting Open Streetmap Data: 

bb <- getbb('New York City, New York, USA', format_out = 'polygon')

nightclub <- opq(bbox = bb) %>%
  add_osm_feature(key = 'amenity', value = 'nightclub') %>%
  osmdata_sf() %>%
  trim_osmdata(bb)

hospital <- opq(bbox = bb) %>%
  add_osm_feature(key = 'amenity', value = 'hospital') %>%
  osmdata_sf() %>%
  trim_osmdata(bb)

bar <- opq(bbox = bb) %>% 
  add_osm_feature(key = 'amenity', value = 'bar') %>%
  osmdata_sf() %>%
  trim_osmdata(bb)

social_facility <- opq(bbox = bb) %>% 
  add_osm_feature(key = 'amenity', value = 'social_facility') %>%
  osmdata_sf() %>%
  trim_osmdata(bb)

attraction <- opq(bbox = bb) %>%  
  add_osm_feature(key = 'tourism', value = 'attraction') %>%
  osmdata_sf() %>%
  trim_osmdata(bb)

c(nightclub, hospital, bar, social_facility, attraction)

variables <- c("osm_id","name","addr.postcode")
features <- c("nightclub", "hospital", "bar", "attraction", "social_facility")
lists_of_lists <- mget(features)

for (i in features){
  datasets[[i]] <- lists_of_lists[[i]][["osm_points"]][!is.na(lists_of_lists[[i]][["osm_points"]][["addr.postcode"]]), variables]
}

result <- lapply(mget(features), function(x) 
  x$osm_points[!is.na(x$osm_points$addr.postcode), variables])

nightclub1 <- nightclub$osm_points[!is.na(nightclub$osm_points$addr.postcode), variables]
attraction1 <- attraction$osm_points[!is.na(attraction$osm_points$addr.postcode), variables]
bar1 <- bar$osm_points[!is.na(bar$osm_points$addr.postcode), variables]
social_facility1 <- social_facility$osm_points[!is.na(social_facility$osm_points$addr.postcode), variables]
hospital1 <- hospital$osm_points[!is.na(hospital$osm_points$addr.postcode), variables]

## Put this into one function in R 

#Change parameters 
feature <- rep("hospital", nrow(hospital1))
hospital1 <- cbind(hospital1, feature)

all_features <- rbind(nightclub1, attraction1, bar1, social_facility1, hospital1)
view(all_features)
all_features <- all_features[, c(3,4)] 
### Merge Built environment features with joined dataset for built environment regression: 

built_environment <- joined
st_geometry(built_environment) <- NULL 
st_geometry(all_features) <- NULL  

built_environment_agg <- built_environment[,c("time", "Passenger_count", "Trip_distance", "day", "hour", "summary", "apparentTemperature", "windSpeed",
                                              "ZIPCODE", "POPULATION", "AREA")]
characters <- c("day", "hour", "summary", "apparentTemperature", "windSpeed", "ZIPCODE", "POPULATION", "AREA")
built_environment_agg[, characters]  <- lapply(built_environment_agg[characters], as.character)

built_environment_agg1 <- built_environment_agg %>% group_by(ZIPCODE, time, summary, day, hour, apparentTemperature, POPULATION, AREA)  %>% 
  summarise(total_passengers = sum(Passenger_count), total_trip_distance = sum(Trip_distance))  

names(all_features) <- c("ZIPCODE", "feature")



## Count number of feature in each zipcode: 
view(all_features)

all_features1 <- all_features %>% group_by(ZIPCODE)  %>% 
    summarise(bar = sum(feature %in% "bar"),
              nightclub = sum(feature %in% "nightclub"),
              attraction = sum(feature %in% "attraction"),
              hospital = sum(feature %in% "hospital"),
              social_facility = sum(feature %in% "social_facility"))

view(all_features1)

regression <- merge(built_environment_agg1, all_features1, by = "ZIPCODE", all.x = TRUE)
view(regression)






















### Task 3: Tip and Fare Amount 
final$percTip = final$Tip_amount * 100 /  final$Total_amount

hist(final$percTip, xlab = "Percentage Tip", ylab = "Count",
     main = "Histogram of Percentage Tip", xlim = c(0, 40), breaks = 50)



## Looking at the data distribution, I find that most of the passengers giving no tip and mean of tip percent at 17%.
## Average Tip % in the US is 20%


### Task 4: Economic Analysis 


### Task 5: Geo-Spatial Analysis 


### Task 6: Capacity Analysis 



###### Stack overflow example 
#library(sp)
#final2 <- final1
#coordinates(final2) <- ~Pickup_longitude+Pickup_latitude
#head(final2)
#final3 <- final2
######

#pick_up <- SpatialPointsDataFrame(locations_sf[, c("Pickup_longitude", "Pickup_latitude")], locations_sf, proj4string = CRS("+proj=lcc + +lat_1=41.03333333333333 +lat_2=40.66666666666666 +lat_0=40.16666666666666 +lon_0=-74 +x_0=300000.0000000001 +y_0=0 +datum=NAD83 +units=us-ft"))


## Mapping point data: 
## Add the points as an additional tm_shape file: 
#tm_shape(geo_spatial) + tm_borders(alpha=.4) + tm_shape(pick_up) + tm_dots(col = "Duration", palette = "Reds", style = "quantile")
#tm_bubbles # For proportional bubble size 


################# Join points onto the same Polygon: 

#proj4string(geo_spatial) <- CRS("+proj=lcc + +lat_1=41.03333333333333 +lat_2=40.66666666666666 +lat_0=40.16666666666666 +lon_0=-74 +x_0=300000.0000000001 +y_0=0 +datum=NAD83")
#proj4string(pick_up) <- CRS("+proj=lcc + +lat_1=41.03333333333333 +lat_2=40.66666666666666 +lat_0=40.16666666666666 +lon_0=-74 +x_0=300000.0000000001 +y_0=0 +datum=NAD83")
#proj4string(final2) <- CRS("+proj=lcc + +lat_1=41.03333333333333 +lat_2=40.66666666666666 +lat_0=40.16666666666666 +lon_0=-74 +x_0=300000.0000000001 +y_0=0 +datum=NAD83 +units=us-ft")

#final2@proj4string
#geo_spatial@proj4string
#locations_sf@proj4string

#proj4string(pick_up) <- proj4string(geo_spatial)
#plot(final1$Pickup_longitude, final1$Pickup_latitude)


# point in polygon. Gives the points the attributes of the polygones that they are in
#proj4string(final2) == proj4string(geo_spatial)

#pip <- over(final2, geo_spatial)
#pip1 <- over(geo_spatial, pick_up)
#pip1 <- over(locations_sf, geo_spatial)



#try <- point.in.poly(final2, geo_spatial)
#try <- point.in.poly(locations_sf, geo_spatial)

#view(try@data)
#Need to bind the census data to our original points:
#pick_up@data <- cbind(pick_up@data, pip)
#view(pick_up@data)

#summary(New_York)
