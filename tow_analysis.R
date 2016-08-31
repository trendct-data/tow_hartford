library(dplyr)
library(lubridate)
library(leaflet)
library(ggmap)
library(knitr)
library(stringr)
library(geosphere)
library(ggplot2)
tows <- read.csv("data/tows.csv", stringsAsFactors=F)

# Cleaning the data

## Figure out tow firm and address based on phone number

### Subset dataframe of complete data

tows_sub <- subset(tows, Tow_Firm!="")
tows_sub <- subset(tows_sub, !duplicated(Tow_Firm))
tows_sub <- subset(tows_sub, Tow_Firm!="CROSS COUNTRY AUTO")
nrow(tows_sub)

#### There are 21 Towing Yards in Hartford

#### Geolocate tow firms

geo <- geocode(location = tows_sub$Tow_Firm_Address, output="latlon", source="google")
tows_sub <- cbind(tows_sub, geo)

color <- data.frame("#29e908", 
                     "#0a5cee", 
                     "#8d480c", 
                     "#8edeb7", 
                     "#c9e746", 
                     "#96bab2", 
                     "#0fe5a2", 
                     "#6a5c5b", 
                     "#19cdfd", 
                     "#279fe6", 
                     "#7ac150", 
                     "#660e6e", 
                     "#095a21", 
                     "#dfe142", 
                     "#786839", 
                     "#f5657c", 
                     "#4decd2", 
                     "#4eb06f", 
                     "#fdc200", 
                     "#08d479", 
                     "#b2cca8")
color <- data.frame(t(color))
rownames(color) <- NULL
tows_sub <- cbind(tows_sub, color)

#### Delete Tow_Firm and Tow_Firm_Address columns in original dataframe

tows <- tows[,-3]
tows <- tows[,-3]

#### Prep dataframe for joining

tows_sub <- tows_sub[c("Tow_Firm", "Tow_Firm_Address", "Tow_Firm_Phone", "lon", "lat", "t.color.")]

#### Join the dataframes

tows <- left_join(tows, tows_sub)

#### Clean up time and dates

tows$Date <- ymd(tows$Date)
tows$Time <- hms(tows$Time)
tows$created_at <- ymd_hms(tows$created_at)
tows$created_at <- ymd_hms(tows$updated_at)
tows$removed_at <- ymd_hms(tows$removed_at)


#### Prepping Lat/Lon

tows$tow_lon <- gsub(",.*", "", tows$geom)
tows$tow_lat <- gsub(".*,", "", tows$geom)

# Questions to answer

## How many vehicles towed?

nrow(tows)

## How many with no vehicle plate info?

sum(is.na(tows$Vehicle_Plate))
nrow(subset(tows, Vehicle_Plate==""))

## Where are vehicles towed from?

m <- leaflet(tows) %>% addTiles('http://{s}.basemaps.cartocdn.com/dark_all/{z}/{x}/{y}.png', attribution='Map tiles by <a href="http://stamen.com">Stamen Design</a>, <a href="http://creativecommons.org/licenses/by/3.0">CC BY 3.0</a> &mdash; Map data &copy; <a href="http://www.openstreetmap.org/copyright">OpenStreetMap</a>') %>% 
  setView(-72.690940, 41.751426, zoom = 12) %>% 
  addCircles(~tow_lon, ~tow_lat, popup=tows$Make, weight = 3, radius=40, 
             color="#ffa500", stroke = TRUE, fillOpacity = 0.8) %>% 
  addLegend("bottomright", colors= "#ffa500", labels="Towed'", title="In Hartford")

m

## Where are vehicles towed to?

m <- leaflet(tows) %>% addTiles('http://{s}.basemaps.cartocdn.com/dark_all/{z}/{x}/{y}.png', attribution='Map tiles by <a href="http://stamen.com">Stamen Design</a>, <a href="http://creativecommons.org/licenses/by/3.0">CC BY 3.0</a> &mdash; Map data &copy; <a href="http://www.openstreetmap.org/copyright">OpenStreetMap</a>') %>% 
  setView(-72.690940, 41.751426, zoom = 12) %>% 
  addCircles(~lon, ~lat, popup=tows$Make, weight = 3, radius=40, 
             color="#ffa500", stroke = TRUE, fillOpacity = 0.8) %>% 
  addLegend("bottomright", colors= "#ffa500", labels="Towed'", title="In Hartford")

m


## Most common year?

years <- tows %>%
  group_by(Vehicle_Year) %>%
  summarise(count=n()) %>%
  arrange(-count)

kable(head(years,10))

## Most common vehicle?
make <- tows %>%
  group_by(Make) %>%
  summarise(count=n()) %>%
  arrange(-count)

kable(head(make))

## Most common model? 

model <- tows %>%
  group_by(Model) %>%
  summarise(count=n()) %>%
  arrange(-count)

kable(head(model,10))

## Any particular color? 


color <- tows %>%
  group_by(Color) %>%
  summarise(count=n()) %>%
  arrange(-count)

kable(head(color,10))

## Most common time of day towed?

tows$hour <- hour(tows$Time)

hour <- tows %>%
  group_by(hour) %>%
  summarise(count=n()) %>%
  arrange(-count)

kable(head(hour,10))

ggplot(tows, aes(x=hour)) + geom_histogram(binwidth=1)

## How long are they in the tow yards for?

tows$duration <- interval(tows$created_at, tows$removed_at)
tows$days <- ddays(tows$duration)
### CANT DO THIS. DATA IS TOO DIRTY. WILL HAVE TO RECONSIDER APPROACH

## Bad-luck drivers-- who's towed more often?
plates <- tows %>%
  group_by(Vehicle_Plate) %>%
  summarise(count=n()) %>%
  arrange(-count)

kable(head(plates,10))

## Which tow yards are most prolific?

firms <- tows %>%
  group_by(Tow_Firm) %>%
  summarise(count=n()) %>%
  arrange(-count)

kable(head(firms,10))

## Most common address?

address <- tows %>%
  group_by(Tow_From_Address) %>%
  summarise(count=n()) %>%
  arrange(-count)

kable(head(address, 10))

## Most common address and tow company?

tow_address <- tows %>%
  group_by(Tow_From_Address, Tow_Firm) %>%
  summarise(count=n()) %>%
  arrange(-count)

kable(head(tow_address, 20))

## Where do these tow yards target?

m <- leaflet(tows) %>% addTiles('http://{s}.basemaps.cartocdn.com/dark_all/{z}/{x}/{y}.png', attribution='Map tiles by <a href="http://stamen.com">Stamen Design</a>, <a href="http://creativecommons.org/licenses/by/3.0">CC BY 3.0</a> &mdash; Map data &copy; <a href="http://www.openstreetmap.org/copyright">OpenStreetMap</a>') %>% 
  setView(-72.690940, 41.751426, zoom = 12) %>% 
  addCircles(~tow_lon, ~tow_lat, popup=tows$Tow_Firm, weight = 3, radius=40, 
             color=tows$t.color., stroke = TRUE, fillOpacity = 0.8) %>% 
  addLegend("bottomright", colors= "#ffa500", labels="Towed'", title="In Hartford")

m


## What neighborhoods? Anything to do with racial makeup?

# Bring in the shape files for census tracts

require(rgdal)

# dsn is the folder the shape files are in. layer is the name of the file.
towntracts <- readOGR(dsn="maps", layer="census_tracts")

# creating a copy
towntracts_only <- towntracts

# turn the shapefile into a dataframe that can be worked on in R
require(maptools)
require(ggplot2)

towntracts <- fortify(towntracts, region="GEOID10")

# We only need the columns with the latitude and longitude
coords <- tows[c("tow_lon", "tow_lat")]

# Making sure we are working with rows that don't have any blanks
coords$tow_lon <- as.numeric(coords$tow_lon)
coords$tow_lat <- as.numeric(coords$tow_lat)
coords <- coords[complete.cases(coords),]

library(sp)

# Letting R know that these are specifically spatial coordinates
sp <- SpatialPoints(coords)

# Applying projections to the coordinates so they match up with the shapefile we're joining them with
# More projections information http://trac.osgeo.org/proj/wiki/GenParms 
proj4string(sp) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
proj4string(sp)

# Rendering the census tracts
plot(towntracts_only)

# Adding the coordinates of the traffic stops
plot(sp, col="red" , add=TRUE)


by_tract <- over(sp, towntracts_only)


by_tract <- by_tract %>%
  group_by(GEOID10) %>%
  summarise(total=n())

kable(head(by_tract))

colnames(by_tract) <- c("id", "total")

by_tract$id <- as.character(by_tract$id)

# Bring in a dataframe that has matches census tract ID numbers to town names
tracts2towns <- read.csv("data/tracts_to_towns.csv", stringsAsFactors=FALSE)

# Changing the column names so it can be joined to the by_tract dataframe
colnames(tracts2towns) <- c("id", "town_name")

# Changing the GEOID number to character so it can be joined to the by_tract dataframe
tracts2towns$id <- as.character(tracts2towns$id)

# Adding a 0 to the front of the GEOID string because it was originally left out when it was imported
tracts2towns$id <- paste0("0", tracts2towns$id)## Distance between yards and tow locations?


# Eliminating leading and trailing white space just in case
tracts2towns$town_name <- str_trim(tracts2towns$town_name)

# Joining the by_tract dataframe to the tracts2towns dataframe

by_tract <- left_join(by_tract, tracts2towns)

total_map <- left_join(towntracts, by_tract)

require(ggmap)
require(scales)


total_map <- subset(total_map, !is.na(total))

tm_ct <- ggplot() +
  geom_polygon(data = total_map, aes(x=long, y=lat, group=group, fill=total), color = "black", size=0.2) +
  coord_map() +
  scale_fill_distiller(type="seq", trans="reverse", palette = "Reds", breaks=pretty_breaks(n=10)) +
  theme_nothing(legend=TRUE) +
  labs(title="Where vehicles are towed from", fill="")
print(tm_ct)

townborders <- readOGR(dsn="maps", layer="ctgeo")
townborders_only <- townborders
townborders<- fortify(townborders, region="NAME10")

# Subset the town borders to just Hamden since that's the department we're looking at
town_borders <- subset(townborders, id=="Hartford")

tm_ct <- ggplot() +
  geom_polygon(data = total_map, aes(x=long, y=lat, group=group, fill=total), color = "black", size=0.2) +
  geom_polygon(data = town_borders, aes(x=long, y=lat, group=group, fill=total), color = "black", fill=NA, size=0.5) +
  coord_map() +
  scale_fill_distiller(type="seq", trans="reverse", palette = "Reds", breaks=pretty_breaks(n=10)) +
  theme_nothing(legend=TRUE) +
  labs(title="Where vehicles are towed from", fill="")
print(tm_ct)


## census data comparison

# If you do not yet have the censusapi package installed, uncomment the lines below and run them.

#install.packages("devtools")
#devtools::install_github("hrecht/censusapi")
library("censusapi")

# Loading my census key from an external script
source("keys.R")

# Replace census_key below with "your_own_key_whatever_it_is"
# Apply for one here http://api.census.gov/data/key_signup.html

race_tracts <- getCensus(name="acs5",
                         vintage=2014,
                         key=census_key,
                         vars=c("NAME", "B02001_001E", "B02001_002E"),
                         region="tract:*", regionin="state:09")

# What did we just do?



# I pulled the following population data for all census tracts in state 09, which is Connecticut

# B02001_001E - Total
# B02001_002E - White alone

kable(head(race_tracts))

# ok, let's clean this up
race_tracts$NAME <- NULL

# Creating a new column for the GEOID that can be joined with the dataframe we already have
race_tracts$id <- paste0(race_tracts$state, race_tracts$county, race_tracts$tract)

# Renaming the column names for clarity
colnames(race_tracts) <- c("state_code", "county_code", "tract_code", "total_pop", "white_pop", "id")

# Determining the minority population by subtracting the white population from the total
race_tracts$minority_pop <- race_tracts$total_pop - race_tracts$white_pop

# Now figuring out the percent makeup of each census tract
race_tracts$white_pop_p <- round(race_tracts$white_pop/race_tracts$total_pop*100,2)
race_tracts$minority_pop_p <- round(race_tracts$minority_pop/race_tracts$total_pop*100,2)

kable(head(race_tracts,5))
# Joining the two datframes
joined_tracts <- left_join(total_map, race_tracts)

#total_map <- left_join(towntracts, by_tract)

kable(head(joined_tracts,5))

# Mapping population

tm_ct <- ggplot() +
  geom_polygon(data = joined_tracts, aes(x=long, y=lat, group=group, fill=total_pop), color = "black", size=0.2) +
  geom_polygon(data = town_borders, aes(x=long, y=lat, group=group, fill=total), color = "black", fill=NA, size=0.5) +
  coord_map() +
  scale_fill_distiller(type="seq", trans="reverse", palette = "Reds", breaks=pretty_breaks(n=10)) +
  theme_nothing(legend=TRUE) +
  labs(title="Hartford population", fill="")
print(tm_ct)


# Minority pop


tm_ct <- ggplot() +
  geom_polygon(data = joined_tracts, aes(x=long, y=lat, group=group, fill=minority_pop_p), color = "black", size=0.2) +
  geom_polygon(data = town_borders, aes(x=long, y=lat, group=group, fill=total), color = "black", fill=NA, size=0.5) +
  coord_map() +
  scale_fill_distiller(type="seq", trans="reverse", palette = "Reds", breaks=pretty_breaks(n=10)) +
  theme_nothing(legend=TRUE) +
  labs(title="Hartford minority population", fill="")
print(tm_ct)

for_analysis <- subset(joined_tracts, !duplicated(tract_code))
for_analysis <- subset(for_analysis, town_name!="Hartford")

cor(for_analysis$total, for_analysis$total_pop)

# restaurants

restaurants2 <- read.csv("https://data.hartford.gov/api/views/cwxs-2pd8/rows.csv?accessType=DOWNLOAD")

#restaurants2 <- read.csv("https://data.hartford.gov/api/views/xk8h-j69c/rows.csv?accessType=DOWNLOAD")

restaurants2$lon <- gsub(".*, ", "", restaurants2$geom)
restaurants2$lon <- as.numeric(gsub("\\)", "", restaurants2$lon))


restaurants2$lat <- gsub(",.*", "", restaurants2$geom)
restaurants2$lat <- as.numeric(gsub("\\(", "", restaurants2$lat))

coords <- restaurants2[c("lon", "lat")]

coords <- coords[complete.cases(coords),]
sp <- SpatialPoints(coords)
proj4string(sp) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
proj4string(sp)

by_tract <- over(sp, towntracts_only)

by_tract <- by_tract %>%
  group_by(GEOID10) %>%
  summarise(total=n())

colnames(by_tract) <- c("id", "total")

by_tract$id <- as.character(by_tract$id)

tracts2towns <- read.csv("data/tracts_to_towns.csv", stringsAsFactors=FALSE)

colnames(tracts2towns) <- c("id", "town_name")

tracts2towns$id <- as.character(tracts2towns$id)

tracts2towns$id <- paste0("0", tracts2towns$id)## Distance between yards and tow locations?

tracts2towns$town_name <- str_trim(tracts2towns$town_name)

by_tract <- left_join(by_tract, tracts2towns)

total_map <- left_join(towntracts, by_tract)


total_map <- subset(total_map, !is.na(total))

tm_ct <- ggplot() +
  geom_polygon(data = total_map, aes(x=long, y=lat, group=group, fill=total), color = "black", size=0.2) +
  coord_map() +
  scale_fill_distiller(type="seq", trans="reverse", palette = "Reds", breaks=pretty_breaks(n=10)) +
  theme_nothing(legend=TRUE) +
  labs(title="Where restaurants are", fill="")
print(tm_ct)

## ok, distance time

tows$lat <- as.numeric(tows$lat)
tows$lon <- as.numeric(tows$lon)
tows$tow_lat <- as.numeric(tows$tow_lat)
tows$tow_lon <- as.numeric(tows$tow_lon)

# tows$lonlat <- paste0(tows$lat, ", ", tows$lon)
# tows$t_lonlat <- paste0(tows$tow_lat, ", ", tows$tow_lon)

tows_no_na <- subset(tows, !is.na(tow_lat))

tows_no_na$distance <- 0

for (i in 1:nrow(tows_no_na)) {
  tows_no_na$distance[i] <- distm(c(tows_no_na$lat[i], tows_no_na$lon[i]), c(tows_no_na$tow_lat[i], tows_no_na$tow_lon[i]), fun=distHaversine)
}

tows_no_na$miles <- tows_no_na$distance * 0.00062137

## average miles per firm

avg_miles <- tows_no_na %>%
  group_by(Tow_Firm) %>%
  summarise(Avg_Meters=mean(distance)) %>%
  mutate(Avg_Miles=Avg_Meters*0.00062137) %>%
  arrange(-Avg_Miles)

kable(avg_miles)
## heat map overall
joined_tracts2 <- subset(joined_tracts, town_name=="Hartford")


hartbox <- make_bbox(lon = tows_no_na$tow_lon, lat =tows_no_na$tow_lat, f = .1)
hart_map <- get_map(location = hartbox, maptype = "roadmap", source = "google")


pm_ct <- ggmap(hart_map) 
pm_ct <- pm_ct + stat_density2d(data = tows_no_na, show.legend=F, aes(x=tow_lon, y=tow_lat, fill=..level.., alpha=..level..),  geom="polygon",size=.5,bins=10)
#pm_ct <- pm_ct + geom_polygon(data = joined_tracts2, aes(x=long, y=lat, group=group), fill=NA, color = "black", size=0.2) 
#pm_ct <- pm_ct + geom_polygon(data = town_borders, aes(x=long, y=lat, group=group), fill=NA, color = "black", size=0.4)
#pm_ct <- pm_ct + geom_polygon(data = ct_only, aes(x=long, y=lat, group=group), fill="seagreen2", color = "gray93", size=0.2)
#pm_ct <- pm_ct + gg_circle(r=9, xc=-73, yc=42, color="white", fill=NA, alpha=0.2, size=40) 
pm_ct <- pm_ct + scale_fill_gradient(low="purple", high="firebrick1", name="Distribution")
#pm_ct <- pm_ct + scale_fill_discrete()
#extra_lat <- c(46.358685, 35.872715)
#extra_lon <- c(-64.209938, -79.735653)
#pm_ct <- pm_ct + theme(legend.position="top",  legend.key = element_blank())
pm_ct <- pm_ct + coord_fixed()
pm_ct <- pm_ct + theme_nothing(legend=TRUE) 
pm_ct <- pm_ct + labs(x=NULL, y=NULL, title="Where tow yards target")
#pm_ct <- pm_ct + facet_wrap(~Tow_Firm)
#pm_ct <- pm_ct + theme(text = element_text(size=15), panel.background = element_rect(fill = 'gray93', color=NA))
pm_ct <- pm_ct + theme(plot.title=element_text(face="bold", hjust=.4))
pm_ct <- pm_ct + theme(plot.subtitle=element_text(face="italic", size=9, margin=margin(l=20)))
pm_ct <- pm_ct + theme(plot.caption=element_text(size=12, margin=margin(t=12), color="#7a7d7e", hjust=0))
#pm_ct <- pm_ct + theme(legend.key.size = unit(1, "cm"))

pm_ct
## heat map per firm


pm_ct <- ggmap(hart_map)
pm_ct <- pm_ct + stat_density2d(data = tows_no_na, show.legend=F, aes(x=tow_lon, y=tow_lat, fill=..level.., alpha=..level..),  geom="polygon",size=.5,bins=10)
pm_ct <- pm_ct + geom_polygon(data = joined_tracts2, aes(x=long, y=lat, group=group), fill=NA, color = "black", size=0.2) 
pm_ct <- pm_ct + geom_polygon(data = town_borders, aes(x=long, y=lat, group=group), fill=NA, color = "black", size=0.4)
#pm_ct <- pm_ct + geom_polygon(data = ct_only, aes(x=long, y=lat, group=group), fill="seagreen2", color = "gray93", size=0.2)
#pm_ct <- pm_ct + gg_circle(r=9, xc=-73, yc=42, color="white", fill=NA, alpha=0.2, size=40) 
pm_ct <- pm_ct + scale_fill_gradient(low="deepskyblue2", high="firebrick1", name="Distribution")
#pm_ct <- pm_ct + scale_fill_discrete()
#extra_lat <- c(46.358685, 35.872715)
#extra_lon <- c(-64.209938, -79.735653)
#pm_ct <- pm_ct + theme(legend.position="top",  legend.key = element_blank())
pm_ct <- pm_ct + coord_fixed()
pm_ct <- pm_ct + theme_nothing(legend=TRUE) 
pm_ct <- pm_ct + labs(x=NULL, y=NULL, title="Where tow yards target")
pm_ct <- pm_ct + facet_wrap(~Tow_Firm)
#pm_ct <- pm_ct + theme(text = element_text(size=15), panel.background = element_rect(fill = 'gray93', color=NA))
pm_ct <- pm_ct + theme(plot.title=element_text(face="bold", hjust=.4))
pm_ct <- pm_ct + theme(plot.subtitle=element_text(face="italic", size=9, margin=margin(l=20)))
pm_ct <- pm_ct + theme(plot.caption=element_text(size=12, margin=margin(t=12), color="#7a7d7e", hjust=0))
#pm_ct <- pm_ct + theme(legend.key.size = unit(1, "cm"))

pm_ct

## For leaflet

tows_leaf <- tows_no_na[c("tow_lon", "tow_lat")]
tows_leaf$extra <- "222"
write.csv(tows_leaf, "tows_leaf.csv")

tows_leaf <- tows_leaf %>%
  group_by(tow_lon, tow_lat) %>%
  summarise(count=n())

the_js <- "var addressPoints=[
"
for (i in 1:nrow(tows_leaf)) {
  print(i)
  the_js <- paste0(the_js, "[", tows_leaf$tow_lat[i], ", ", tows_leaf$tow_lon[i], ", \"" , tows_leaf$count[i], "\"],")
  
}
the_js <- paste0(the_js, "];")
write(the_js, "the_js.js")
