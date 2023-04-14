#loading required packages
library("tidyverse")
library("maptools")
library("ggmap")
#loading in datasets
accidents <- read.csv("C:/Users/Renee m Fabien/Downloads/chicagotraffic2019.csv")
people <- read.csv("C:/Users/Renee m Fabien/Downloads/Traffic_Crashes_-_People.csv")
vehicles <- read.csv("C:/Users/Renee m Fabien/Downloads/Traffic_Crashes_-_Vehicles.csv")


#registering API
api_key = "AIzaSyDdQ6M0OyGc9Aig1lZIDPux_OjhoLEB0q4"
register_google(key = "AIzaSyDdQ6M0OyGc9Aig1lZIDPux_OjhoLEB0q4" )

#filtering out NA values for LATITUDE and LONGITUDE
accidents <- accidents %>% filter(!is.na(LATITUDE), !is.na(LONGITUDE))


mean.longitude <- mean(accidents$LONGITUDE)
mean.latitude <- mean(accidents$LATITUDE)

#creating a data frame of the location data of the most dangerous intersections
intersections <- c("devon_leigh_caldwell_central", "halsted_lincoln_fullerton", "beverly_vincennes_103rd",
                   "clark_ridge_thorndale", "lasalle_clark_eugenie", "north_damen_milwaukee",
                   "ewing_95th_aveL", "ashland_madison_ogden", "archer_cermak_princeton", "belmont_lincoln_ashland",
                   "central_foster_milwaukee_nwhwy", "columbus_87th_pulaski_swhwy")

longitudes <- c(-87.768747, -87.6518617, -87.6582009, -87.6719092, -87.6352951, -87.6795567,
                -87.5389467, -87.6688377, -87.6370252, -87.6704477, -87.7700904, -87.7237045)

latitudes <- c(41.9972704, 41.925514, 41.706787, 41.9884388, 41.9132787, 41.90995, 41.723072,
               41.881874, 41.8528486, 41.939644, 41.9756621, 41.7356366)

dangerous_intersecions <- data.frame(intersections, longitudes, latitudes)

#loading in the street map of Chicago from google maps
map_of_Chicago <- get_map(location = c(mean.longitude, mean.latitude), 
                          zoom = 11, maptype = "roadmap", scale = 1) 

#converting the map from a raster object to a ggplot object so that it can be plotted on
map_of_Chicago <- ggmap(map_of_Chicago, extent="device", legend = "none")



Chicago_accidents_map <- map_of_Chicago + stat_density2d(
  aes(x = LONGITUDE, y = LATITUDE, fill = ..level.., alpha = ..level..),
  size = 0.001, bins = 30, data = accidents,
  geom = "polygon") + #scale_fill_distiller(palette= "Spectral", direction=-1) +
  ggtitle("Accident Density Map Chicago")

print(Chicago_accidents_map)
#Note that accidents are concentrated downtown... 

#map with "most dangerous" intersections shown in red
most_dangerous_map <- map_of_Chicago + geom_point(data = dangerous_intersecions, 
                                                  mapping = aes(x = longitudes, y = latitudes), 
                                                  color = "red") + ggtitle("'Most Dangerous' Intersections in Chicago")
print(most_dangerous_map)
#Note that most dangerous intersections involve interstates/expressways

#MAP for devon leigh caldwell central intersection
devon_leigh_caldwell_central_map <-  get_map(location = c(dangerous_intersecions[1,2], dangerous_intersecions[1,3]), 
                                             zoom = 16, maptype = "roadmap" , scale = 1) 
devon_leigh_caldwell_central_map <- ggmap(devon_leigh_caldwell_central_map, 
                                          extent="device", legend="none")

devon_leigh_caldwell_central_injuries <- devon_leigh_caldwell_central_map + 
                                        geom_point(data = accidents, mapping = aes(x = LONGITUDE, y = LATITUDE, 
                                                                                   color = MOST_SEVERE_INJURY), 
                                                   position = "jitter") + 
        ggtitle("Accidents at the intersection ofDevon, Leigh, Caldwell, and Central ")
print(devon_leigh_caldwell_central_injuries)


#map for halsted lincoln fullerton
halsted_lincoln_fullerton_map <-  get_map(location = c(dangerous_intersecions[2,2], dangerous_intersecions[2,3]), 
                                             zoom = 16, maptype = "roadmap" , scale = 1) 
halsted_lincoln_fullerton_map <- ggmap(halsted_lincoln_fullerton_map, 
                                          extent="device", legend="none")

halsted_lincoln_fullerton_injuries <- halsted_lincoln_fullerton_map + 
  geom_point(data = accidents, mapping = aes(x = LONGITUDE, y = LATITUDE, 
                                             color = MOST_SEVERE_INJURY), position = "jitter") +
            ggtitle("Accidents at the intersection of Halsted, Lincoln, and Fullerton")
print(halsted_lincoln_fullerton_injuries)

#map for beverly vincennes 103rd
beverly_vincennes_103rd_map <-  get_map(location = c(dangerous_intersecions[3,2], dangerous_intersecions[3,3]), 
                                          zoom = 17, maptype = "roadmap" , scale = 1) 
beverly_vincennes_103rd_map <- ggmap(beverly_vincennes_103rd_map, 
                                       extent="device", legend="none")
beverly_vincennes_103rd_injuries <- beverly_vincennes_103rd_map + 
  geom_point(data = accidents, mapping = aes(x = LONGITUDE, y = LATITUDE, 
                                             color = MOST_SEVERE_INJURY), position = "jitter") +
  ggtitle("Accidents at the intersection of Beverly, Vincennes, and 103rd")

print(beverly_vincennes_103rd_injuries)


#map for clark_ridge_thorndale intersection
clark_ridge_thorndale_map <-  get_map(location = c(dangerous_intersecions[4,2], dangerous_intersecions[4,3]), 
                                        zoom = 17, maptype = "roadmap" , scale = 1) 
clark_ridge_thorndale_map <- ggmap(clark_ridge_thorndale_map, 
                                     extent="device", legend="none")
clark_ridge_thorndale_injuries <- clark_ridge_thorndale_map + 
  geom_point(data = accidents, mapping = aes(x = LONGITUDE, y = LATITUDE, 
                                             color = MOST_SEVERE_INJURY), position = "jitter") +
  ggtitle("Accidents at the intersection of Halsted, Clark, Ridge and Thorndale")

print(clark_ridge_thorndale_injuries)


#map for lasalle_clark_eugenie intersection
lasalle_clark_eugenie_map <-  get_map(location = c(dangerous_intersecions[5,2], dangerous_intersecions[5,3]), 
                                      zoom = 17, maptype = "roadmap" , scale = 1) 
lasalle_clark_eugenie_map <- ggmap(lasalle_clark_eugenie_map, 
                                   extent="device", legend="none")
lasalle_clark_eugenie_injuries <- lasalle_clark_eugenie_map + 
  geom_point(data = accidents, mapping = aes(x = LONGITUDE, y = LATITUDE, 
                                             color = MOST_SEVERE_INJURY), position = "jitter") +
  ggtitle("Accidents at the intersection of LaSalle, Clark, and Eugenie")

print(lasalle_clark_eugenie_injuries)

#map for north_damen_milwaukee intersection
north_damen_milwaukee_map <-  get_map(location = c(dangerous_intersecions[6,2], dangerous_intersecions[6,3]), 
                                      zoom = 16, maptype = "roadmap" , scale = 1) 
north_damen_milwaukee_map <- ggmap(north_damen_milwaukee_map, 
                                   extent="device", legend="none")
north_damen_milwaukee_injuries <- north_damen_milwaukee_map + 
  geom_point(data = accidents, mapping = aes(x = LONGITUDE, y = LATITUDE, 
                                             color = MOST_SEVERE_INJURY), position = "jitter") +
  ggtitle("Accidents at the intersection of North, Damen, and Milwaukee")

print(north_damen_milwaukee_injuries)


#map for ewing_95th_aveL intersection
ewing_95th_aveL_map <-  get_map(location = c(dangerous_intersecions[7,2], dangerous_intersecions[7,3]), 
                                      zoom = 15, maptype = "roadmap" , scale = 1) 
ewing_95th_aveL_map <- ggmap(ewing_95th_aveL_map, 
                                   extent="device", legend="none")
ewing_95th_aveL_injuries <- ewing_95th_aveL_map + 
  geom_point(data = accidents, mapping = aes(x = LONGITUDE, y = LATITUDE, 
                                             color = MOST_SEVERE_INJURY), position = "jitter") +
  ggtitle("Accidents at the intersection of Ewing, 95th, and Avenue L")

print(north_damen_milwaukee_injuries)

#map for ashland_madison_ogden intersection
ashland_madison_ogden_map <-  get_map(location = c(dangerous_intersecions[8,2], dangerous_intersecions[8,3]), 
                                zoom = 16, maptype = "roadmap" , scale = 1) 
ashland_madison_ogden_map <- ggmap(ashland_madison_ogden_map, 
                             extent="device", legend="none")
ashland_madison_ogden_injuries <- ashland_madison_ogden_map + 
  geom_point(data = accidents, mapping = aes(x = LONGITUDE, y = LATITUDE, 
                                             color = MOST_SEVERE_INJURY), position = "jitter") +
  ggtitle("Accidents at the intersection of Ashland, Madison, and Ogden")

print(ashland_madison_ogden_injuries)


#map for archer_cermak_princeton intersection
archer_cermak_princeton_map <-  get_map(location = c(dangerous_intersecions[9,2], dangerous_intersecions[9,3]), 
                                      zoom = 17, maptype = "roadmap" , scale = 1) 
 
archer_cermak_princeton_map <- ggmap(archer_cermak_princeton_map, 
                                   extent="device", legend="none")
archer_cermak_princeton_injuries <- archer_cermak_princeton_map + 
  geom_point(data = accidents, mapping = aes(x = LONGITUDE, y = LATITUDE, 
                                             color = MOST_SEVERE_INJURY), position = "jitter") +
  ggtitle("Accidents at the intersection of Archer, Cermak, and Princeton")

print(archer_cermak_princeton_injuries)

#map for belmont_lincoln_ashland intersection
belmont_lincoln_ashland_map <-  get_map(location = c(dangerous_intersecions[10,2], dangerous_intersecions[10,3]), 
                                        zoom = 17, maptype = "roadmap" , scale = 1) 

belmont_lincoln_ashland_map <- ggmap(belmont_lincoln_ashland_map, 
                                     extent="device", legend="none")
belmont_lincoln_ashland_injuries <- belmont_lincoln_ashland_map + 
  geom_point(data = accidents, mapping = aes(x = LONGITUDE, y = LATITUDE, 
                                             color = MOST_SEVERE_INJURY), position = "jitter") +
  ggtitle("Accidents at the intersection of Belmont, Lincoln, and Ashland")

print(belmont_lincoln_ashland_injuries)


#map for central_foster_milwaukee_nwhwy intersection
central_foster_milwaukee_nwhwy_map <-  get_map(location = c(dangerous_intersecions[11,2], dangerous_intersecions[11,3]), 
                                        zoom = 17, maptype = "roadmap" , scale = 1) 

central_foster_milwaukee_nwhwy_map <- ggmap(central_foster_milwaukee_nwhwy_map, 
                                     extent="device", legend="none")
central_foster_milwaukee_nwhwy_injuries <- central_foster_milwaukee_nwhwy_map + 
  geom_point(data = accidents, mapping = aes(x = LONGITUDE, y = LATITUDE, 
                                             color = MOST_SEVERE_INJURY), position = "jitter") +
  ggtitle("Accidents at the intersection of Central, Foster, and Milwaukee")

print(central_foster_milwaukee_nwhwy_injuries)


#map for columbus_87th_pulaski_swhwy intersection
columbus_87th_pulaski_swhwy_map <-  get_map(location = c(dangerous_intersecions[12,2], dangerous_intersecions[12,3]), 
                                               zoom = 17, maptype = "roadmap" , scale = 1) 

columbus_87th_pulaski_swhwy_map <- ggmap(columbus_87th_pulaski_swhwy_map, 
                                            extent="device", legend="none")
columbus_87th_pulaski_swhwy_injuries <- columbus_87th_pulaski_swhwy_map + 
  geom_point(data = accidents, mapping = aes(x = LONGITUDE, y = LATITUDE, 
                                             color = MOST_SEVERE_INJURY), position = "jitter") +   
      ggtitle("Accidents at the intersection of Colombus, 87th, and Pulaski")

print(columbus_87th_pulaski_swhwy_injuries)

fatal_accidents <- accidents %>% filter(MOST_SEVERE_INJURY == "FATAL")

fatal_accident_map <- map_of_Chicago + geom_point(data = fatal_accidents, 
                                                 mapping = aes(x = LONGITUDE, y = LATITUDE),
                                                 color = "red")
print(fatal_accident_map)
#Note that most of the fatal accidents occured near/along exressways
#Also notice the cluster of fatal accidents in the Garfield Park neighborhood. 
#I would like to explore this further...

#Loading in the map of Garfield Park using google maps
garfield_park_map <- get_map(location = c(-87.7391678, 41.878296), 
                             zoom = 14, maptype = "roadmap" , scale = 1)

garfield_park_map <- ggmap(garfield_park_map, extent="device", legend="none")

fatal_accident_garfield_park <- garfield_park_map + geom_point(data = fatal_accidents, 
                                                               mapping = aes(x = LONGITUDE, y = LATITUDE),
                                                               color = "red") +
  labs(Title = "Map of Fatal Accidents in the Garfield Park Neighborhood")
print(fatal_accident_garfield_park)

#I want to look at the details of these 5 fatal accidents and see if they have anything in
#common. Perhaps they all occured in a construction zone or maybe conditions were poor...
#Perhaps they have the cause of crash in common



