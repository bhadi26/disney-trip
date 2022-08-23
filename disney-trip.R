# disney map 

# imports
library(png)
library(ggplot2) 
library(ggmap)
library(gifski)
library(gganimate)
library(ggimage)
library(geosphere)
library(magrittr)
library(tidyverse)

register_google(key = "")

# set wd and seed
setwd("~/Documents/disney")
set.seed(42)

# get ride coordinate location and historical wait time
rides <- data.frame(read.csv('disney-ride-coords.csv'))
number_of_rides <- nrow(rides)

# Recommend an order! 

# first, pick a random order
# TODO: dynamic starting ride would be cool 
entrance_coords <- c(33.81020638886844, -117.91773920146908)
df <- rides[sample(number_of_rides, 12), ]
df$index <- 1:nrow(df)


# distance from entrace 
df$start_lon <- entrance_coords[2]
df$start_lat <- entrance_coords[1]

df2 <- df
df2$next_distance<-distGeo(df2[c("start_lon", "start_lat")], df2[c("long","lat")])
df2$next_lon <- 0000.00
df2$next_lat <- 0000.00

ride_order <- 0 

# distance to beat (choose next ride based on shortest distance)
for (i in 1:nrow(df)) {
  print(paste("Ride Number:", i))
  next_ride <- df2 %>%
                   top_n(-1, next_distance) 
  print(paste("Next ride is",next_ride$name))
  ride_order <- c(ride_order, next_ride$index)
  next_ride_lat <- next_ride$lat 
  next_ride_long <- next_ride$long
  df2 <- df2 %>% filter(!index %in% ride_order)
  if (nrow(df2)!=0){
      df2$next_lon <- next_ride_long
      df2$next_lat <- next_ride_lat
      df2$next_distance<-distGeo(df2[c("next_lon", "next_lat")], df2[c("long","lat")])
  } else{
    print("done!")
  }
}


index <- ride_order[2:number_of_rides]
index <- c(index,12)
ride_order_place <- (1:number_of_rides)
order_df <- data.frame(index, ride_order_place)
df <- left_join(df, order_df, by = 'index')

df <- df %>% arrange(ride_order_place)

euclidean <- function(a, b) sqrt(sum((a - b)^2))
# How would I add in mixing a combination of wait time and distance? 
# calc distance, but would need to standardize first 

#euclidean(point_a, point_b)

#### MAP SHIT ####
map <- ggmap(get_map(location = c(lon = -117.918976, lat = 33.812511),
                     zoom = 16, 
                     maptype = "satellite", 
                     source = c("google")
 ))

ggm <- map + 
  geom_point(aes(x = long, y = lat, col = "orange"), data = df, alpha=1, size = 5) + 
  transition_states(ride_order_place) + 
  ggtitle("Disneyland Trip\n Ride #{closest_state}: {df$name[as.integer(closest_state)]}") +
  shadow_wake(wake_length = 0.4) + 
  theme(legend.position="none")

animate(ggm)

anim_save("disneyland-ride-plan-satellite.gif", ggm)


# get wait time data 
# source https://www.thrill-data.com/waits/park/dlr/disneyland/#download


