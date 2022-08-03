# disney map 
library(png)
library(ggplot2) 
library(ggmap)
library(gifski)
library(gganimate)
library(ggimage)

# set wd 
setwd("~/Documents/disney")

# which rides and which order?

names <- c("Splash Mountain",
           "Star Wars: Rise of the Resistance",
           "Pirates of the Carribean", 
           "Space Mountain", 
           "Big Thunder Railroad")
lat <- c(33.812275636641123,
         33.814165429777304, 
         33.81119700958086,
         33.81142006666125,
         33.81280511705337)
long <-c(-117.92209455013683,
         -117.92331763740978, 
         -117.91993805415557, 
         -117.91769614348789, 
         -117.92025173471825)
order <- c(1,2,3,4,5)


df <- data.frame(names, lat, long,order)


map <- ggmap(get_map(location = c(lon = -117.918976, lat = 33.812511),
                     zoom = 16, 
                     maptype = "satellite", 
                     source = c("google"),
))

ggm <- map + 
  geom_point(aes(x = long, y = lat, col = "orange"), data = df, alpha=1, size = 5) + 
  transition_states(order) + 
  ggtitle("Disneyland Trip\n Ride #{closest_state}: {df$names[as.integer(closest_state)]}") +
  shadow_wake(wake_length = 0.2) + 
  theme(legend.position="none")

animate(ggm)

anim_save("disneyland-ride-plan-satellite.gif", ggm)

