# Creating a map of Manchester good house buying locations based on different data sources
# Authors: Serena Abbott and Joshua Crew
# February 2020

# Packages ----------------------------------------------------------------

library("leaflet")
library("dplyr")
library("rgdal")
library("sp")

# Read in data ------------------------------------------------------------
setwd("data")

# postcode converter https://www.freemaptools.com/convert-uk-postcode-to-lat-lng.htm

# supermarkets
aldi <- read.csv("aldi_locations.csv", stringsAsFactors = FALSE)
asda <- read.csv("asda_locations.csv", stringsAsFactors = FALSE)
tesco <- read.csv("tesco_locations.csv", stringsAsFactors = FALSE)
lidl <- read.csv("lidl_locations.csv", stringsAsFactors = FALSE)
holland_barrett <- read.csv("holland_barrett_locations.csv", stringsAsFactors = FALSE)
marks_and_spencer <- read.csv("m_and_s_locations.csv", stringsAsFactors = FALSE)
supermarket_list <- list(Aldi = aldi, Asda = asda, Tesco = tesco, Lidl = lidl, "Holland and Barrett" = holland_barrett, "Marks and Spencer" = marks_and_spencer)
supermarkets <- bind_rows(supermarket_list, .id = "supermarket")

# gyms
puregym <- read.csv("pure_gym_locations.csv", stringsAsFactors = FALSE)
puregym[, "X"] <- "Pure Gym"

# transport
transport <- read.csv("metro_rail_stops.csv", stringsAsFactors = FALSE)
tram <- subset(transport, transport$NETTYP=="M")
train <- subset(transport, transport$NETTYP=="R")
tram[tram$NETTYP=="M", "NETTYP"] <- "Tram"
train[train$NETTYP=="R", "NETTYP"] <- "Train"

# Leaflet map -------------------------------------------------------------

leaflet() %>%
  addProviderTiles(providers$OpenStreetMap) %>%
  setView(-2.2426, 53.4808, zoom = 11) %>%
  # supermarkets
  addAwesomeMarkers(lat = supermarkets$Lat, 
                    lng = supermarkets$Long, 
                    group = supermarkets$supermarket,
                    icon = awesomeIcons(icon ="shopping-cart",
                                        library = "fa")
                    ) %>%
  # gyms
  addAwesomeMarkers(lat = puregym$Lat,
                    lng = puregym$Long,
                    group = puregym$X,
                    icon = awesomeIcons(icon = "heartbeat",
                                        library = "fa")
                    ) %>%
  # transport
  addAwesomeMarkers(lat = tram$GPSLAT,
                    lng = tram$GPSLON,
                    group = tram$NETTYP,
                    icon = awesomeIcons(icon = "subway",
                                        library = "fa")
                    ) %>%
  addAwesomeMarkers(lat = train$GPSLAT,
                    lng = train$GPSLON,
                    group = train$NETTYP,
                    icon = awesomeIcons(icon = "train",
                                        library = "fa")
                    
  ) %>%
  addLayersControl(
    overlayGroups = c(supermarkets$supermarket, puregym$X, tram$NETTYP, train$NETTYP),  # add these layers
    options = layersControlOptions(collapsed = FALSE)  # expand on hover?
)

