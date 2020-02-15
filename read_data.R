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
supermarket_list <- list(Aldi = aldi, Asda = asda, Tesco = tesco, Lidl = lidl, "Holland and Barrett" = holland_barrett)
supermarkets <- bind_rows(supermarket_list, .id = "supermarket")


# Leaflet map -------------------------------------------------------------

leaflet() %>%
  addProviderTiles(providers$OpenStreetMap) %>%
  setView(-2.2426, 53.4808, zoom = 11) %>%
  addMarkers(
    lat = supermarkets$Lat, lng = supermarkets$Long, group = supermarkets$supermarket) %>%
  addLayersControl(
    overlayGroups = supermarkets$supermarket,  # add these layers
    options = layersControlOptions(collapsed = FALSE)  # expand on hover?
)

