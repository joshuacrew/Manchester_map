# Creating a map of Manchester good house buying locations based on different data sources
# Authors: Serena Abbott and Joshua Crew
# February 2020

# Packages ----------------------------------------------------------------

library("leaflet")
library("dplyr")
library("rgdal")
library("sp")
library("stringr")
library("ggmap")

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

# IMD
imd <- readOGR(".", "Lower_Super_Output_Area_LSOA_IMD2019__WGS84")
imd$IMDDecil <- as.numeric(imd$IMDDecil)
imd$IMDDecil <- ifelse(imd$IMDDecil >= 9, 9, imd$IMDDecil)
imd$IMDDecil <- as.factor(imd$IMDDecil)
imd$lsoa11nm <- as.character(imd$lsoa11nm)
imd$name <- str_detect(imd$lsoa11nm, "Rochdale|Oldham|Salford|Manchester|Bury|Trafford|Stockport|Tameside")
imd <- imd[imd$name == TRUE, ]

# colour palette
pal <- colorFactor("RdYlGn", NULL, n = 8)
pal2 <- colorFactor("RdYlGn", NULL, n = 8, reverse = TRUE)

# House price data
ppd <- read.csv("ppd_data.csv", stringsAsFactors = FALSE)
postcode_lookup <- read.csv("NSPL_NOV_2019_UK_M.csv", stringsAsFactors = FALSE)

ppd_pc <- merge(ppd, postcode_lookup, by.x = "postcode", by.y = "pcd", all.x = TRUE)
ppd_pc <- ppd_pc[, c("postcode", "lat", "long", "price_paid", "unique_id", "deed_date", "property_type", "new_build", "street", "locality", "town", "district", "lsoa11")]
ppd_lsoa <- ppd_pc[, c("price_paid", "lsoa11")]
ppd_lsoa <- ppd_lsoa %>%
  group_by(lsoa11) %>%
  summarise(average = mean(price_paid))
imd$lsoa11cd <- as.character(imd$lsoa11cd)
lsoa_map <- merge(imd, ppd_lsoa, by.x = "lsoa11cd", by.y = "lsoa11")
lsoa_map$average <- as.character(round(lsoa_map$average))

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
  addPolygons(data = imd,
              group = "IMD",
              fillColor = ~pal(IMDDecil),
              fillOpacity = 0.7,
              color = "grey",
              weight = 0.5,
              label = ~lsoa11nm) %>%
  addPolygons(data = lsoa_map,
              group = "Average House Price",
              fillColor = ~pal2(average),
              fillOpacity = 0.7,
              color = "grey",
              weight = 0.5,
              label = ~lsoa11nm,
              popup = ~paste0("Average house price: £", average)) %>%
  addLayersControl(
    baseGroups = c("Average House Price", "IMD"),
    overlayGroups = c(supermarkets$supermarket, puregym$X, tram$NETTYP, train$NETTYP),  # add these layers
    options = layersControlOptions(collapsed = FALSE)  # expand on hover?
)
  


