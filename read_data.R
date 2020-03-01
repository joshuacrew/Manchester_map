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
# install.packages("formatR")
# library("formatR")

# Read in data ------------------------------------------------------------
setwd("C:\\Source\\Manchester_map\\data")

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

# gyms/sports facilities
puregym <- read.csv("pure_gym_locations.csv", stringsAsFactors = FALSE)
puregym[, "X"] <- "Pure Gym"
sites <- read.csv("Sites.csv", stringsAsFactors = FALSE)
facilities <- read.csv("Facilities.csv", stringsAsFactors = FALSE)
facility_type <- read.csv("FacilityType.csv", stringsAsFactors = FALSE)
geographic <- read.csv("Geographic.csv", stringsAsFactors = FALSE)
# select certain rows and columns
facility_type <- facility_type[facility_type$ï..FacTypeID %in% c(2, 6, 7, 8, 10, 12), c("ï..FacTypeID", "FacTypeDescription")]
facilities <- facilities[facilities$ClosureReason %in% "", c("ï..FACILITYID","SiteID","FacTypeID")]
geographic <- geographic[geographic$LDP.Name == "Greater Manchester" , c("ï..SiteID","FACILITYID","Latitude", "Longitude")]
sites <- sites[, c("ï..SiteID", "SiteName")]
# merge datasets
merge1 <- merge(facility_type, facilities, by.x = "ï..FacTypeID", by.y = "FacTypeID")
merge2 <- merge(geographic, merge1, by.x = "FACILITYID", by.y = "ï..FACILITYID")
sports_sites <- merge(merge2, sites, by = "ï..SiteID")

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
postcode_lookup <- read.csv("TRIMMED_PCD_OA_LSOA_MSOA_LAD_NOV19_UK_LU.csv", stringsAsFactors = FALSE)

ppd_pc <- merge(ppd, postcode_lookup, by.x = "postcode", by.y = "pcds", all.x = TRUE)
ppd_lsoa <- ppd_pc[, c("price_paid", "lsoa11nm")]

ppd_lsoa$lsoa11nm <- str_trim(ppd_lsoa$lsoa11nm)
ppd_lsoa <- ppd_lsoa %>%
  group_by(lsoa11nm) %>%
  summarise(average = mean(price_paid, na.rm = TRUE))

imd$lsoa11cd <- as.character(imd$lsoa11cd)

lsoa_map <- merge(imd, ppd_lsoa, by = "lsoa11nm")
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
                                        library = "fa"),
                    popup = supermarkets$supermarket
                    ) %>%
  # gyms
  addAwesomeMarkers(lat = puregym$Lat,
                    lng = puregym$Long,
                    group = puregym$X,
                    icon = awesomeIcons(icon = "heartbeat",
                                        library = "fa"),
                    popup = puregym$X
                    ) %>%
  # transport
  addAwesomeMarkers(lat = tram$GPSLAT,
                    lng = tram$GPSLON,
                    group = tram$NETTYP,
                    icon = awesomeIcons(icon = "subway",
                                        library = "fa"),
                    popup = tram$RSTNAM
                    ) %>%
  addAwesomeMarkers(lat = train$GPSLAT,
                    lng = train$GPSLON,
                    group = train$NETTYP,
                    icon = awesomeIcons(icon = "train",
                                        library = "fa"),
                    popup = str_to_title(train$RSTNAM)
                    ) %>%
  addAwesomeMarkers(lat = sports_sites$Latitude,
                    lng = sports_sites$Longitude,
                    group = sports_sites$FacTypeDescription,
                    icon = awesomeIcons(icon ="heart",
                                        library = "fa"),
                    popup = paste("<b> Facility type: </b>", sports_sites$FacTypeDescription, "<br/>", "<b>Site name:</b>", str_to_title(sports_sites$SiteName))
                    ) %>%
  addPolygons(data = imd,
              group = "IMD",
              fillColor = ~pal(IMDDecil),
              fillOpacity = 0.5,
              color = "grey",
              weight = 0.5,
              label = ~lsoa11nm,
              popup = ~paste("<b> Index of Multiple Deprivation Decile: </b>", IMDDecil)) %>%
#  addLegend("bottomleft", pal = pal, values = imd$IMDDecil,
#            title = "Index of Multiple Deprivation Decile",
#            opacity = 1
#  )
  addPolygons(data = lsoa_map,
              group = "Average House Price",
              fillColor = ~pal2(average),
              fillOpacity = 0.5,
              color = "grey",
              weight = 0.5,
              label = ~lsoa11nm,
              popup = ~paste0("<b> Average house price:</b> £", average)) %>% #TEST
#  addLegend("bottomleft", pal = pal2, values = lsoa_map$average,
#            title = "Average house price",
#            labFormat = labelFormat(prefix = "£"),
#            opacity = 1
#  ) %>%
  addLayersControl(
    baseGroups = c("Average House Price", "IMD"),
    overlayGroups = c(supermarkets$supermarket, puregym$X, tram$NETTYP, train$NETTYP, sports_sites$FacTypeDescription),  # add these layers
    options = layersControlOptions(collapsed = FALSE) 
    # expand on hover?
) %>%
hideGroup(c(supermarkets$supermarket, puregym$X, tram$NETTYP, train$NETTYP, sports_sites$FacTypeDescription))
#,
#popup = paste("Facility type:", sports_sites$FacTypeDescription, "\n", "Site name:", sports_sites$SiteName)