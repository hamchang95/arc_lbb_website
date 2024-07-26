########## SET UP ##########
#--Clear enviornment
rm(list = ls())

#--Options
options(timeout = 120)
API_KEY <- "AIzaSyCjxhelYNRXnXvyzdg9q79XIXiXMuqWB9A"
Sys.setenv(GOOGLEGEOCODE_API_KEY = API_KEY)

#--Install / load packages

pacman::p_load(sf, httr, jsonlite, here, tmap, osmdata, tidyverse, utf8, data.table, rmapshaper, xml2, rvest, rio, tidygeocoder, httpgd, languageserver,leaflet)

#--Import data
cctv_safety_raw <- rio::import(here("1_data", "cctv_list_1223.csv")) #cctvs installed for community safety
cctv_traffic_raw <- rio::import(here("1_data", "cctv_traffic_list.csv")) #cctvs on roads for traffic control

#--Tidy names
names(cctv_safety_raw) <- str_to_lower(names(cctv_safety_raw))
names(cctv_traffic_raw) <- str_to_lower(str_replace_all(names(cctv_traffic_raw), " ", "_"))

####### PROCESS #########
#--Inspect 
str(cctv_safety_raw)
    #location includes street name without postcode 

str(cctv_traffic_raw)
    #location_description includes street name and outcode

#--Get bounding box of Barnet
bb_lbb <- osmdata::getbb("London Barnet") 

#--Tidy up & geocode
#---Using ArcGIS geocoder
cctv_safety <- cctv_safety_raw |> 
    mutate(across(where(is.character), ~utf8::utf8_encode(.x))) |>
    mutate(location = paste0(location, " , Barnet, London, United Kingdom")) |>
    #mutate(location = gsub("\\s*\\([^\\)]+\\)", "", location)) |>
    #mutate(location = str_replace_all(location, fixed("Ave"), "Avenue")) |>
    #mutate(cleaned_location =  gsub("(.+?\\b(?:\\S+\\s+){0,2})(street|road|way|lane|avenue)\\b.*", "\\1\\2", location, ignore.case = TRUE)) |>
    #mutate(cleaned_location = ifelse(grepl("Northway Circus", cleaned_location), "Watford Way", cleaned_location)) |>
    tidygeocoder::geocode(
        address = location, 
        method = "google"
    ) # Converts street into lat & long  

cctv_safety_sf <- st_as_sf(drop_na(cctv_safety), coords = c("long", "lat"), crs = 4326)


leaflet::leaflet(data = cctv_safety_sf)|>
    leaflet::addTiles()|>
    leaflet::addCircleMarkers()

cctv_traffic <- cctv_traffic_raw |> 
    mutate(across(where(is.character), ~utf8::utf8_encode(.x))) |>
    mutate(location_description = paste0(location_description, " , Barnet, London, United Kingdom")) |>
    #mutate(location = gsub("\\s*\\([^\\)]+\\)", "", location)) |>
    #mutate(location = str_replace_all(location, fixed("Ave"), "Avenue")) |>
    #mutate(cleaned_location =  gsub("(.+?\\b(?:\\S+\\s+){0,2})(street|road|way|lane|avenue)\\b.*", "\\1\\2", location, ignore.case = TRUE)) |>
    #mutate(cleaned_location = ifelse(grepl("Northway Circus", cleaned_location), "Watford Way", cleaned_location)) |>
    tidygeocoder::geocode(
        address = location_description, 
        method = "google"
    ) # Converts street into lat & long  


cctv_traffic_sf <- st_as_sf(drop_na(cctv_traffic), coords = c("long", "lat"), crs = 4326)


leaflet::leaflet(data = cctv_traffic_sf)|>
    leaflet::addTiles()|>
    leaflet::addCircleMarkers()