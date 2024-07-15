####### SET UP #######
#--Clear global environment
rm(list = ls())

#--Install / load packages
pacman::p_load(tidyverse, here, rio, data.table, tidygeocoder, mapview)

#--Import data
#---HMO
hmo_raw <- rio::import(here("1_data", "hmo_register_220124.csv"))
names(hmo_raw) <- str_to_lower(str_replace_all(names(hmo_raw), " ", "_"))

#---ONS Postcode Directory
#onspd <- rio::import(here("1_data", "9_geo", "onspd_bnt_0224.csv"))

#--Inspect
str(hmo_raw)
hmo_raw$hmo_address |> tail(10)

#str(onspd)
    # pcd: 7-character ver.
    # pcd2: 8-character ver.
    # pcds: variable length
    # postcodes include all live postcodes and terminated postcodes that haven't been re-used by Royal Mail

######## PROCESS ######
#--Parse postcode of HMO address
#---Separate address by "\r"
pcd_list <- str_split(hmo_raw$hmo_address, "\r")

#---Find elements starting with N, NW, EN or HA
filtered_pcd <- purrr::map(pcd_list, ~.x[grepl("^N|NW|EN|HA.*", .x)]) 

#---Filter out elements starting with North
filtered_pcd <- purrr::map(filtered_pcd, ~.x[!grepl("^North.*", .x)]) 

#---Add it to the tbl
fin_pcd <- filtered_pcd |> unlist()

hmo <- hmo_raw |> 
    mutate(pcd = fin_pcd, .after = hmo_address)

#--Find lat & long of postcodes
hmo_fin <- hmo |> 
 tidygeocoder::geocode(postalcode =  pcd)

#which(is.na(hmo_fin$lat))

#--Manually insert lat & long
hmo_ultimate <- hmo_fin |> 
    mutate(lat = ifelse(is.na(lat), 51.649909, lat)) |>
    mutate(long = ifelse(is.na(long), -0.174248, long)) |>
    mutate(across(c(lat, long), ~round(.x, 4)))

#--Check map
mapview(hmo_ultimate, xcol = "long", ycol = "lat", crs = 4269, grid = FALSE)

hmo_ultimate2 <- hmo_ultimate |>
    select(hmo_licence_reference, pcd, lat, long)

####### EXPORT #########
rio::export(hmo_ultimate2, here("3_output", "hmo.csv"))
