########## SET UP ##########
#--Clear enviornment
#rm(list = ls())

#--Set longer timeout
options(timeout = 120)

#--Install / load packages
pacman::p_load(sf, magrittr, jsonlite, here, tmap, osmdata, tidyverse, data.table, rmapshaper, xml2, rvest, mapview, rio)

#--Get Barnet polygon
bnt_bb <- getbb("London Borough of Barnet", format_out = "polygon")

#--Get Barnet boudnary
    # For easy lookup on boundary level: https://osm-boundaries.com/Map
bnt_bdry <- opq(bnt_bb) |> 
    add_osm_feature(key = "admin_level",value = 8) |>
    osmdata_sf()

#--Get Barnet Polygon for filtering
bnt_poly <- bnt_bdry[["osm_multipolygons"]] |>
  filter(grepl("Barnet", name))

#bnt_grid <- st_make_grid(bnt_poly)

#--Check the output 
ggplot(bnt_bdry$osm_multipolygons) +
    geom_sf()
    # it looks alright but it also returns other contingent boroughs
    # this is probably there will be some points or lines on the intersection 
    # will first download attributes data and filter out later on

ggplot(bnt_poly) +
    geom_sf()

#ggplot() +
    #geom_sf(data = bnt_poly, fill = "grey") +
    #geom_sf(data = bnt_grid, fill = NA) +
    #theme_void()


#--Get list of features with values on OSM
dict <- import_list(here("0_ref", "places.xlsx")) 
#dict[["office"]] <- osmdata::available_tags("office") |>
#mutate(Description = NA, Use = NA)

#export(dict, here("0_ref", "places.xlsx"))

dict <- dict[-c(1,2)] #first two sheets are irrelevant
dict <-map(dict, 
    ~.x |>
        filter(Use == 1) |>
        dplyr::select(Key, Value)
)
names(dict)

########### DOWNLOAD ###########
#--Define downloading function
get_features <- function(key, value) {
    data <- opq(bnt_bb) |> 
        add_osm_feature(key = key, value = value) |> 
        osmdata_sf() |>
        unique_osmdata()

    null_test <- map(data, ~is.null(.x))

    not_null_cols <- null_test[null_test == FALSE] |> names()

    not_null_osm <- not_null_cols[grepl("osm", not_null_cols)]

    data <- data[names(data) %in% not_null_osm]

    return(data)
}

#--Amenity
amenity_list <- vector("list", length(dict[[1]][["Value"]]))
amenity_list <- map(seq_along(dict[[1]][["Value"]]), function(i) {
  get_features("amenity", dict[[1]][["Value"]][[i]])
})
names(amenity_list) <- dict[[1]][["Value"]]

#map(amenity_list, ~map(.x, ~st_geometry_type(.x)))


#--Public transport
public_transport_list <- vector("list", length(dict[[2]][["Value"]]))
public_transport_list <- map(seq_along(dict[[2]][["Value"]]), function(i) {
  get_features("public_transport", dict[[2]][["Value"]][[i]])
})

names(public_transport_list) <- dict[[2]][["Value"]]
#names(public_transport_list[["station"]][["osm_points"]])
#names(public_transport_list[["platform"]][["osm_points"]])

public_transport_list[["bus_stop"]][["osm_points"]] <- public_transport_list[["platform"]][["osm_points"]] |> filter(highway == "bus_stop")
public_transport_list[["station"]][["osm_points"]] %<>% filter(!is.na(network))
public_transport_list[["underground_station"]][["osm_points"]] <- filter(public_transport_list[["station"]][["osm_points"]], grepl("Underground", network))
public_transport_list[["rail_station"]][["osm_points"]] <- filter(public_transport_list[["station"]][["osm_points"]], grepl("Rail", network))
public_transport_list <- public_transport_list[!names(public_transport_list) %in% c("platform", "station")]

#leaflet(data = public_transport_list[["bus_stop"]][["osm_points"]]) |>
  #addTiles() |>
  #addCircleMarkers(radius = 1, popup = ~name)

#--Building
building_list <- vector("list", length(dict[[3]][["Value"]]))
building_list <- map(seq_along(dict[[3]][["Value"]]), function(i) {
  get_features("building", dict[[3]][["Value"]][[i]])
})
names(building_list) <- dict[[3]][["Value"]]

#leaflet(data = building_list[["church"]][["osm_centroid"]]) |>
  #addTiles() |>
  #addCircleMarkers(radius = 1, popup = ~name)

#for (i in seq_along(building_list)){
  #building_list[[i]][["osm_centroid"]] <- st_centroid(building_list[[i]][["osm_polygons"]]) 
#}

#--Shop
shop_list <- vector("list", length(dict[[4]][["Value"]]))
shop_list <- map(seq_along(dict[[4]][["Value"]]), function(i) {
  get_features("shop", dict[[4]][["Value"]][[i]])
})
names(shop_list) <- dict[[4]][["Value"]]

#--Leisure
leisure_list <- vector("list", length(dict[[5]][["Value"]]))
leisure_list <- map(seq_along(dict[[5]][["Value"]]), function(i) {
  get_features("leisure", dict[[5]][["Value"]][[i]])
})
names(leisure_list) <- dict[[5]][["Value"]]

#--Office
office_list <- vector("list", length(dict[[6]][["Value"]]))
office_list <- map(seq_along(dict[[6]][["Value"]]), function(i) {
  get_features("office", dict[[6]][["Value"]][[i]])
})
names(office_list) <- dict[[6]][["Value"]]

#--Park
park_list <- get_features("leisure", "park")


######## FILTER ########
place_list <- list(amenity_list, public_transport_list, building_list, shop_list, leisure_list, office_list)

for (j in seq_along(place_list)){
  for (i in seq_along(place_list[[j]])){
    place_list[[j]][[i]] <- ifelse(nrow(place_list[[j]][[i]][["osm_points"]]) > nrow(place_list[[j]][[i]][["osm_polygons"]]),
      place_list[[j]][[i]][names(place_list[[j]][[i]]) == "osm_points"],
      place_list[[j]][[i]][names(place_list[[j]][[i]]) == "osm_polygons"])
    
    names(place_list) <- names(dict)
      }
  }


place_list_fin <- map(place_list, ~map(.x, ~discard(.x, ~nrow(.x) == 0)))

rm(list = c('place_list', 'park_list', 'amenity_list', 'building_list', 'shop_list', 'office_list', 'public_transport_list', 'leisure_list', 'bnt_bb', 'bnt_poly', 'bnt_bdry'))

########## EXPORT ##########
#map(place_list_fin, ~export_list(.x, here("3_output", "place", "%s.csv")))

#export_list(amenity_pts, here("3_output", "place", paste0("amenity_", "%s.csv")))
#export_list(building_pts, here("3_output", "place", paste0("building_", "%s.csv")))
#export_list(pt_pts, here("3_output", "place", paste0("transport_", "%s.csv")))
#export_list(leisure_pts, here("3_output", "place", paste0("leisure_", "%s.csv")))
#export_list(shop_pts, here("3_output", "place", paste0("shop_", "%s.csv")))
#st_write(park_poly, here("3_output", "place", "park.shp"), delete_dsn = TRUE, delete_layer = TRUE)

