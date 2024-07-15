##### SETTING #####
pacman::p_load(sf, here, tmap, osmdata, tidyverse, data.table, rio,flextable, mapview, units, knitr, spdep, deldir, sp, rgeoda, GGally)

#--Import street-level crime data
crime <- rio::import(here::here("3_output", "crime_2024-05-09.csv")) |>
    dplyr::mutate(category = stringr::str_replace_all(category, "-", " ")) |>
    sf::st_as_sf(coords = c("location.longitude", "location.latitude"), crs = 4326, dim = "XY") 
    #from 2021-04 to 2024-03

#--Import Barnet shapefile
bnt_shp <- sf::st_read(here("1_data", "9_geo", "bnt_lad.json"), crs = 4326) |>
  st_make_valid()

#--Filter crime that intersects Barnet boundary
crime_bnt <- crime[which(st_intersects(bnt_shp, crime, sparse = FALSE)),]

#--Select necessary columns
crime_bnt <- crime_bnt |>
  dplyr::select(id, category, month)

#--Import places data 
source(here("2_script", "osm_process.r"))

#--Select necessary columns
poi_bnt_fin <- map(poi_bnt_fin, ~dplyr::select(.x, osm_id, geometry))

#### CREATE GRID ####
#--Need to reproject to OSGB36
bnt_shp <- st_transform(bnt_shp, 27700)

#--Function to make grids and select those intersecting Barnet boundary
make_bnt_grid <- function(m){
  #--Make grids of n x n m^2 
  grid <- st_make_grid(bnt_shp, cellsize = units::as_units(m, "m")) 

  #--Select grids only that intsersect the Barnet boundary
  index <- which(lengths(st_intersects(grid, bnt_shp))>0)
  grid <- grid[index]

  return(grid)
}

#--Apply
grid_list <- list()
meter_list <- seq(100, 400, by = 100)
meter_list[5] <-350

for (i in seq_along(meter_list)){
  grid_list[[i]] <- make_bnt_grid(meter_list[i])  
}

names(grid_list) <- meter_list

###### COUNT POINTS IN GRID #####
#--Pre-process grid objects
grid_sf_list <- lapply(grid_list, function(grid) {
  grid_sf <- grid |>
    st_as_sf() |>
    st_transform(4326) |>
    mutate(grid_id = row_number())
  return(grid_sf)
})

#--Define function for counting points within each grid cell
ct_pts <- function(grid_sf, pts){
  index <- st_within(pts, grid_sf) |> as.integer()
  print("indexed")

  ct <- data.frame(grid_id = index) |>
    group_by(grid_id) |>
    tally() |>
    arrange(desc(n))
  print("counted")
  
  joined_ct <- grid_sf |>
    left_join(ct) |>
    arrange(desc(n))|>
    mutate(n = ifelse(is.na(n), 0, n))
  print("joined")

  return(joined_ct)
}

#--Initialize result lists
ct_poi <- vector("list", length = length(grid_list))
ct_crime <- vector("list", length = length(grid_list))
ct_all <- vector("list", length = length(grid_list))

#--Count crime
for (i in seq_along(grid_list)){
  # Count crime points
  ct_crime[[i]] <- ct_pts(grid_sf_list[[i]], crime_bnt)
}

#--Count places
for (i in seq_along(grid_list)){
  # Loop over poi_bnt_fin
  for (j in seq_along(poi_bnt_fin)){
    # Count place points
    ct_poi[[i]][[j]] <- ct_pts(grid_sf_list[[i]], poi_bnt_fin[[j]])
    
    # Name the count of points with their names
    names(ct_poi[[i]][[j]])[2] <- names(poi_bnt_fin)[j]
  }
}

#--Join places and crime points for each grid
for (i in seq_along(grid_list)) {
  # Initialize the result for the current grid item
  ct_all[[i]] <- ct_crime[[i]]
  
  for (j in seq_along(poi_bnt_fin)) {
    # Join the points for the current poi_bnt_fin to the current ct_all item
    if (j != length(poi_bnt_fin)) {  # Skip the last item since it has been already joined with crime
      ct_all[[i]] <- left_join(ct_all[[i]], st_drop_geometry(ct_poi[[i]][[j]]))
    }
  }
}

#--Only select grid cells that are within Barnet boundary
for (i in seq_along(ct_all)){
  index <- which(lengths(st_intersects(ct_all[[i]], st_transform(bnt_shp, 4326)))>0)

  ct_all[[i]] <- ct_all[[i]][index,]
}

#--Check total counts for all places and crimes
colSums(st_drop_geometry(ct_all[[1]]))|> sort(TRUE)
    #amenity_parking, building_garage, amenity_bicycle_parking, park, building_garages, amenity_school, amenity_place_of_whorship, amenity_restaurant, amenity_cafe, shop_convenience

###### OUTPUT AREA ###
oa <- st_read(here("1_data", "9_geo", "bnt_oa.json"))
st_crs(oa) <- 4326
oa <- st_transform(oa, 27700)
oa <- st_make_valid(oa)
st_is_valid(oa)

test <- st_join(st_transform(crime_bnt, 27700), oa, join = st_within)

oa <- mutate(oa, row_id = 1:nrow(oa)) |>
  dplyr::select(row_id, OA21CD, n, geometry)

#--Define function for counting points within each grid cell
ct_pts_oa <- function(oa, pts){
  index <- st_within(pts, oa) |> unlist()
  print("indexed")

  ct <- data.frame(row_id = index) |>
    group_by(row_id) |>
    tally() |>
    arrange(desc(n))
  print("counted")
  
  joined_ct <- oa |>
    left_join(ct) |>
    arrange(desc(n))|>
    mutate(n = ifelse(is.na(n), 0, n))
  print("joined")

  return(joined_ct)
}

ct_crime_oa <- ct_pts_oa(oa = oa, pts = st_transform(crime_bnt, 27700))

ggplot() +
geom_sf(data = ct_crime_oa, aes(fill = n))

ct_poi_oa <- map(poi_bnt_fin, ~ct_pts_oa(pts = st_transform(.x, 27700), oa = oa))

for (j in seq_along(ct_poi_oa)){
  names(ct_poi_oa[[j]])[3] <- names(poi_bnt_fin)[j]
}

#--Join places and crime points for each grid
ct_all_oa <- ct_crime_oa

for (j in seq_along(poi_bnt_fin)) {
    if (j != length(poi_bnt_fin)) {  # Skip the last item since it has been already joined with crime
      ct_all_oa <- left_join(ct_all_oa, st_drop_geometry(ct_poi_oa[[j]]))
    }
}

