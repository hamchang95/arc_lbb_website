######### SETTING ##########
#--Install / load packages
pacman::p_load(sp, sf, data.table, rio, here, leaflet, gstat, tidyverse, Metrics,
 scales, corrr, ggcorrplot, FactoMineR, factoextra, corrplot)

#--Load POI list 
load("pre_pca_multi_krige.RData")

#--Import street-level crime data
crime <- import(here("3_output", "crime_2024-05-09.csv")) |>
    select(month, category, location.longitude, location.latitude)

#--Turn crime into sf object
crime_sf <- st_as_sf(crime, 
                     coords = c("location.longitude", "location.latitude"),
                     crs = 4326)

####### CREATE GRID #######
#--Import Barnet shapefile
bnt_shp <- sf::st_read(here("1_data", "9_geo", "bnt_lad.json"), crs = 4326) |> 
    st_make_valid()

#--Make grid
grid <- bnt_shp |> 
    st_make_grid(n = 100) |>
    st_as_sf() 

#--Filter grids within the Barnet polygon
result <- st_within(grid, bnt_shp) |>
    as.data.frame()

grid_bnt <- grid |> 
    mutate(row.id = 1:nrow(grid), .before = everything()) |> 
    left_join(result) |> 
    filter(!is.na(col.id)) |>
    select(-col.id)

#--Check
ggplot(grid_bnt) +
    geom_sf() +
    ggtitle("All the grids within Barnet boundary") +
    theme_minimal()

###### COUNT NUMBER OF CRIMES #####
#--Count number of crimes in Barnet grid
result_crime <- st_within(crime_sf, grid_bnt) |>
    data.frame()

crime_count <- crime_sf |> 
    mutate(row.id = 1:nrow(crime_sf), .before = everything()) |> 
    left_join(result_crime) |> 
    filter(!is.na(col.id)) |>
    group_by(col.id) |>
    tally() |>
    rename(row.id = col.id) |>
    st_drop_geometry()

asb_count <- crime_sf |> 
    filter(category == "anti-social-behaviour") |>
    mutate(row.id = 1:nrow(cur_data()), .before = everything()) |> 
    left_join(result_crime) |> 
    filter(!is.na(col.id)) |>
    group_by(col.id) |>
    tally() |>
    rename(row.id = col.id) |>
    st_drop_geometry()

vc_count <- crime_sf |> 
    filter(category == "violent-crime") |>
    mutate(row.id = 1:nrow(cur_data()), .before = everything()) |> 
    left_join(result_crime) |> 
    filter(!is.na(col.id)) |>
    group_by(col.id) |>
    tally() |>
    rename(row.id = col.id) |>
    st_drop_geometry()

#--Join the counts with grid_bnt
grid_by_n <- grid_bnt |>
    left_join(crime_count) |>
    mutate(n = ifelse(is.na(n), 0, n))

grid_by_n_asb <- grid_bnt |>
    left_join(asb_count) |>
    mutate(n = ifelse(is.na(n), 0, n))

grid_by_n_vc <- grid_bnt |>
    left_join(vc_count) |>
    mutate(n = ifelse(is.na(n), 0, n))

#--Check
ggplot(grid_by_n, aes(fill = n)) +
    geom_sf() +
    scale_fill_gradient2()+
    ggtitle("Crime Hot Spot") +
    theme_minimal()

ggplot(grid_by_n_asb, aes(fill = n)) +
    geom_sf() +
    scale_fill_gradient2()+
    ggtitle("ASB Hot Spot") +
    theme_minimal()

ggplot(grid_by_n_vc, aes(fill = n)) +
    geom_sf() +
    scale_fill_gradient2()+
    ggtitle("VC Hot Spot") +
    theme_minimal()

##### CALCULATE THE DISTANCE TO NEAREST POI ####
#--Reproject to OSGB36
poi_bnt_fin <- map(poi_bnt_fin, ~st_transform(.x, 27700))
grid_list <- list(grid_by_n, grid_by_n_asb, grid_by_n_vc)
grid_list <- map(grid_list, ~st_transform(.x, 27700))

#--Get X (long) and Y (lat)
poi_bnt_fin <- map(poi_bnt_fin, ~mutate(
    .x,
    longitude = st_coordinates(.x)[, 1],
    latitude = st_coordinates(.x)[, 2]
))

grid_list <- map(grid_list, ~mutate(
    .x,
    longitude = st_coordinates(st_centroid(.x))[, 1],
    latitude = st_coordinates(st_centroid(.x))[, 2]
))

# Extracting the longitude and latitude for each point in the grid and POI lists
poi_x <- map(poi_bnt_fin, ~select(st_drop_geometry(.x), longitude))
grid_x <- map(grid_list, ~select(st_drop_geometry(.x), longitude))
poi_y <- map(poi_bnt_fin, ~select(st_drop_geometry(.x), latitude))
grid_y <- map(grid_list, ~select(st_drop_geometry(.x), latitude))

# Initializing the list to store the minimum distances
min_distances_list <- vector("list", length(grid_list))

# Loop through each grid dataframe
for (i in seq_along(grid_list)) {
  grid_points <- grid_list[[i]]
  num_grid_points <- nrow(grid_points)
  num_poi_dfs <- length(poi_bnt_fin)
  
  # Initialize a dataframe to store minimum distances for this grid
  min_distances <- data.frame(matrix(NA, nrow = num_grid_points, ncol = num_poi_dfs))
  colnames(min_distances) <- names(poi_bnt_fin)
  
  # Loop through each point in the current grid
  for (j in seq_len(num_grid_points)) {
    grid_point_x <- grid_x[[i]]$longitude[j]
    grid_point_y <- grid_y[[i]]$latitude[j]
    
    # Loop through each POI dataframe
    for (k in seq_along(poi_bnt_fin)) {
      poi_points <- poi_bnt_fin[[k]]
      num_poi_points <- nrow(poi_points)
      
      # Initialize a vector to store distances from the current grid point to all POIs in the current dataframe
      distances <- numeric(num_poi_points)
      
      # Calculate distances from the current grid point to each POI point in the current POI dataframe
      for (m in seq_len(num_poi_points)) {
        poi_point_x <- poi_x[[k]]$longitude[m]
        poi_point_y <- poi_y[[k]]$latitude[m]
        distances[m] <- sqrt((grid_point_x - poi_point_x)^2 + (grid_point_y - poi_point_y)^2)
      }
      
      # Store the minimum distance to the current POI dataframe
      min_distances[j, k] <- min(distances)
      print(paste0("Min dist calucated for row ", j))
    }
  }
  
  # Store the minimum distances dataframe in the list
  min_distances_list[[i]] <- min_distances
}

grid_by_n_dist <- cbind(grid_by_n, min_distances_list[[1]])
grid_by_n_asb_dist <- cbind(grid_by_n_asb, min_distances_list[[2]])
grid_by_n_vc_dist <- cbind(grid_by_n_vc, min_distances_list[[3]])

