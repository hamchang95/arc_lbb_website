######### SET UP #########
rm(list = ls())
pacman::p_load(sf, dials,ranger, broom, kernlab, doParallel, workflowsets, rpart.plot, lubridate, here, rio, vip, ggplot2, readr, rsample, parsnip, recipes, workflows, tune, yardstick)

asb <- rio::import(here("3_output", "asb_with_nearest_distances.csv"))

# Basic transformation
asb <- asb |>
  rename(latitude = location.latitude, longitude = location.longitude) |>
  mutate(across(where(is.character), as.factor)) |>
  select(-contains("id"), -category)

asb_sf <- asb |>
    st_as_sf(coords = c("longitude", "latitude"), crs = 4326)

poi_bnt_fin <- readRDS("poi_bnt_fin.rds")

#--Import Barnet shapefile
bnt_shp <- sf::st_read(here("1_data", "9_geo", "bnt_lad.json"), crs = 4326) |> 
    st_make_valid()

#--Make grid
grid <- bnt_shp |> 
    st_make_grid(n = 10, what = "centers") |>
    st_as_sf() 

#--Get centroid of each grid cell
grid_centroid <- grid |> 
    st_centroid()

grid_cen_xy <- data.frame(grid_centroid) |> 
    mutate(x = st_coordinates(grid_centroid)[, 1],
           y = st_coordinates(grid_centroid)[, 2]) |>
    st_as_sf(coords = c("x", "y"), crs = 4326)

#--Filter grid points to include only those within the Barnet polygon
result <- st_within(grid_cen_xy, bnt_shp) |>
    as.data.frame()

grid_bnt <- grid_cen_xy |> 
    mutate(row.id = 1:nrow(grid_cen_xy)) |> 
    left_join(result) |> 
    filter(!is.na(col.id))

#--Check
ggplot(grid_bnt) +
    geom_sf() +
    ggtitle("All the points in the grids within Barnet boundary") +
    theme_minimal()

#--Test intersecting points with ASb
test_inter <- st_intersects(grid_bnt, asb_sf, sparse = FALSE)

#--Convert to a logical matrix and count non-intersecting points
non_intersecting_points <- grid_bnt[!rowSums(test_inter), ]
non_asb <- non_intersecting_points

ggplot() +
  geom_sf(data = non_intersecting_points, color = 'green', size = 0.1) +
  geom_sf(data = asb_sf, color = 'red', size = 0.1) +
  ggtitle("Grid Points within Barnet Boundary Excluding ASB Points") +
  theme_minimal()

#--Manage columns
non_asb_osgb <- non_asb |> 
    select(-row.id, -col.id) |> 
    st_transform(27700)

non_asb_osgb <- non_asb_osgb |>
    mutate(x = st_coordinates(non_asb_osgb)[, 1],
           y = st_coordinates(non_asb_osgb)[, 2])

#--Reproject POIs to same CRS 
# load workspace.multikrige.Rdata
poi_bnt_osgb <- lapply(poi_bnt_fin, function(x) st_transform(x, 27700))

#--Convert POI to matrix
poi_coords_list <- lapply(poi_bnt_osgb, st_coordinates)

#--Convert grid_bnt to df
grid_coords <- non_asb_osgb |>
    st_drop_geometry()

#--Calculate distances 
poi_x <- lapply(poi_coords_list, function(x) x[, 1])  # Extract x-coordinates of POIs
poi_y <- lapply(poi_coords_list, function(x) x[, 2])  # Extract y-coordinates of POIs
grid_x <- grid_coords[, 1]  # Extract x-coordinates of grid points
grid_y <- grid_coords[, 2]  # Extract y-coordinates of grid points

#--Initialize distances list with empty lists
min_distances <- matrix(nrow = length(grid_x), ncol = length(poi_x))  # Pre-allocate with NAs

#--Loop minimum distance calculation 
for (i in seq_along(poi_x)) {
  # Pre-allocate squared distances
  squared_distances <- matrix(nrow = length(grid_x), ncol = length(poi_x[[i]]))
  
  for (j in seq_along(poi_x[[i]])) {
    squared_distances[, j] <- sqrt((grid_x - poi_x[[i]][j])^2 + (grid_y - poi_y[[i]][j])^2)
  }
  
  # Calculate minimum distance for each grid point for current POI type 
  min_distances[, i] <- apply(squared_distances, 1, min)  # Store minimum for current POI type
}

#--Convert the min_distances matrix to a data frame for easier use
min_distances_df <- as.data.frame(min_distances)
names(min_distances_df) <- paste0("d_", names(poi_bnt_fin))

#--Add distance columns
non_asb_dist <- cbind(non_asb, min_distances_df) |>
  select(-c(row.id, col.id))

saveRDS(non_asb_dist, "non_asb_points.RDS")
