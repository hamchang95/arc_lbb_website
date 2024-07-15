######### SETTING ##########
#--Install / load packages
pacman::p_load(sp, sf, data.table, rio, here, leaflet, gstat, tidyverse, Metrics,
 scales, corrr, ggcorrplot, FactoMineR, factoextra, corrplot)

#--Import street-level asb data
asb <- import(here("3_output", "asb_with_nearest_distances.csv"))

#--Calculate count of crimes per location coordinate
asb_count <- asb |> 
    group_by(location.latitude, location.longitude) |>
    count() |> 
    ungroup() |> 
    inner_join(asb, by = c('location.latitude', 'location.longitude')) |> 
    distinct(location.latitude, location.longitude, .keep_all = TRUE) |>
    group_by(location.latitude, location.longitude) |>
    mutate(location_id = cur_group_id()) |>
    ungroup()

names(asb_count)[grepl('longitude', names(asb_count))] <- 'x' 
names(asb_count)[grepl('latitude', names(asb_count))] <- 'y' 

#--Get sf version and reproject to OSGB36
asb_count_sf <- asb_count |> 
    st_as_sf(coords = c('x', 'y'), crs = 4326) |>
    st_transform(27700) 

asb_count_sf <- asb_count_sf |>
    mutate(x = st_coordinates(asb_count_sf)[, 1],
           y = st_coordinates(asb_count_sf)[, 2])

#--Change the sf back to df
asb_count <- st_drop_geometry(asb_count_sf)

#--Get only numerical version
asb_x <- asb_count |> select(x, y, starts_with('d'))
asb_y <- asb_count |> select(x, y, n)

####### SPLIT ######
#--Create random indices
total_rows <- nrow(asb_count)
sample_size <- round(total_rows * 0.75)

set.seed(1234)
random_indices <- sample(1:total_rows, sample_size, replace = FALSE)

#--Create test sets using the random indices
x_train <- asb_x[dimnames(asb_x)[[1]] %in% random_indices,] |>
    mutate(across(where(is.numeric), ~round(.x, 2)))

y_train <- asb_y[dimnames(asb_y)[[1]] %in% random_indices,]|>
    mutate(across(where(is.numeric), ~round(.x, 2)))

#--Create training sets by excluding the indices used for the test set
x_test <- asb_x[!dimnames(asb_x)[[1]] %in% random_indices,]|>
    mutate(across(where(is.numeric), ~round(.x, 2)))

y_test <- asb_y[!dimnames(asb_y)[[1]] %in% random_indices,] |>
    mutate(across(where(is.numeric), ~round(.x, 2)))

###### PCA ######
#--Apply PCA
x_train_pca <- PCA(x_train[-c(1,2)], graph = FALSE, scale.unit = TRUE)

#--Check cumulative percentage of variance
x_train_pca$eig
# Comp 1-7 explains around 70% of total variance
# Hence, 7 is the number of components to keep

#--Apply PCA again with the selected number of components
x_train_pca <- PCA(x_train[-c(1,2)], graph = FALSE, scale.unit = TRUE, ncp = 7)

#--Predict 
x_train_pred <- FactoMineR::predict.PCA(x_train_pca, x_train[-c(1,2)])
x_test_pred <- FactoMineR::predict.PCA(x_train_pca, x_test[-c(1,2)])

#--Extract PCA-transformed data
x_train_pred_df <- as.data.frame(x_train_pred$coord)
x_test_pred_df <- as.data.frame(x_test_pred$coord)

#--Add coordinates to the PCA-transformed data
x_train_pred_df <- cbind(x_train_pred_df, x = x_train$x, y = x_train$y)
x_test_pred_df <- cbind(x_test_pred_df, x = x_test$x, y = x_test$y)

#--Convert to spatial data frames
coordinates(x_train_pred_df) <- ~ x + y
coordinates(x_test_pred_df) <- ~ x + y
proj4string(x_train_pred_df) <- CRS("+init=epsg:27700")
proj4string(x_test_pred_df) <- CRS("+init=epsg:27700")

###### KRIGING ######
#--Combine PCA-transformed predictors with the target variable for training data
train_data <- cbind(x_train_pred_df, n = y_train$n)
names(train_data)[8] <- "n" 

#--Combine PCA-transformed predictors with the target variable for test data
test_data <- cbind(x_test_pred_df, n = y_test$n)
names(test_data)[8] <- "n"

#--Create variogram using PCA-transformed predictors
lzn.vgm <- variogram(
    log(n) ~ x + y + Dim.1 + Dim.2 + Dim.3 + Dim.4 + Dim.5 + Dim.6 + Dim.7, 
    data = train_data, 
    width = 1)

#--Fit variogram model
lzn.fit <- fit.variogram(lzn.vgm, vgm(c("Gau", "Sph", "Mat", "Exp")), fit.kappa = TRUE)

#--Plot variogram model fit
plot(lzn.vgm, main = "Variogram Model Fit", cutoff = max(lzn.vgm$dist))
plot(lzn.fit, main = "Variogram Model Fit", cutoff = max(lzn.vgm$dist))

#--Kriging over the test set
lzn.kriged <- krige(
    log(n) ~ x + y + Dim.1 + Dim.2 + Dim.3 + Dim.4 + Dim.5 + Dim.6 + Dim.7, 
    train_data, 
    test_data, 
    model = lzn.fit)

#--Plot
test_sf <- st_as_sf(test_data, coords = c("x", "y"), crs = 27700)
test_sf$krige_pred <- exp(lzn.kriged@data$var1.pred)
test_sf$variance <- exp(lzn.kriged@data$var1.var)

ggplot() +
  geom_sf(data = test_sf, aes(colour = krige_pred)) +
  scale_colour_gradient(low = "blue", high = "red", name = "Predicted Value") +
  ggtitle("Hotspot Map for Kriged Data") +
  theme_minimal()

rmse(test_sf$n, test_sf$krige_pred)  

####### CREATE GRID #######
#--Import Barnet shapefile
bnt_shp <- sf::st_read(here("1_data", "9_geo", "bnt_lad.json"), crs = 4326)

#--
#--Convert sf object to SpatialPolygonsDataFrame
#spd <- sf::as_Spatial(st_geometry(bnt_shp), IDs = as.character(1:nrow(bnt_shp)))

#--Extract the data from the shapefile
#spd_data <- bnt_shp

#--Remove geometry column from the data frame
#spd_data$geometry <- NULL

#--Convert to a regular data frame
#spd_data <- as.data.frame(spd_data)

#--Combine spatial data with attribute data
#spd <- sp::SpatialPolygonsDataFrame(spd, data = spd_data)

#--Get the bounding box of the polygon and expand it slightly
bbox <- bbox(spd)
cellsize <- 0.001
x_range <- seq(from = bbox[1,1] - cellsize, to = bbox[1,2] + cellsize, by = cellsize)
y_range <- seq(from = bbox[2,1] - cellsize, to = bbox[2,2] + cellsize, by = cellsize)

#--Create a dataframe of grid points
grid_points <- expand.grid(x = x_range, y = y_range)

#--Convert grid points to SpatialPoints
grid <- SpatialPoints(grid_points, proj4string = CRS("+init=epsg:4326"))

#--Filter grid points to include only those within the Barnet polygon
grid <- grid[spd, ]

#--Convert sp SpatialPoints to sf
grid_sf <- grid |> 
    st_as_sf()

grid_sf <- st_transform(grid_sf, 27700)

#--Get coordinates    
grid_sf <- grid_sf |> 
    mutate(x = st_coordinates(grid_sf)[, 1],
           y = st_coordinates(grid_sf)[, 2]) |>
    select(x, y)

#--Check
ggplot(data = grid_sf, aes(x, y)) +
    geom_point() +
    ggtitle("All the points in the grids within Barnet boundary") +
    theme_minimal()

#--Reproject POIs to same CRS
load("pre_pca_multi_krige.RData")
poi_bnt_osgb <- lapply(poi_bnt_fin, function(x) st_transform(x, 27700))

#--Convert POI to matrix
poi_coords_list <- lapply(poi_bnt_osgb, st_coordinates)

#--Convert grid_sf to matrix
grid_coords_list <- grid_sf |>
    st_drop_geometry()

#--Calculate distances 
poi_x <- lapply(poi_coords_list, function(x) x[, 1])  # Extract x-coordinates of POIs
poi_y <- lapply(poi_coords_list, function(x) x[, 2])  # Extract y-coordinates of POIs
grid_x <- grid_coords_list[, 1]  # Extract x-coordinates of grid points
grid_y <- grid_coords_list[, 2]  # Extract y-coordinates of grid points

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

#--Add distance columns to the grid_sf
grid_sf <- cbind(grid_sf, min_distances_df)
names(grid_sf)[3:50] <- names(min_distances_df)

#--Convert sf to df
grid_df <- st_drop_geometry(grid_sf)

#--Predict PCA components for grid data
grid_pca_pred <- predict.PCA(x_train_pca, newdata = grid_df[,-c(1:2)])

#--Add PCA components to grid data
grid_pca_df <- as.data.frame(grid_pca_pred$coord)
grid_data <- cbind(grid_df, grid_pca_df)

#--Convert grid data to SpatialPointsDataFrame
coordinates(grid_data) <- ~ x + y
proj4string(grid_data) <- CRS("+init=epsg:27700")

min_distances_df |> head()
######## KRIGING OVER GRID #######
#--Kriging over the grid
bnt.kriged <- krige(
    log(n) ~ x + y + Dim.1 + Dim.2 + Dim.3 + Dim.4 + Dim.5 + Dim.6 + Dim.7,
    train_data, 
    grid_data,
    model = lzn.fit)

#--Plot
grid_fin <- st_as_sf(grid_data, coords = c("x", "y")) |> 
    st_transform(27700)
grid_fin$krige_pred <- exp(bnt.kriged@data$var1.pred)
grid_fin$variance <- exp(bnt.kriged@data$var1.var)
grid_fin$x <- st_coordinates(grid_fin)[, 1]
grid_fin$y <- st_coordinates(grid_fin)[, 2]

ggplot(data = grid_fin, aes(x, y)) +
  geom_point(aes(colour = krige_pred)) +
  scale_colour_gradient(low = "green", high = "red", name = "Predicted Value") +
  ggtitle("Hotspot Map for Kriged Data") +
  theme_minimal()

grid_fin_wgs84 <- st_transform(grid_fin, 4326) 

grid_fin_wgs84 <- grid_fin_wgs84 |>
    mutate(x = st_coordinates(grid_fin_wgs84)[, 1],
           y = st_coordinates(grid_fin_wgs84)[, 2])
summary(grid_fin_wgs84$krige_pred)

pal <- colorNumeric(palette = c("green", "red"), domain = grid_fin_wgs84$krige_pred)

leaflet(grid_fin_wgs84) |> 
    leaflet::addTiles() |>
    leaflet::addCircles(color = ~pal(grid_fin_wgs84$krige_pred), opacity = 0.6)

