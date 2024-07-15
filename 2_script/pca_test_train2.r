## title: PCA-Incorporated Multi-Kriging Model
## author: Hannah Chang

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
# After validation, the number of components to keep has been updated to 4

#--Apply PCA again with the selected number of components
x_train_pca <- PCA(x_train[-c(1,2)], graph = FALSE, scale.unit = TRUE, ncp = 4)

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
names(train_data)[5] <- "n" 

#--Combine PCA-transformed predictors with the target variable for test data
test_data <- cbind(x_test_pred_df, n = y_test$n)
names(test_data)[5] <- "n"

#--Create variogram using PCA-transformed predictors
lzn.vgm <- variogram(
    log(n) ~ x + y + Dim.1 + Dim.2 + Dim.3 + Dim.4, 
    data = train_data, 
    width = 1)

#--Fit variogram model
lzn.fit <- fit.variogram(lzn.vgm, vgm(c("Gau", "Sph", "Mat", "Exp")), fit.kappa = TRUE)

#--Plot variogram model fit
plot(lzn.vgm, main = "Variogram Model Fit", cutoff = max(lzn.vgm$dist))
plot(lzn.fit, main = "Variogram Model Fit", cutoff = max(lzn.vgm$dist))

#--Kriging over the test set
lzn.kriged <- krige(
    log(n) ~ x + y + Dim.1 + Dim.2 + Dim.3 + Dim.4, 
    train_data, 
    test_data, 
    model = lzn.fit)

lzn.kriged.train <- krige(
    log(n) ~ x + y + Dim.1 + Dim.2 + Dim.3 + Dim.4, 
    train_data, 
    train_data, 
    model = lzn.fit)
#plot points in test & train

#--Plot
test_sf <- st_as_sf(test_data, coords = c("x", "y"), crs = 27700)
test_sf$krige_pred <- exp(lzn.kriged@data$var1.pred)
test_sf$variance <- exp(lzn.kriged@data$var1.var)

ggplot() +
  geom_sf(data = test_sf, aes(colour = krige_pred)) +
  scale_colour_gradient(low = "blue", high = "red", name = "Predicted Value") +
  ggtitle("Hotspot Map for Kriged Data") +
  theme_minimal()

train_sf <- st_as_sf(train_data, coords = c("x", "y"), crs = 27700)
train_sf$krige_pred <- exp(lzn.kriged.train@data$var1.pred)
train_sf$variance <- exp(lzn.kriged.train@data$var1.var)

#--Evaluation
rmse(test_sf$n, test_sf$krige_pred)
rmse(train_sf$n, train_sf$krige_pred)

str(train_data@data$n)
str(lzn.fit)
####### CREATE GRID #######
#--Import Barnet shapefile
bnt_shp <- sf::st_read(here("1_data", "9_geo", "bnt_lad.json"), crs = 4326) |> 
    st_make_valid()

#--Make grid
grid <- bnt_shp |> 
    st_make_grid(n = 100, what = "centers") |>
    st_as_sf() 

#--Get centroid of each grid cell
grid_centroid <- grid |> 
    st_centroid()

grid_cen_xy <- data.frame(grid_centroid) |> 
    mutate(x = st_coordinates(grid_centroid)[, 1],
           y = st_coordinates(grid_centroid)[, 2]) |>
    st_as_sf(coords = c("x", "y"), crs = 4326)

grid_cen_xy <- st_as_sf(grid_cen_xy, coords = c("x", "y"), crs = st_crs(bnt_shp))

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

#--Manage columns
grid_bnt_osgb <- grid_bnt |> 
    select(-row.id, -col.id) |> 
    st_transform(27700)

grid_bnt_osgb <- grid_bnt_osgb |>
    mutate(x = st_coordinates(grid_bnt_osgb)[, 1],
           y = st_coordinates(grid_bnt_osgb)[, 2])

#--Reproject POIs to same CRS 
# load workspace.multikrige.Rdata
poi_bnt_osgb <- lapply(poi_bnt_fin, function(x) st_transform(x, 27700))

#--Convert POI to matrix
poi_coords_list <- lapply(poi_bnt_osgb, st_coordinates)

#--Convert grid_bnt to df
grid_coords <- grid_bnt_osgb |>
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

#--Add distance columns to the grid_sf
grid_bnt_osgb_dist <- cbind(grid_bnt_osgb, min_distances_df)
names(grid_bnt_osgb_dist)[3:50] <- names(min_distances_df)

#--Convert sf to df
grid_bnt_df <- st_drop_geometry(grid_bnt_osgb_dist)

#--Predict PCA components for grid data
grid_pca_pred <- predict.PCA(x_train_pca, newdata = grid_bnt_df[,-c(1:2)])

#--Add PCA components to grid data
grid_pca_df <- as.data.frame(grid_pca_pred$coord)
grid_data <- cbind(grid_bnt_df, grid_pca_df)

#--Convert grid data to SpatialPointsDataFrame
coordinates(grid_data) <- ~ x + y
proj4string(grid_data) <- CRS("+init=epsg:27700")

######## KRIGING OVER GRID #######
#--Kriging over the grid
bnt.kriged <- krige(
    log(n) ~ x + y + Dim.1 + Dim.2 + Dim.3 + Dim.4,
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

#--Plot over leaflet
grid_fin_wgs84 <- st_transform(grid_fin, 4326) 

grid_fin_wgs84 <- grid_fin_wgs84 |>
    mutate(x = st_coordinates(grid_fin_wgs84)[, 1],
           y = st_coordinates(grid_fin_wgs84)[, 2])

pal <- colorNumeric(palette = c("green", "red"), domain = grid_fin_wgs84$krige_pred)

leaflet(grid_fin_wgs84) |> 
    leaflet::addTiles() |>
    leaflet::addCircles(color = ~pal(grid_fin_wgs84$krige_pred), 
    opacity = 0.6)
