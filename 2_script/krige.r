######### SETTING ##########
#--Install / load packages
pacman::p_load(sp, data.table, sf, here, leaflet, gstat, tidyverse, Metrics)

#--Import street-level crime data
crime <- rio::import(here::here("3_output", "crime_2024-05-09.csv")) |>
    dplyr::mutate(category = stringr::str_replace_all(category, "-", " ")) 

#--Subset crime
asb <- subset(crime, category == "anti social behaviour")
vc <- subset(crime, category == "violent crime")
other <- subset(crime, category == "other theft")
vhc <- subset(crime, category == "vehicle crime")
tfp <- subset(crime, category == "theft from the person")
brg <- subset(crime, category == "burglary")

crime_list <- list(asb, vc, other, vhc, tfp, brg)
####### COUNT CRIME BY LOCATION ID #########
#--Calculate the frequency of ASB by location_id
asb_count <- asb |> 
    group_by(location.latitude, location.longitude) |>
    count() |> 
    ungroup() |> 
    inner_join(asb, by =c('location.latitude', 'location.longitude')) |> 
    distinct(location.latitude, location.longitude, .keep_all = TRUE) |>
    select(location.latitude, location.longitude, n) |>
    group_by(location.latitude, location.longitude) |>
    mutate(location_id = group_indices())

names(asb_count)[1:2] <- c("y", "x")

#--Create random indices
total_rows <- nrow(asb_count)
sample_size <- round(total_rows * 0.75)

set.seed(1234)
random_indices <- sample(1:total_rows, sample_size, replace = FALSE)

#--Create the test set using the random indices
train_asb <- asb_count |> filter(location_id %in% random_indices)

# Create the training set by excluding the indices used for the test set
test_asb <-  asb_count |> filter(!location_id %in% random_indices)

######## CREATE GRID ##########
#--Import Barnet shapefile
bnt_shp <- sf::st_read(here("1_data", "9_geo", "bnt_lad.json"), crs = 4326) 
bnt_shp <- st_transform(bnt_shp, 4326)

spd <- sf::as_Spatial(st_geometry(bnt_shp), IDs = as.character(1:nrow(bnt_shp)))

spd_data = bnt_shp
spd_data$geometry = NULL
spd_data <- as.data.frame(spd_data)
spd <- sp::SpatialPolygonsDataFrame(spd, data = spd_data)

# Get the bounding box of the polygon and expand it slightly
bbox <- bbox(spd)
cellsize = 0.001
x_range <- seq(from = bbox[1,1] - cellsize, to = bbox[1,2] + cellsize, by = cellsize)
y_range <- seq(from = bbox[2,1] - cellsize, to = bbox[2,2] + cellsize, by = cellsize)

# Create a data.frame of grid points
grid_points <- expand.grid(x = x_range, y = y_range)

# Convert grid points to SpatialPoints
grid <- SpatialPoints(grid_points, proj4string = CRS("+proj=longlat +datum=WGS84"))

# Step 4: Filter grid points to include only those within the Barnet polygon
grid <- grid[spd, ]

# Optionally, visualize the result to ensure it covers the entire polygon
plot(st_geometry(bnt_shp), col = 'lightblue', border = 'darkblue')
plot(grid_sf, add = TRUE, col = 'red', pch = 16)

coordinates(grid)|>head()

############# BUILD VARIOGRAM ###########
#--Convert train / test to sp
coordinates(train_asb) <- c("x", "y")
proj4string(train_asb) <- CRS("+proj=longlat +datum=WGS84")
coordinates(test_asb) <- c("x", "y")
proj4string(test_asb) <- CRS("+proj=longlat +datum=WGS84")

#--Create variogram
lzn.vgm <- variogram(log(n) ~ x+y, train_asb, width=0.1)

#--Create the fitted curve line
lzn.fit = fit.variogram(lzn.vgm, vgm(c("Gau", "Sph", "Mat", "Exp")), fit.kappa = TRUE)

#--Plot the curve function
plot(lzn.vgm, lzn.fit, main = "Variogram Model Fit", cutoff = max(lzn.vgm$dist))
 # When the curve plateaus, the point pairs are no longer spatially correlated


######## KRIGE #######
lzn.kriged <-krige(log(n) ~ x + y, train_asb, test_asb, model = lzn.fit, maxdist=10, nmax=50)
bnt.kriged <-krige(log(n) ~ x + y, train_asb, grid, model = lzn.fit, maxdist=10, nmax=50)

#--Retrieve interpolated values
#---Over test set
test_sf <- st_as_sf(test_asb, coords = c("x", "y"), crs = 4326)
test_sf$krige_pred <- exp(lzn.kriged@data$var1.pred)
test_sf$variance <- exp(lzn.kriged@data$var1.var)

ggplot() +
  geom_sf(data = test_sf, aes(colour = krige_pred)) +
  scale_colour_gradient(low = "blue", high = "red", name = "Predicted Value") +
  ggtitle("Hotspot Map for Kriged Data") +
  geom_sf(data = bnt_shp, alpha=0.1, lwd = 1.2)+
  theme_minimal()

kriging_error = rmse(test_asb$n, test_sf$krige_pred)

kriging_error; range(asb_count$n)
  #12.9; 1-329

#---Over Barnet grid 
grid_sf <- st_as_sf(grid, coords = c("x", "y"), crs=4326)
grid_sf$krige_pred <- exp(bnt.kriged@data$var1.pred)
grid_sf$var <- exp(bnt.kriged@data$var1.var)

ggplot() +
  geom_sf(data = grid_sf, aes(colour = krige_pred)) +
  scale_colour_gradient(low = "blue", high = "red", name = "Predicted Value") +
  ggtitle("Hotspot Map for Kriged Data based on Location Coordinates") +
  geom_sf(data = bnt_shp, alpha=0.1, lwd = 1.2)+
  theme_minimal()

### IMPORT PLACES DATA #####
source(here("2_script", "osm_process.r"))

#--Find nearest place of interest
poi_bnt_fin <- map(poi_bnt_fin, ~.x |> 
                                    select(osm_id, geometry) |>
                                    mutate(row_id = 1:nrow(.x), .before = everything()))

crime_list <- map(crime_list, ~.x |>
                                st_as_sf(coords = c("location.longitude", "location.latitude"), crs=4326) |>
                                select(id, category, month, geometry) |>
                                mutate(crime_row_id = 1:nrow(.x)), .before = everything())

nearest_list <- vector("list", 6)
nearest_df_list <- vector("list", 6)

for (i in seq_along(crime_list)){
  for (j in seq_along(poi_bnt_fin)){
    poi_id <- paste0("id_", names(poi_bnt_fin))
    names(poi_bnt_fin[[j]])[1] <- poi_id[[j]]

    # Find nearest place of interest
    nearest_list[[i]][[j]] <- st_nearest_feature(crime_list[[i]], poi_bnt_fin[[j]])
    nearest_df_list[[i]][[j]] <- data.frame(row_id = nearest_list[[i]][[j]], crime_row_id = 1:length(nearest_list[[i]][[j]]))
    names(nearest_df_list[[i]][[j]])[1] <- poi_id[[j]]
    
    # Join the nearest index with crime
    if(j != length(poi_bnt_fin)){
      crime_list[[i]] <- crime_list[[i]] |>
         left_join(nearest_df_list[[i]][[j]]) 
    }
  }
}

asb <- crime_list[[1]]
asb["id_money_exchange_transfer"] <- st_nearest_feature(asb, poi_bnt_fin[["money_exchange_transfer"]])

#--Initialize columns in asb to store distances
for (poi_name in names(poi_bnt_fin)) {
  asb[[paste0("d_", poi_name)]] <- NA
}

asb[1,][[poi_id[[1]]]]
st_distance(asb[1, ], poi_bnt_fin[[1]][19, ])

#--Loop over each row in asb
for (j in 1:length(poi_bnt_fin)) {
  for (k in 1:nrow(asb)) {

    # Get the index of the nearest POI for the particular crime point
    x <- asb[k,][[poi_id[[j]]]]

    # Calculate the distance between the current asb point and the nearest POI
    distance <- st_distance(asb[k, ], poi_bnt_fin[[j]][x, ])
    
    # Store the distance in the appropriate column in asb
    asb[[paste0("d_", names(poi_bnt_fin)[j])]][k] <- as.numeric(distance)
    
    # Print progress
    print(paste("Nearest distance calculated for", names(poi_bnt_fin)[j], "for asb row", asb$crime_row_id[k]))
  }
}

for (k in 1:nrow(asb)){
  x <- asb[k,][["id_money_exchange_transfer"]]
  print(x)

  asb[["d_money_exchange_transfer"]][k] <- as.numeric(st_distance(asb[k,], poi_bnt_fin[["money_exchange_transfer"]][x,]))
  print("done")
}


#--Order tbl
asb <- asb |> select(crime_row_id, category, month, contains("id_"), contains("d_"))

map(poi_bnt_fin, ~nrow(.x))
##### MULTIVARIATE KRIGE #######
#--Calculate the frequency of ASB by location_id
asb <-  asb |> 
    mutate(location.latitude = st_coordinates(asb)[,2], location.longitude = st_coordinates(asb)[,1]) |>
    st_drop_geometry()

asb_bike_count <- asb |> 
    group_by(location.latitude, location.longitude, d_bicycle_parking) |>
    count() |>
    ungroup() |>
    inner_join(asb, by =c('location.latitude', 'location.longitude',  'd_bicycle_parking')) |>
    distinct(location.latitude, location.longitude,  .keep_all = TRUE) |>
    select(location.latitude, location.longitude,  d_bicycle_parking, n) |>
    group_by(location.latitude, location.longitude) |>
    mutate(location_id =  cur_group_id())

names(asb_bike_count)[1:2] <- c("y", "x")
head(asb_bike_count)

#--Create random indices
total_rows <- nrow(asb_bike_count)
sample_size <- round(total_rows * 0.75)

set.seed(1234)
random_indices <- sample(1:total_rows, sample_size, replace = FALSE)

#--Create the test set using the random indices
train_asb_bike <- asb_bike_count |> filter(location_id %in% random_indices)

# Create the training set by excluding the indices used for the test set
test_asb_bike <-  asb_bike_count |> filter(!location_id %in% random_indices)

############# BUILD VARIOGRAM ###########
#--Convert train / test to sp
coordinates(train_asb_bike) <- c("x", "y")
proj4string(train_asb_bike) <- CRS("+proj=longlat +datum=WGS84")
coordinates(test_asb_bike) <- c("x", "y")
proj4string(test_asb_bike) <- CRS("+proj=longlat +datum=WGS84")

#--Create variogram
bike.vgm <- variogram(log(n) ~ x+y+d_bicycle_parking, train_asb_bike, width=0.1)

#--Create the fitted curve line
bike.fit = fit.variogram(bike.vgm, vgm(c("Gau", "Sph", "Mat", "Exp")), fit.kappa = TRUE)

#--Plot the curve function
plot(bike.vgm, bike.fit, main = "Variogram Model Fit", cutoff = max(bike.vgm$dist))
 # When the curve plateaus, the point pairs are no longer spatially correlated


######## CREATE GRID ##########
#--Incorporate d_bicycle_parking into grid
distances <- st_distance(grid_sf, poi_bnt_fin[["bicycle_parking"]], by_element = FALSE)
min_distances <- apply(distances, 1, min)

grid$d_bicycle_parking <- min_distances

######## MULTI KRIGE TEST #######
bike.kriged <-krige(log(n) ~ x + y + d_bicycle_parking, train_asb_bike, test_asb_bike, model = bike.fit, maxdist=10, nmax=50)
bike.bnt.kriged <-krige(log(n) ~ x + y + d_bicycle_parking, train_asb_bike, grid, model = bike.fit, maxdist=10, nmax=50)

#--Retrieve interpolated values
#---Over test set
test_bike_sf <- st_as_sf(test_asb_bike, coords = c("x", "y"), crs = 4326)
test_bike_sf$krige_pred <- exp(bike.kriged@data$var1.pred)
test_bike_sf$variance <- exp(bike.kriged@data$var1.var)

ggplot() +
  geom_sf(data = test_sf, aes(colour = krige_pred)) +
  scale_colour_gradient(low = "blue", high = "red", name = "Predicted Value") +
  ggtitle("Hotspot Map for Kriged Data based on Location Coordinates and Distance to Closest Bike Parking") +
  geom_sf(data = bnt_shp, alpha=0.1, lwd = 1.2)+
  theme_minimal()

#--Evaluate
#---Kriging error
kriging_error_bike = rmse(test_asb_bike$n, test_bike_sf$krige_pred)

kriging_error; kriging_error_bike; range(asb_bike_count$n)
  #12.7; 1-329

#---Predicted values & variance
range(test_bike_sf$krige_pred); range(test_sf$krige_pred)
range(test_bike_sf$variance); range(test_sf$variance)

#---Over grid
grid_bike_sf <- st_as_sf(grid, coords = c("x", "y"), crs = 4326)
grid_bike_sf$krige_pred <- exp(bike.bnt.kriged@data$var1.pred)
grid_bike_sf$variance <- exp(bike.bnt.kriged@data$var1.var)

ggplot() +
  geom_sf(data = grid_bike_sf, aes(colour = krige_pred)) +
  scale_colour_gradient(low = "blue", high = "red", name = "Predicted Value") +
  labs(title = "Hotspot Map for Kriged Data based on\nLocation Coordinates and Distance to Nearest Bike Parking") +
  geom_sf(data = bnt_shp, alpha=0.1, lwd = 1.2)+
  theme_minimal()+
  theme(title = element_text(size = 9))

########### MULTI KRIGE #############
multi_krige <- function(data, poi){
  d_poi <- paste0("d_", poi)

  # Group data by latitude, longitude, and the poi variable, count occurrences
  data_count <- data %>%
    group_by(location.latitude, location.longitude, !!sym(d_poi)) %>%
    count() %>%
    ungroup() 
  
  print(names(data_count))

  data_count <- data_count |>
    inner_join(data, by = c('location.latitude', 'location.longitude', d_poi)) %>%
    distinct(location.latitude, location.longitude, .keep_all = TRUE) %>%
    select(location.latitude, location.longitude, !!sym(d_poi), n) %>%
    group_by(location.latitude, location.longitude) %>%
    mutate(location_id = cur_group_id())

  print("location pair counted")

  # Rename columns
  names(data_count)[1:2] <- c("y", "x")
  
  # Create random indices for splitting data into train and test sets
  total_rows <- nrow(data_count)
  sample_size <- round(total_rows * 0.75)
  set.seed(1234)
  random_indices <- sample(1:total_rows, sample_size, replace = FALSE)
  
  # Split data into train and test sets
  train_data <- data_count %>%
    filter(location_id %in% random_indices)
  
  test_data <- data_count %>%
    filter(!location_id %in% random_indices)

  print("test/train data splitted") 

  # Convert train and test sets to sp objects
  coordinates(train_data) <- c("x", "y")
  proj4string(train_data) <- CRS("+proj=longlat +datum=WGS84")
  coordinates(test_data) <- c("x", "y")
  proj4string(test_data) <- CRS("+proj=longlat +datum=WGS84")
  
  # Create variogram
  predictors <- c("x", "y", d_poi)
  formula_string <- paste("log(n)", "~", paste(predictors, collapse = " + "))
  krige_formula <- as.formula(formula_string)
  
  vgm <- variogram(krige_formula, train_data, width=0.1)
  
  print("variogram created")

  # Fit variogram
  fit <- fit.variogram(vgm, vgm(c("Gau", "Sph", "Mat", "Exp")), fit.kappa = TRUE)
  
  print("variogram fitted")
  
  # Import Barnet shapefile
  bnt_shp <- sf::st_read(here("1_data", "9_geo", "bnt_lad.json"), crs = 4326)
  bnt_shp <- st_transform(bnt_shp, 4326)
  
  spd <- sf::as_Spatial(st_geometry(bnt_shp), IDs = as.character(1:nrow(bnt_shp)))
  
  spd_data <- bnt_shp
  spd_data$geometry = NULL
  spd_data <- as.data.frame(spd_data)
  spd <- sp::SpatialPolygonsDataFrame(spd, data = spd_data)
  
  # Get the bounding box of the polygon and expand it slightly
  bbox <- bbox(spd)
  cellsize = 0.001
  x_range <- seq(from = bbox[1,1] - cellsize, to = bbox[1,2] + cellsize, by = cellsize)
  y_range <- seq(from = bbox[2,1] - cellsize, to = bbox[2,2] + cellsize, by = cellsize)
  
  # Create a data.frame of grid points
  grid_points <- expand.grid(x = x_range, y = y_range)
  
  # Convert grid points to SpatialPoints
  grid <- SpatialPoints(grid_points, proj4string = CRS("+proj=longlat +datum=WGS84"))
  
  # Filter grid points to include only those within the Barnet polygon
  grid <- grid[spd, ]
  
  print(head(grid))
  print("grid created")

  # Convert grid back to sf object if needed
  grid_sf <- st_as_sf(grid)
  
  # Calculate distances to bicycle parking for grid points
  distances <- st_distance(grid_sf, poi_bnt_fin[[poi]], by_element = FALSE)
  min_distances <- apply(distances, 1, min)
  
  # Add distance to grid_sf 
  grid_sf[[d_poi]] <- min_distances
  
  grid_sf <- grid_sf |>
    mutate(x = st_coordinates(grid_sf)[,1], y = st_coordinates(grid_sf)[,2])

  print("distance to nearest poi from each grid cell calculated")

  # Kriging model
  predictors <- c("x", "y", d_poi)
  formula_string <- paste("log(n)", "~", paste(predictors, collapse = " + "))
  krige_formula <- as.formula(formula_string)
  
  # Perform kriging over the grid
  kriged_result_grid <- krige(
    formula = krige_formula,
    locations = train_data,
    newdata = grid_sf,
    model = fit,
    maxdist = 10,
    nmax = 50
  )
  
  print(names(kriged_result_grid))
  print("kriging done over grid")

  # Perform kriging over the test set
  kriged_result_test <- krige(
    formula = krige_formula,
    locations = train_data,
    newdata = test_data,
    model = fit,
    maxdist = 10,
    nmax = 50
  )
  
  print(names(kriged_result_test))
  print("kriging done over test set")
  # Retrieve interpolated values for the test set
  test_sf <- st_as_sf(test_data, coords = c("x", "y"), crs = 4326)
  test_sf$krige_pred <- exp(kriged_result_test$var1.pred)
  test_sf$variance <- exp(kriged_result_test$var1.var)
  
  # Retrieve interpolated values for the grid
  grid_sf$krige_pred <- exp(kriged_result_grid$var1.pred)
  grid_sf$variance <- exp(kriged_result_grid$var1.var)
  
  # Calculate RMSE for the test set
  kriging_error <- rmse(test_data$n, test_sf$krige_pred)
  
  print("kriging rmse calculated")
 
  return(list(kriging_error = kriging_error, grid_sf = grid_sf, test_sf = test_sf))
}

#--Initialise
multi_krige_list <- vector("list", length(poi_bnt_fin))

#--Apply multi_krige() 
for (i in seq_along(multi_krige_list)){
  multi_krige_list[[i]] <- multi_krige(data = asb, poi = names(poi_bnt_fin)[[i]])
  }

names(multi_krige_list) <- names(poi_bnt_fin)

#--Evaluate
#---Krige
kriging_error_multi <- map(multi_krige_list, ~.x[["kriging_error"]])

kriging_error_df <- data.frame(poi = names(poi_bnt_fin), 
error = unlist(kriging_error_multi)) |> arrange(desc(error))
rownames(kriging_error_df) <- NULL
kriging_error_df

#---SD by category
poi_category <- rio::import(here("0_ref", "poi_category.csv"))

asb_dist <- asb |>
  select(contains("d_") & -contains("id")) 

asb_mean <- asb_dist |> 
  summarise_all(mean) |>
  pivot_longer(cols = everything(), names_to = "poi", values_to = "mean_distance_to_nearest_poi") |>
  arrange(desc(mean_distance_to_nearest_poi)) 

asb_median <- asb_dist |> 
  summarise_all(median) |>
  pivot_longer(cols = everything(), names_to = "poi", values_to = "median_distance_to_nearest_poi") |>
  arrange(desc(median_distance_to_nearest_poi)) 

asb_mean_median <- poi_category |>
  left_join(asb_mean) |>
  left_join(asb_median) 

asb_mean_median |> group_by(poi_category) |> summarise(across(where(is.numeric), ~sd(.x, na.rm=TRUE))) |> 
  arrange(desc(median_distance_to_nearest_poi))

###### EXPORT #######
rio::export(asb, here::here("3_output", "asb_with_nearest_distances.csv"))