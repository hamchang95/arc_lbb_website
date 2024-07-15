########### SET UP ############
#--Install / load packages
rm(list = ls())
pacman::p_load(sf, sp, gstat, here, tmap, osmdata, tidyverse, data.table, rio, tidyverse, leaflet)

#--Load RData from EDA.qmd
load(here("fin", "eda.RData"))
    #contains crime_bnt (sf) & poi_bnt_fin (list)

#--Subset ASB
asb <- subset(crime_bnt, category == "anti social behaviour")

#--Drop geometry
asb_df <-  asb |> 
    mutate(
        latitude = st_coordinates(asb)[, 2],
        longitude = st_coordinates(asb)[, 1]) |>
    st_drop_geometry()

#--Count the number of ASB by location_id
asb_ct <- asb_df |> 
    group_by(longitude, latitude) |>
    count() |> 
    ungroup() |> 
    inner_join(asb_df, by = c('longitude', 'latitude')) |> 
    distinct(longitude, latitude, .keep_all = TRUE) |>
    select(longitude, latitude, n) |>
    group_by(longitude, latitude) |>
    mutate(location_id = group_indices())

#--Convert to shapefile with crs of OSGB36
asb_ct_sf <- asb_ct |>
    st_as_sf(
        coords = c('longitude', 'latitude'),
        crs = 4326) |>
    st_transform(27700)

#---Check
ggplot(asb_ct_sf, aes(colour = n)) +
    geom_sf() +
    scale_color_gradient2()

#--Count 
counts <- numeric(nrow(asb_ct_sf))
l_counts <- vector("list", length(poi_bnt_fin))
l_counts <- map(l_counts, ~vector("numeric", nrow(asb_ct_sf)))
names(l_counts) <- names(poi_bnt_osgb)

poi_bnt_osgb <- map(poi_bnt_fin, ~st_transform(.x, 27700))

for (j in seq_along(poi_bnt_osgb)){
    for (i in 1:nrow(asb_ct_sf)){
      #--Get a point 
      point <- asb_ct_sf[i, ]$geometry
        
      #--Get buffer basically area within 100m 
      buffer <- st_buffer(point, units::as_units(100, "m"))
        
      #--Test if the asb points intersect with buffer
      test <- st_intersects(poi_bnt_osgb[[j]]$geometry, buffer, sparse = FALSE)
        
      #--Calculate the number of points intersecting buffer
      n <- sum(test)
        
      #--Store counts
      l_counts[[j]][i] <- n
        
      print(paste("Row", i, "counted for", names(poi_bnt_fin)[j]))
  }
}

#--Add to asb count data
df_counts <- as.data.frame(l_counts)

asb_n_ct_df <- asb_ct_sf |>
    cbind(df_counts) |>
    mutate(
        lng = st_coordinates(asb_ct_sf)[, 1],
        lat = st_coordinates(asb_ct_sf)[, 2]
    ) |>
    st_drop_geometry()

###### SPLIT #########
#--Define rows for train & test
total_rows <- nrow(asb_n_ct_df)
train_size <- round(total_rows * 0.75)
set.seed(1234)
train_id <- sample(1:total_rows, train_size, replace = FALSE)

#--Split by test and train
train <- asb_n_ct_df[asb_n_ct_df$location_id %in% train_id, ]
test <- asb_n_ct_df[!asb_n_ct_df$location_id %in% train_id, ]

#--Split by x (predictors) and y (outcome)
x_train <- train |> select(-location_id, -n)
x_test <- test |> select(-location_id, -n)
y_train <- train |> select(n, lng, lat)
y_test <- test |> select(n, lng, lat)

###### PCA #########
#--Run PCA
x_train_pca <- FactoMineR::PCA(
    x_train[!names(x_train) %in% c("lng", "lat")],
    graph = FALSE, scale.unit = TRUE)

#--Check cumulative percentage of variance
x_train_pca$eig
# Comp 1-24 explains around 71% of total variance

#--Re-run PCA
x_train_pca <- FactoMineR::PCA(
    x_train[!names(x_train) %in% c("lng", "lat")],
    graph = FALSE, scale.unit = TRUE, ncp = 24)

#--Predict 
x_train_pred <- FactoMineR::predict.PCA(x_train_pca, x_train[!names(x_train) %in% c("lng", "lat")])
x_test_pred <- FactoMineR::predict.PCA(x_train_pca, x_test[!names(x_train) %in% c("lng", "lat")])

#--Extract PCA-transformed data
x_train_pred_df <- as.data.frame(x_train_pred$coord)
x_test_pred_df <- as.data.frame(x_test_pred$coord)

#--Add coordinates to the PCA-transformed data
x_train_pred_sp <- cbind(x_train_pred_df, lng = x_train$lng, lat = x_train$lat)
x_test_pred_sp <- cbind(x_test_pred_df, lng = x_test$lng, lat = x_test$lat)

#--Convert to spatial data frames
sp::coordinates(x_train_pred_sp) <- ~ lng + lat
sp::coordinates(x_test_pred_sp) <- ~ lng + lat
sp::proj4string(x_train_pred_sp) <- sp::CRS("+init=epsg:27700")
sp::proj4string(x_test_pred_sp) <- sp::CRS("+init=epsg:27700")

###### KRIGING ######
#--Combine PCA-transformed predictors with the target variable for training data
train_data <- cbind(x_train_pred_sp, n = y_train$n)
names(train_data)[25] <- "n" 

#--Combine PCA-transformed predictors with the target variable for test data
test_data <- cbind(x_test_pred_sp, n = y_test$n)
names(test_data)[25] <- "n"

#--Create variogram using PCA-transformed predictors
ncp <- 24
eqn <- as.formula(paste0("log(n) ~ lng + lat + ", paste0("Dim.", 1:ncp, collapse = " + ")))

vgm <- variogram( 
    eqn,
    data = train_data, 
    width = 1)

#--Fit variogram model
fit <- fit.variogram(vgm, vgm(c("Gau", "Sph", "Mat", "Exp")), fit.kappa = TRUE)

#--Plot variogram model fit
plot(vgm, main = "Variogram Model Fit", cutoff = max(vgm$dist))
plot(fit, main = "Variogram Model Fit", cutoff = max(vgm$dist))

#--Krige
kriged_train <- krige(
    eqn,
    train_data,
    train_data,
    model = fit
)
kriged_test <- krige(
    eqn,
    train_data,
    test_data,
    model = fit
)

#--Plot
#---Train
train_sf <- st_as_sf(train_data, coords = c("lng", "lat"), crs = 27700)
train_sf$krige_pred <- exp(kriged_train@data$var1.pred)
train_sf$variance <- exp(kriged_train@data$var1.var)

ggplot() +
  geom_sf(data = train_sf, aes(colour = krige_pred)) +
  scale_colour_gradient(low = "blue", high = "red", name = "Predicted Value") +
  ggtitle("Hotspot Map for Kriged Data") +
  theme_minimal()

#---Test
test_sf <- st_as_sf(test_data, coords = c("lng", "lat"), crs = 27700)
test_sf$krige_pred <- exp(kriged_test@data$var1.pred)
test_sf$variance <- exp(kriged_test@data$var1.var)

ggplot() +
  geom_sf(data = test_sf, aes(colour = krige_pred)) +
  scale_colour_gradient(low = "blue", high = "red", name = "Predicted Value") +
  ggtitle("Hotspot Map for Kriged Data") +
  theme_minimal()

##### LOOP #######
#--Re-run PCA
run_pca <- function(ncp, x_train, x_test, y_train, y_test){
    x_train_pca <- FactoMineR::PCA(
    x_train[!names(x_train) %in% c("lng", "lat")],
    graph = FALSE, scale.unit = TRUE, ncp = ncp)

    #--Predict 
    x_train_pred <- FactoMineR::predict.PCA(x_train_pca, x_train[!names(x_train) %in% c("lng", "lat")])
    x_test_pred <- FactoMineR::predict.PCA(x_train_pca, x_test[!names(x_train) %in% c("lng", "lat")])

    #--Extract PCA-transformed data
    x_train_pred_df <- as.data.frame(x_train_pred$coord)
    x_test_pred_df <- as.data.frame(x_test_pred$coord)

    #--Add coordinates to the PCA-transformed data
    x_train_pred_sp <- cbind(x_train_pred_df, lng = x_train$lng, lat = x_train$lat)
    x_test_pred_sp <- cbind(x_test_pred_df, lng = x_test$lng, lat = x_test$lat)

    #--Convert to spatial data frames
    sp::coordinates(x_train_pred_sp) <- ~ lng + lat
    sp::coordinates(x_test_pred_sp) <- ~ lng + lat
    sp::proj4string(x_train_pred_sp) <- sp::CRS("+init=epsg:27700")
    sp::proj4string(x_test_pred_sp) <- sp::CRS("+init=epsg:27700")

   #--Combine PCA-transformed predictors with the target variable for training data
    train_data <- cbind(x_train_pred_sp, n = y_train$n)
    names(train_data)[(ncp + 1)] <- c("n") 

    #--Combine PCA-transformed predictors with the target variable for test data
    test_data <- cbind(x_test_pred_sp, n = y_test$n)
    names(test_data)[(ncp + 1)] <- c("n")

    return(list(train_data, test_data))
}

l_pre_krige <- vector("list", 2)
l_pre_krige <- map(l_pre_krige, ~vector("numeric", ncp))

for (j in 1:ncp){
    l_pre_krige[[j]] <- run_pca(
           ncp = j,
           x_train = x_train,
           x_test = x_test,
           y_train = y_train,
           y_test = y_test)
    }

run_krige_for_pca <- function(ncp, train_data, test_data){
    #--Create variogram using PCA-transformed predictors
    eqn <- as.formula(paste0("log(n) ~ lng + lat + ", paste0("Dim.", 1:ncp, collapse = " + ")))
    print("Equation formulated")

    vgm <- variogram(
        eqn,
        data = train_data, 
        width = 1)
    print("Variogram created")
    
    #--Fit variogram model
    initial_models <- list(vgm("Sph"), vgm("Exp"), vgm("Gau"), vgm("Mat"))
    fit <- NULL
    for (initial_model in initial_models) {
        try({
            fit <- fit.variogram(vgm, model = initial_model, fit.kappa = TRUE)
            if (!is.null(fit)) break
        }, silent = TRUE)
    }
    
    if (is.null(fit)) {
        stop("Variogram fitting failed with all initial models")
    }

    #--Krige
    kriged_train <- krige(
        eqn,
        train_data,
        train_data,
        model = fit
    )
    print("Train data kriged")

    kriged_test <- krige(
        eqn,
        train_data,
        test_data,
        model = fit
    )
    print("Test data kriged")

    #---Store results
    train_sf <- st_as_sf(train_data, coords = c("lng", "lat"), crs = 27700)
    train_sf$krige_pred <- exp(kriged_train@data$var1.pred)
    train_sf$variance <- exp(kriged_train@data$var1.var)

    test_sf <- st_as_sf(test_data, coords = c("lng", "lat"), crs = 27700)
    test_sf$krige_pred <- exp(kriged_test@data$var1.pred)
    test_sf$variance <- exp(kriged_test@data$var1.var)
    print("All results stored")

    return(list(train_sf, test_sf))
}

l_krige <- vector("list", 2)
l_krige <- map(l_krige, ~vector("numeric", ncp_grid))

for (j in 1:ncp){
    l_krige[[j]] <- run_krige_for_pca(
           ncp = j,
           train_data = l_pre_krige[[j]][[1]],
           test_data = l_pre_krige[[j]][[2]] 
    )
}

plot_kriging <- function(train_sf, test_sf){
    p_train <- ggplot() +
    geom_sf(data = train_sf, aes(colour = krige_pred)) +
    scale_colour_gradient(low = "blue", high = "red", name = "Predicted Value") +
    ggtitle("Hotspot Map for Kriged Data") +
    theme_minimal()

    p_test <- ggplot() +
    geom_sf(data = test_sf, aes(colour = krige_pred)) +
    scale_colour_gradient(low = "blue", high = "red", name = "Predicted Value") +
    ggtitle("Hotspot Map for Kriged Data") +
    theme_minimal()

    return(list(p_train, p_test))
}

p_krige <- vector("list", 2)
p_krige <- map(p_krige, ~vector("numeric", ncp))

for (j in 1:ncp){
     p_krige[[j]] <- plot_kriging(
          train_sf <- l_krige[[j]][[1]],
          test_sf <- l_krige[[j]][[2]]
          )
}

evaluate_kriging <- function(train_sf, test_sf){
    # RMSE
    test_rmse <- Metrics::rmse(test_sf$n, test_sf$krige_pred)
    train_rmse <- Metrics::rmse(train_sf$n, test_sf$krige_pred)

    return(list(train_rmse, test_rmse))
}

l_eval_krige <- vector("list", 2)
l_eval_krige <- map(l_eval_krige, ~vector("numeric", ncp))

for (j in 1:ncp){
    l_eval_krige[[j]] <- evaluate_kriging(l_krige[[j]][[1]], l_krige[[j]][[2]])
}

l_train_rmse <- vector("list", ncp)
l_test_rmse <- vector("list", ncp)

for (j in 1:ncp){
    l_train_rmse[[j]] <- l_eval_krige[[j]][[1]]
    l_test_rmse[[j]] <- l_eval_krige[[j]][[2]]
}

df_train_rmse <- data.frame(
    ncp = 1:ncp, 
    rmse = as.numeric(l_train_rmse)
)
df_test_rmse <- data.frame(
    ncp = 1:ncp, 
    rmse = as.numeric(l_test_rmse)
)

p_train_rmse <- ggplot(df_train_rmse, aes(ncp, rmse)) +
    geom_point() +
    geom_line() +
    ggtitle("Train RMSE") 

p_test_rmse <- ggplot(df_test_rmse, aes(ncp, rmse)) +
    geom_point() +
    geom_line()+
    ggtitle("Test RMSE") 

cowplot::plot_grid(p_train_rmse, p_test_rmse)

####### CREATE GRID #######
#--Import Barnet shapefile
bnt_shp <- sf::st_read(here("1_data", "9_geo", "bnt_lad.json"), crs = 4326) |> 
    st_make_valid()

#--Make grid
grid <- bnt_shp |> 
    st_make_grid(n = c(100, 100), what = "centers") |>
    st_as_sf()

#--Filter grid points to include only those within the Barnet polygon
result <- st_within(grid, bnt_shp) |>
    as.data.frame()

grid_bnt <- grid |> 
    mutate(row.id = 1:nrow(grid)) |> 
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

#--Count
grid_ct <- grid_bnt_osgb |>
    mutate(lng = st_coordinates(grid_bnt_osgb)[, 1],
           lat = st_coordinates(grid_bnt_osgb)[, 2]) |>
    group_by(lng, lat) |>
    count() |> 
    ungroup() |> 
    distinct(lng, lat, .keep_all = TRUE) |>
    select(lng, lat, n) 

grid_ct <- grid_ct |>
    mutate(location_id = 1:nrow(grid_ct)) 

######### COUNT ##########
#--Initialise
counts <- numeric(nrow(grid_ct))
l_grid_counts <- vector("list", length(poi_bnt_fin))
l_grid_counts <- map(l_grid_counts, ~vector("numeric", nrow(grid_ct)))
names(l_grid_counts) <- names(poi_bnt_osgb)

#--Count
for (j in seq_along(poi_bnt_osgb)){
    for (i in 1:nrow(grid_ct)){
      #--Get a point 
      point <- grid_ct[i, ]$x
        
      #--Get buffer basically area within 100m 
      buffer <- st_buffer(point, units::as_units(100, "m"))
        
      #--Test if the asb points intersect with buffer
      intersect_test <- st_intersects(poi_bnt_osgb[[j]]$geometry, buffer, sparse = FALSE)
        
      #--Calculate the number of points intersecting buffer
      n <- sum(within_test)
        
      #--Store counts
      l_grid_counts[[j]][i] <- n
        
      print(paste("Row", i, "counted for", names(poi_bnt_fin)[j], j, "/", length(poi_bnt_fin)))
  }
}

#--Add to gri count data
df_grid_counts <- as.data.frame(l_grid_counts)

lapply(df_grid_counts, table)
head(df_grid_counts)

grid_ct_df <- grid_ct |>
    st_drop_geometry() |>
    cbind(df_grid_counts)

head(grid_ct_df)

###### SPLIT #########
#--Define rows for train & test
total_grid_rows <- nrow(grid_ct_df)
train_grid_size <- round(total_grid_rows * 0.75)
set.seed(1234)
train_grid_id <- sample(1:total_grid_rows, train_grid_size, replace = FALSE)

#--Split by test and train
train_grid <- grid_ct_df[grid_ct_df$location_id %in% train_grid_id, ]
test_grid <- grid_ct_df[!grid_ct_df$location_id %in% train_grid_id, ]

#--Split by x (predictors) and y (outcome)
x_grid_train <- train_grid |> select(-location_id, -n)
x_grid_test <- test_grid |> select(-location_id, -n)
y_grid_train <- train_grid |> select(n, lng, lat)
y_grid_test <- test_grid |> select(n, lng, lat)

###### PCA #########
#--Run PCA
x_grid_train_pca <- FactoMineR::PCA(
    x_grid_train[!names(x_grid_train) %in% c("lng", "lat")],
    graph = FALSE, scale.unit = TRUE)
check <- lapply(x_grid_train[!names(x_grid_train) %in% c("lng", "lat")], table)
check
#--Check cumulative percentage of variance
x_grid_train_pca$eig
# Comp 1-24 explains around 71% of total variance
ncp_grid <- 24

#--Re-run PCA
l_g_pre_krige <- vector("list", 2)
l_g_pre_krige <- map(l_pre_krige, ~vector("numeric", ncp_grid))

for (j in ncp_grid:1){
    l_g_pre_krige[[j]] <- run_pca(
           ncp = j,
           x_train = x_grid_train,
           x_test = x_grid_test,
           y_train = y_grid_train,
           y_test = y_grid_test)
    }


###### KRIGING ######
l_g_krige <- vector("list", 2)
l_g_krige <- map(l_g_krige, ~vector("numeric", ncp_grid))

for (j in 1:ncp_grid){
    l_g_krige[[j]] <- run_krige_for_pca(
           ncp = j,
           train_data = l_pre_krige[[j]][[1]],
           test_data = l_pre_krige[[j]][[2]] 
    )
}

p_g_krige <- vector("list", 2)
p_g_krige <- map(p_g_krige, ~vector("numeric", ncp_grid))

for (j in 1:ncp_grid){
     p_g_krige[[j]] <- plot_kriging(
          train_sf <- l_g_krige[[j]][[1]],
          test_sf <- l_g_krige[[j]][[2]]
          )
}



