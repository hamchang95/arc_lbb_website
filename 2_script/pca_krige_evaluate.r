krige_with_pca <- function(number_of_pc, train_size){
    ####### SPLIT ######
    #--Create random indices
    total_rows <- nrow(asb_count)
    sample_size <- round(total_rows * train_size)

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
    x_train_pca <- PCA(x_train[-c(1,2)], graph = FALSE, scale.unit = TRUE, ncp = number_of_pc)

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
    names(train_data)[ncol(train_data)] <- "n" 

    #--Combine PCA-transformed predictors with the target variable for test data
    test_data <- cbind(x_test_pred_df, n = y_test$n)
    names(test_data)[ncol(test_data)] <- "n"

    #--Create variogram using PCA-transformed predictors
    dim_list <- paste0("Dim.", 1:number_of_pc, collapse = " + ")

    fx <- as.formula(paste("log(n) ~ x + y +", dim_list))

    lzn.vgm <- variogram(
        fx, 
        data = train_data, 
        width = 1)

    #--Fit variogram model
    lzn.fit <- fit.variogram(lzn.vgm, vgm(c("Gau", "Sph", "Mat", "Exp")), fit.kappa = TRUE)

    #--Kriging over the test set
    lzn.kriged <- krige(
        fx, 
        train_data, 
        test_data, 
        model = lzn.fit)

    #--Kriging over the train set
    lzn.kriged.train <- krige(
        fx, 
        train_data, 
        train_data, 
        model = lzn.fit)

    ###### EVALUATION #######
    #--RMSE
    rmse_test <- rmse(test_data$n, exp(lzn.kriged@data$var1.pred))
    rmse_train <- rmse(train_data$n, exp(lzn.kriged.train@data$var1.pred))

    #--MAE
    mae_test <- mae(test_data$n, exp(lzn.kriged@data$var1.pred))
    mae_train <- mae(train_data$n, exp(lzn.kriged.train@data$var1.pred))

    #--Average Error
    error_test <- mean(test_data$n - exp(lzn.kriged@data$var1.pred))
    error_train <- mean(train_data$n - exp(lzn.kriged.train@data$var1.pred))

    #--Map
    map_test_train <- function(data) {
        map <- data |> 
            st_as_sf(coords = c("x", "y"), crs = 27700) |>
            ggplot() + 
            geom_sf()

        return(map)
    }
    
    test_train <- list(x_test, x_train, y_test, y_train)

    map_list <- lapply(test_train, map_test_train)

    return(list(test_krige = lzn.kriged,
                train_krige = lzn.kriged.train,
                rmse_test = rmse_test,
                rmse_train = rmse_train,
                mae_test = mae_test,
                mae_train = mae_train,
                error_test = error_test,
                error_train = error_train,
                map = map_list))
}

#--Initialise
result_0.75 <- list()
result_0.7 <- list()

#--Get results 
for (i in 1:7){
    result_0.75[[i]] <- krige_with_pca(number_of_pc = i, train_size = 0.75)
    print(paste0("Iteration ", i, " done for 0.75!"))

    result_0.7[[i]] <- krige_with_pca(number_of_pc = i, train_size = 0.7)
    
    print(paste0("Iteration ", i, " done for 0.7!"))
}

#--Extract error-related info
rmse <- vector("list", 4)
mae <- vector("list", 4)
error <- vector("list", 4)

for (i in 1:7){
    rmse[[1]][[i]] <- result_0.7[[i]][["rmse_train"]]
    rmse[[2]][[i]] <- result_0.7[[i]][["rmse_test"]]
    rmse[[3]][[i]] <- result_0.75[[i]][["rmse_train"]]
    rmse[[4]][[i]] <- result_0.75[[i]][["rmse_test"]]
    
    mae[[1]][[i]] <- result_0.7[[i]][["mae_train"]]
    mae[[2]][[i]] <- result_0.7[[i]][["mae_test"]]
    mae[[3]][[i]] <- result_0.75[[i]][["mae_train"]]
    mae[[4]][[i]] <- result_0.75[[i]][["mae_test"]]

    error[[1]][[i]] <- result_0.7[[i]][["error_train"]]
    error[[2]][[i]] <- result_0.7[[i]][["error_test"]]
    error[[3]][[i]] <- result_0.75[[i]][["error_train"]]
    error[[4]][[i]] <- result_0.75[[i]][["error_test"]]
}

rmse_df <- map(rmse, ~as.matrix(.x, nrow = 7) |> 
                        as.data.frame() |> 
                        mutate(ncp = 1:7, .before = everything()) |> 
                        rename(RMSE = 2) |> 
                        mutate(RMSE = as.numeric(RMSE)))

mae_df <- map(mae, ~as.matrix(.x, nrow = 7) |> 
                        as.data.frame() |> 
                        mutate(ncp = 1:7, .before = everything()) |> 
                        rename(MAE = 2) |> 
                        mutate(MAE = as.numeric(MAE)))

error_df <- map(error, ~as.matrix(.x, nrow = 7) |> 
                        as.data.frame() |> 
                        mutate(ncp = 1:7, .before = everything()) |> 
                        rename(ERROR = 2)|> 
                        mutate(ERROR = as.numeric(ERROR)))

#--Plot errors 
p_error_list <- vector("list", 3)

for (i in 1:4){
    p_error_list[[1]][[i]] <- ggplot(rmse_df[[i]], aes(x = ncp, y = RMSE)) +
    geom_point() +
    geom_line() +
    scale_x_continuous(breaks = 1:7)+
    theme_minimal()

    p_error_list[[2]][[i]] <- ggplot(mae_df[[i]], aes(x = ncp, y = MAE)) +
    geom_point() +
    geom_line() +
    scale_x_continuous(breaks = 1:7)+
    theme_minimal()

    p_error_list[[3]][[i]] <- ggplot(error_df[[i]], aes(x = ncp, y = ERROR)) +
    geom_point() +
    geom_line() +
    scale_x_continuous(breaks = 1:7)+
    theme_minimal()
}

pacman::p_load(cowplot)

cowplot::plot_grid(p_error_list[[1]][[1]], p_error_list[[1]][[2]], 
                   p_error_list[[1]][[3]], p_error_list[[1]][[4]],
                   labels = c("Train RMSE for 7:3", "Test RMSE for 7:3",
                              "Train RMSE for 7.5:2.5", "Test RMSE for 7.5:2.5"),
                   label_size = 12)

cowplot::plot_grid(p_error_list[[2]][[1]], p_error_list[[2]][[2]], 
                   p_error_list[[2]][[3]], p_error_list[[2]][[4]],
                   labels = c("Train MAE for 7:3", "Test MAE for 7:3",
                              "Train MAE for 7.5:2.5", "Test MAE for 7.5:2.5"),
                   label_size = 12)

cowplot::plot_grid(p_error_list[[3]][[1]], p_error_list[[3]][[2]], 
                   p_error_list[[3]][[3]], p_error_list[[3]][[4]],
                   labels = c("Train ERROR for 7:3", "Test ERROR for 7:3",
                              "Train ERROR for 7.5:2.5", "Test ERROR for 7.5:2.5"),
                   label_size = 12)
