######### SET UP #########
rm(list = ls())
pacman::p_load(dials,ranger, broom, kernlab, doParallel, workflowsets, rpart.plot, lubridate, here, rio, vip, ggplot2, readr, rsample, parsnip, recipes, workflows, tune, yardstick)

asb <- rio::import(here("3_output", "asb_with_nearest_distances.csv"))

# Basic transformation
asb <- asb |>
  rename(latitude = location.latitude, longitude = location.longitude) |>
  mutate(across(where(is.character), as.factor)) |>
  select(-contains("id"), -category)

# Load non-asb points
non_asb <- readRDS("non_asb_points.RDS") 

non_asb <- non_asb |>
  st_transform(4326) |>
  mutate(
    longitude = st_coordinates(non_asb)[, 1],
    latitude = st_coordinates(non_asb)[, 2]
  ) 

# Count
asb_ct <- asb |> 
  group_by(longitude, latitude) |>
  count() |> 
  ungroup() |>
  inner_join(asb, by = c('longitude', 'latitude')) |>
  distinct(longitude, latitude, .keep_all = TRUE) |>
  select(!c(longitude, latitude, month)) 

non_asb_ct <- non_asb |>
  st_drop_geometry() |>
  group_by(longitude, latitude) |>
  count() |> 
  ungroup() |>
  inner_join(non_asb, by = c('longitude', 'latitude')) |>
  distinct(longitude, latitude, .keep_all = TRUE) |>
  st_drop_geometry() |>
  select(!c(longitude, latitude, geometry, d_bridge)) |>
  mutate(n = 0)

# Combine
asb_all <- rbind(asb_ct, non_asb_ct)

# Split the data into training and testing sets
set.seed(1234) 
asb_split <- initial_split(asb_all, prop = 0.8)
asb_train <- training(asb_split)
asb_test <- testing(asb_split)

# Preprocess the data
asb_recipe <- recipe(n ~ ., data = asb_train) |>
  step_BoxCox(all_outcomes()) |> # this step didn't really run bcs of non-positive values in the outcome
  step_normalize(all_numeric_predictors()) |>
  # centres & scales numeric variables
  step_zv(all_predictors()) 
  # removes indicator variables that only contain a single unique values

# Prepare the recipe
asb_prep <- recipes::prep(asb_recipe)

# Bake the training and testing datasets
asb_train_preprocessed <- bake(asb_prep, new_data = asb_train)
asb_test_preprocessed <- bake(asb_prep, new_data = asb_test)

# Print a summary of the preprocessed training data
summary(asb_train_preprocessed$n)        
hist(asb_train_preprocessed$n)

# Create a 5-fold CV object from training data
asb_cv <- vfold_cv(asb_train, 5)

##### MODEL SPECIFICATION #####
#--Linear regression
lm_spec <-
    linear_reg() |>
    set_engine('lm')

#--SVM regression
svm_spec <-
    svm_poly(
        cost = tune(), 
        degree = tune()
    ) |>
    set_engine('kernlab') |>
    set_mode('regression')

#--RF regression
rf_spec <-
    rand_forest(
        trees = 500,
        mtry = tune(),
        # no. of predictors that'll be randomly sampled at each split when creating the tree models
        min_n = 10
        # minimum no. of data points in a node required for the node to be split further
        # the node needs to have at least min_n data points
    ) |>
    set_engine('ranger', importance = "impurity") |>
    set_mode('regression')

#--Create workflow
lm_wf <- workflow() |>
    add_model(lm_spec) |>
    add_recipe(asb_recipe)

svm_wf <- workflow() |>
    add_model(svm_spec)|>
    add_recipe(asb_recipe)

rf_wf <- workflow() |>
    add_model(rf_spec)|>
    add_recipe(asb_recipe)

################# MODEL TRAINING ###############
#--Control settings
distr_grid_ctrl <- 
    control_grid(
        save_pred = TRUE, 
        save_workflow = TRUE,
        verbose = TRUE
    )

# Define a smaller tuning grid
svm_grid <- grid_regular(
  cost(range = c(-1, 1)),
  degree(range = c(1, 3)),
  levels = 3
)

# Define the tuning grid for rf
rf_grid <- grid_regular(
    mtry(range = c(1, ncol(asb_train_preprocessed)) - 1),
    levels = 5)

# Enable parallel processing
library(doParallel)
cl <- makePSOCKcluster(detectCores() - 1)
registerDoParallel(cl)

#--Tune models
tuned_lm <- tune_grid(
    lm_wf,
    resamples = asb_cv,
    control = distr_grid_ctrl)

tuned_svm <- tune_grid(
    svm_wf,
    grid = svm_grid,
    resamples = asb_cv, 
    control = distr_grid_ctrl)

tuned_rf <- tune_grid(
    rf_wf,
    grid = rf_grid,
    resamples = asb_cv, 
    control = distr_grid_ctrl)

# Stop parallel processing
stopCluster(cl)

############ MODEL EVALUATION ##############
# Get metrics
lm_metrics <- collect_metrics(tuned_lm)
svm_metrics <- collect_metrics(tuned_svm) 
rf_metrics <- collect_metrics(tuned_rf)

# Compare RMSE of all models
lm_best <- lm_metrics |> filter(.metric == "rmse") |> arrange(mean) |> slice(1)
svm_best <- svm_metrics |> filter(.metric == "rmse") |> arrange(mean) |> slice(1)
rf_best <- rf_metrics |> filter(.metric == "rmse") |> arrange(mean) |> slice(1)

print(lm_best)
print(svm_best)
print(rf_best)

# Extract the best hyperparameters for rf
best_rf_params <- select_best(tuned_rf, metric =  "rmse")

# Finalize the rf workflow with best parameters
final_rf_wf <- finalize_workflow(rf_wf, best_rf_params)

# Train the final model on the entire training dataset
final_rf_fit <- fit(final_rf_wf, data = asb_train_preprocessed)

############ MODEL TESTING ###############
# Predict on the test data
rf_test_predictions <- predict(final_rf_fit, asb_test_preprocessed)

# Evaluate the predictions
rf_test_results <- bind_cols(asb_test_preprocessed, rf_test_predictions)

# Calculate performance metrics
metrics_result <- rf_test_results |>
  metrics(truth = n, estimate = .pred)

saveRDS(final_rf_fit, file = paste0("final_rf_model", Sys.Date(), ".RDS"))

# Plot the importance of features
vip(final_rf_fit, num_features = 49)

# Plot predicted vs actual values
p_rf <- rf_test_results |>
  ggplot(aes(x = n, y = .pred)) +
  geom_point() +
  geom_abline(linetype = "dashed", color = "red") +
  labs(title = "Predicted vs Actual", x = "Actual Values", y = "Predicted Values")

summary(rf_test_results$.pred)

p_rf
###########
# Extract lambda value from the prepped recipe
#lambda <- asb_prep$steps[[1]]$lambdas

# Define a function for the inverse Box-Cox transformation
#inverse_boxcox <- function(y, lambda) {
  #if (lambda == 0) {
    #exp(y)
  #} else {
    #(lambda * y + 1)^(1 / lambda)
  #}
#}

# Apply the inverse Box-Cox transformation to the actual and predicted values
#rf_test_results <- rf_test_results %>%
  #mutate(
    #actual_original = inverse_boxcox(n, lambda),
    #predicted_original = inverse_boxcox(.pred, lambda)
  #)

# Summary of the back-transformed values
#summary(rf_test_results$actual_original)
#summary(rf_test_results$predicted_original)

# Plot predicted vs actual values on the original scale
#p_rf_original <- rf_test_results %>%
  #ggplot(aes(x = actual_original, y = predicted_original)) +
  #geom_point() +
  #geom_abline(linetype = "dashed", color = "red") +
  #labs(title = "Predicted vs Actual (Original Scale)", x = "Actual Values", y = "Predicted Values")

#print(p_rf_original)

# Calculate performance metrics
#metrics_result <- rf_test_results |>
  #metrics(truth = actual_original, estimate = predicted_original)

#metrics_result

save.image("ml_reg_ws.RData")