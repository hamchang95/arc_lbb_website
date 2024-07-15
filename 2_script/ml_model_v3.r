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

# Classify crime risk
summary(asb_all$n)
hist(log(asb_all$n)[asb_all$n >= 3])
length(asb_all$n[asb_all$n < 3])
    # 3 was chosen as the threshold as it was median

asb_bi <- asb_all |>
    mutate(risk = ifelse(n < 3, "low", "high")) |>
    mutate(risk = factor(risk, levels = c("low", "high"))) |>
    select(-n)

# Split the data into training and testing sets
set.seed(1234) 
asb_split <- initial_split(asb_bi, prop = 0.8)
asb_train <- training(asb_split)
asb_test <- testing(asb_split)

# Preprocess the data
asb_recipe <- recipe(risk ~ ., data = asb_train) %>%
  step_nzv(all_numeric_predictors()) %>%
  step_dummy(all_nominal_predictors(), -all_outcomes()) %>%
  step_normalize(all_numeric_predictors())
  # removes indicator variables that only contain a single unique values

# Create a 5-fold CV object from training data
asb_cv <- vfold_cv(asb_train, 5)

# Prepare the recipe
asb_prep <- recipes::prep(asb_recipe)

# Bake the training and testing datasets
asb_train_preprocessed <- bake(asb_prep, new_data = asb_train)
asb_test_preprocessed <- bake(asb_prep, new_data = asb_test)

# Print a summary of the preprocessed training data
summary(asb_train_preprocessed$risk)        

##### MODEL SPECIFICATION #####
#--Logistic regression
lr_spec <-
    logistic_reg(penalty = tune(), mixture = tune()) |>
    set_engine('glmnet')

#--SVM regression
svm_spec <-
    svm_poly(
        cost = tune(), 
        degree = tune()
    ) |>
    set_engine('kernlab') |>
    set_mode('classification')

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
    set_engine('ranger') |>
    set_mode('classification')

#--Create workflow
lr_wf <- workflow() |>
    add_model(lr_spec) |>
    add_recipe(asb_recipe)

svm_wf <- workflow() |>
    add_model(svm_spec)|>
    add_recipe(asb_recipe)

rf_wf <- workflow() |>
    add_model(rf_spec)|>
    add_recipe(asb_recipe)
################# MODEL TRAINING ###############
#--Control settings
grid_ctrl <- 
    control_grid(
        save_pred = TRUE, 
        save_workflow = TRUE
    )

# Define a smaller tuning grid
svm_grid <- grid_regular(
  cost(range = c(-1, 1)),
  degree(range = c(1, 3)),
  levels = 3
)

# Define the tuning grid for rf
rf_grid <- grid_regular(
    mtry(range = c(1, ncol(asb_train) - 1)),
    levels = 5)

# Enable parallel processing
library(doParallel)
cl <- makePSOCKcluster(detectCores() - 1)
registerDoParallel(cl)

#--Tune models
tuned_lr <- tune_grid(
    lr_wf,
    resamples = asb_cv,
    control = grid_ctrl)

tuned_svm <- tune_grid(
    svm_wf,
    grid = svm_grid,
    resamples = asb_cv, 
    control = grid_ctrl)

tuned_rf <- tune_grid(
    rf_wf,
    grid = rf_grid,
    resamples = asb_cv, 
    control = grid_ctrl)

############ MODEL EVALUATION ##############
# Get metrics
lr_metrics <- collect_metrics(tuned_lr)
svm_metrics <- collect_metrics(tuned_svm) 
rf_metrics <- collect_metrics(tuned_rf)

# Compare RMSE of all models
lr_best <- lr_metrics |> filter(.metric == "roc_auc") |> arrange(mean) |> slice(1)
svm_best <- svm_metrics |> filter(.metric == "roc_auc") |> arrange(mean) |> slice(1)
rf_best <- rf_metrics |> filter(.metric == "roc_auc") |> arrange(mean) |> slice(1)

print(lr_best)
print(svm_best)
print(rf_best)

# Extract the best hyperparameters for rf
best_lr_params <- select_best(tuned_lr, metric =  "roc_auc")
best_svm_params <- select_best(tuned_svm, metric =  "roc_auc")
best_rf_params <- select_best(tuned_rf, metric =  "roc_auc")

# Plot AUC
lr_auc <- tuned_lr |>
  collect_predictions(parameters = lr_best) |>
  roc_curve(risk, .pred_high) |>
  mutate(model = "Logistic Regression")

svm_auc <- tuned_svm |>
  collect_predictions(parameters = svm_best) |>
  roc_curve(risk, .pred_high) |>
  mutate(model = "Support Vector Machine")

rf_auc <- tuned_rf |>
  collect_predictions(parameters = rf_best) |>
  roc_curve(risk, .pred_high) |>
  mutate(model = "Random Forest")

rf_auc |>
  bind_rows(svm_auc) |>
  bind_rows(lr_auc) |>
  ggplot(aes(x = 1 - specificity, y = sensitivity, col = model)) + 
  geom_path(lwd = 1.5, alpha = 0.8) +
  geom_abline(lty = 3) + 
  coord_equal() + 
  scale_color_viridis_d(option = "plasma", end = .6) +
  theme_minimal()


############ MODEL TESTING ###############
# Finalize the rf workflow with best parameters
final_rf_wf <- finalize_workflow(rf_wf, best_rf_params) 

# Perform the last fit on the testing data
last_fit_results <- final_rf_wf |>
  last_fit(asb_split) 

# Collect and print the metrics
collect_metrics(last_fit_results)

# Get the predictions
last_predictions <- last_fit_results %>%
  collect_predictions()

# Plot ROC curve
roc_curve_high <- roc_curve(last_predictions, truth = risk, .pred_high)
roc_curve_low <- roc_curve(last_predictions, truth = risk, .pred_low)

autoplot(roc_curve_low)

# Calculate and print the AUC
auc_high <- roc_auc(last_predictions, truth = risk, .pred_high)
auc_low <- roc_auc(last_predictions, truth = risk, .pred_low)

# Confusion matrix
conf_mat(last_predictions, truth = risk, estimate = .pred_class) 

save.image("ml_classification_ws.RData")