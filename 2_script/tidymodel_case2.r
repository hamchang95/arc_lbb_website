######### SET UP #########
rm(list = ls())
pacman::p_load(ranger, broom, kernlab, doParallel, workflowsets, rpart.plot, lubridate, here, rio, vip, ggplot2, readr, rsample, parsnip, recipes, workflows, tune, yardstick)

asb <- rio::import(here("3_output", "asb_with_nearest_distances.csv"))

str(asb)

# Basic transformation
asb <- asb |>
  rename(latitude = location.latitude, longitude = location.longitude) |>
  mutate(across(where(is.character), as.factor)) |>
  select(-contains("id"), -category)|>
  mutate(year = year(ymd(paste0(month, "-01"))),
         month_num = month(ymd(paste0(month, "-01"))))

asb_ct <- asb |> 
  group_by(longitude, latitude) |>
  count() |> 
  ungroup() |>
  inner_join(asb, by = c('longitude', 'latitude')) |>
  distinct(longitude, latitude, .keep_all = TRUE) |>
  select(!c(month, longitude, latitude))

# Split the data into training and testing sets
set.seed(1234) 
asb_split <- initial_split(asb_ct, prop = 0.8)
asb_train <- training(asb_split)
asb_test <- testing(asb_split)

# Preprocess the data
asb_recipe <- recipe(n ~ ., data = asb_train) |>
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
summary(asb_train_preprocessed)        

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
        trees = 1000,
        mtry = tune(),
        # no. of predictors that'll be randomly sampled at each split when creating the tree models
        min_n = tune()
        # minimum no. of data points in a node required for the node to be split further
        # the node needs to have at least min_n data points
    ) |>
    set_engine('ranger', importance = "impurity") |>
    set_mode('regression')

#--Create workflow
distr_workflowset <- 
    workflow_set(
        preproc = list("rec" = asb_recipe), 
        models = list(
            "lm" = lm_spec, 
            "svm" = svm_spec, 
            "rf" = rf_spec
        )
    ) |>
    mutate(wflow_id = wflow_id |> str_remove_all("rec_"))

################# MODEL TRAINING ###############
#--Control settings
distr_grid_ctrl <- 
    control_grid(
        save_pred = TRUE, 
        parallel_over = "everything",
        # enables parallel processing for all operations
        save_workflow = TRUE
    )

#--Parallel processing
cores <- parallel::detectCores()
doParallel::registerDoParallel(cores = cores)

#--Tune models
distr_tune <- 
    distr_workflowset |>
    workflow_map("tune_grid", seed = 1234, 
                resamples = asb_cv, 
                grid = 20, 
                control = distr_grid_ctrl, 
                verbose = TRUE)

saveRDS(distr_tune, paste0("trained_models_", Sys.Date(), "_distr_tune.rds"))

doParallel::stopImplicitCluster()
########### MODEL EVALUATION ##########
#--Compare models
#---Plot
autoplot(distr_tune, select_best = TRUE)

#---Rank
(ranks <- distr_tune |>
    rank_results(select_best = TRUE) |>
    select(-std_err) |>
    pivot_wider(names_from = .metric, values_from = mean) |>
    select(wflow_id, rank, rmse, rsq))

#--Choose best model
(best_wflow_id <- ranks |> 
    head(1) |> pull(wflow_id))

best_results <- 
    distr_tune |>
    extract_workflow_set_result(best_wflow_id) |>
    select_best(metric = "rmse")

lm_results <- 
    distr_tune |>
    extract_workflow_set_result("lm") |>
    select_best(metric = "rmse")

svm_results <- 
    distr_tune |>
    extract_workflow_set_result("svm") |>
    select_best(metric = "rmse")

best_results
# At least 2 data points were required for the node to be split further
# 10 predictors were randomly sampled at each split

lm_results

#--Fit test set
#---Best model (rf)
test_results <- 
    distr_tune |>
    extract_workflow(best_wflow_id) |>
    finalize_workflow(best_results) |>
    last_fit(split = asb_split)

#---LM
lm_test_results <- 
    distr_tune |>
    extract_workflow("lm") |>
    finalize_workflow(lm_results) |>
    last_fit(split = asb_split)

svm_test_results <- 
    distr_tune |>
    extract_workflow("svm") |>
    finalize_workflow(svm_results) |>
    last_fit(split = asb_split)

#--Get metrics
collect_metrics(test_results)
collect_metrics(lm_test_results)
collect_metrics(svm_test_results)

#--Plot observed v. predicted
test_results |>
    collect_predictions() |>
    ggplot(aes(x = n, y = .pred)) + 
    geom_abline(color = "gray50", lty = 2) + 
    geom_point(alpha = 0.5) + 
    coord_obs_pred() + 
    labs(x = "observed crime count", y = "predicted crime count", 
         title = paste0("Predicted vs. Observed Crime Count with ", best_wflow_id))

lm_test_results |>
    collect_predictions() |>
    ggplot(aes(x = n, y = .pred)) + 
    geom_abline(color = "gray50", lty = 2) + 
    geom_point(alpha = 0.5) + 
    coord_obs_pred() + 
    labs(x = "observed crime count", y = "predicted crime count", 
         title = paste0("Predicted vs. Observed Crime Count with LM"))

svm_test_results |>
    collect_predictions() |>
    ggplot(aes(x = n, y = .pred)) + 
    geom_abline(color = "gray50", lty = 2) + 
    geom_point(alpha = 0.5) + 
    coord_obs_pred() + 
    labs(x = "observed crime count", y = "predicted crime count", 
         title = paste0("Predicted vs. Observed Crime Count with SVM"))

################
# Fit the models
lm_fit <- distr_tune |>
    extract_workflow("lm") |>
    finalize_workflow(lm_results) |>
    fit(data = asb_train_preprocessed)

rf_fit <- distr_tune |>
    extract_workflow(best_wflow_id) |>
    finalize_workflow(best_results) |>
    fit(data = asb_train_preprocessed)

# Extract model details
lm_fit_parsnip <- lm_fit |> extract_fit_parsnip()
rf_fit_parsnip <- rf_fit |> extract_fit_parsnip()

# Linear model coefficients
lm_coefficients <- tidy(lm_fit_parsnip)
lm_fit_parsnip$fit$variable.importance
print.data.frame(lm_coefficients)

# Random forest variable importance
rf_importance <- rf_fit_parsnip$fit$variable.importance
print(rf_importance)

# Use the vip() function to get the variable importance
vip_plot <- rf_fit |>
    extract_fit_parsnip() |>
    vip(ncol(asb_train_preprocessed) - 1)
print(vip_plot)

# Convert rf_importance to a tibble for comparison
rf_importance_tibble <- enframe(rf_importance, name = "variable", value = "importance_ranger")

# Extract the importance from vip() in a comparable format
vip_importance <- vip_plot$data
vip_importance <- vip_importance |> rename(importance_vip = Importance, variable = Variable)

# Merge both sets of importance scores
importance_comparison <- rf_importance_tibble |> 
    left_join(vip_importance, by = "variable")

print(importance_comparison)

# Random forest tree structure (example tree)
tree_info <- treeInfo(rf_fit_parsnip$fit, tree = 1)
print(tree_info)
