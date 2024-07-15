######### SET UP #########
pacman::p_load(vip, ggplot2, readr, rsample, parsnip, recipes, workflows, tune, yardstick)

hotels <- 
  read_csv("https://tidymodels.org/start/case-study/hotels.csv") %>%
  mutate(across(where(is.character), as.factor))

dim(hotels)

glimpse(hotels)

hotels %>% 
  count(children) %>% 
  mutate(prop = n/sum(n))

######### DATA SPLITTING & RESAMPLING #########
set.seed(123)

# Split data into testing and non-testing (other) sets
splits      <- rsample::initial_split(hotels, strata = children)
hotel_other <- rsample::training(splits)
hotel_test <- rsample::testing(splits)

hotel_other %>% 
  count(children) %>% 
  mutate(prop = n/sum(n))

hotel_test %>%
    count(children) %>%
    mutate(prop = n/sum(n))

# Split other data into validation and training sets
set.seed(234)
val_set <- rsample::validation_split(hotel_other, 
                            strata = children, 
                            prop = 0.80)

######### MODEL 1: PENALISED LOGISTIC REGRESSION ########
lr_mod <- parsnip::logistic_reg(penalty = tune(), mixture = 1) %>% 
  parsnip::set_engine("glmnet")


# Create recipe
holidays <- c("AllSouls", "AshWednesday", "ChristmasEve", "Easter", 
              "ChristmasDay", "GoodFriday", "NewYearsDay", "PalmSunday")

lr_recipe <- 
  recipe(children ~ ., data = hotel_other) %>% 
  step_date(arrival_date) %>% 
  # create predictors for the yr, month and day of the week
  step_holiday(arrival_date, holidays = holidays) %>% 
  # creates a set of indicator variables for specific holidays
  step_rm(arrival_date) %>% 
  # removes variables
  step_dummy(all_nominal_predictors()) %>% 
  # converts characters / factors into 1+ numeric binary model terms
  step_zv(all_predictors()) %>% 
  # removes indicator variables that only contain a single unique values
  step_normalize(all_predictors())
  # centres & scales numeric variables

# Create workflow
lr_workflow <- 
  workflow() %>% 
  add_model(lr_mod) %>% 
  add_recipe(lr_recipe)

# Create grid for tuning
# Tuning the hyperparameter penalty
lr_reg_grid <- tibble(penalty = 10^seq(-4, -1, length.out = 30))

lr_reg_grid %>% top_n(-5) # lowest penalty values
lr_reg_grid %>% top_n(5)  # highest penalty values

# Train & tune the model
lr_res <- 
  lr_workflow %>% 
  tune_grid(val_set,
            grid = lr_reg_grid,
            control = control_grid(save_pred = TRUE),
            metrics = metric_set(roc_auc))

# Plot AUC 
lr_plot <- 
  lr_res %>% 
  collect_metrics() %>% 
  ggplot(aes(x = penalty, y = mean)) + 
  geom_point() + 
  geom_line() + 
  ylab("Area under the ROC Curve") +
  scale_x_log10(labels = scales::label_number())

lr_plot 

top_models <-
  lr_res %>% 
  show_best(metric = "roc_auc", n = 15) %>% 
  arrange(penalty) 
top_models

#If performance is about the same, we’d prefer to choose a higher penalty value.
lr_best <- 
  lr_res %>% 
  collect_metrics() %>% 
  arrange(penalty) %>% 
  slice(12)
lr_best

lr_auc <- 
  lr_res %>% 
  collect_predictions(parameters = lr_best) %>% 
  roc_curve(children, .pred_children) %>% 
  mutate(model = "Logistic Regression")

autoplot(lr_auc)

############ MODEL 2: TREE BASED ENSEMBLE #########
# Check cores on my machine
cores <- parallel::detectCores()
cores

rf_mod <- 
  rand_forest(mtry = tune(), min_n = tune(), trees = 1000) %>% 
  set_engine("ranger", num.threads = cores) %>% 
  # specifying num.threads enables parallel processing
  set_mode("classification")

rf_recipe <- 
  recipe(children ~ ., data = hotel_other) %>% 
  step_date(arrival_date) %>% 
  step_holiday(arrival_date) %>% 
  step_rm(arrival_date) 

rf_workflow <- 
  workflow() %>% 
  add_model(rf_mod) %>% 
  add_recipe(rf_recipe)

extract_parameter_set_dials(rf_mod)
# mtry: # of predictor variables that each node in the tree sees and can learn about 
#     : ranges from 1 to total # of features present
# min_n: min n to split at any node

set.seed(345)
rf_res <- 
  rf_workflow %>% 
  tune_grid(val_set,
            grid = 25,
            control = control_grid(save_pred = TRUE),
            metrics = metric_set(roc_auc))
# tune_grid determines the upper bound for mtry once data is inputted

rf_res %>% 
  show_best(metric = "roc_auc")

autoplot(rf_res)

rf_best <- 
  rf_res %>% 
  select_best(metric = "roc_auc")
rf_best


rf_res %>% 
  collect_predictions()

rf_auc <- 
  rf_res %>% 
  collect_predictions(parameters = rf_best) %>% 
  roc_curve(children, .pred_children) %>% 
  mutate(model = "Random Forest")

  bind_rows(rf_auc, lr_auc) %>% 
  ggplot(aes(x = 1 - specificity, y = sensitivity, col = model)) + 
  geom_path(lwd = 1.5, alpha = 0.8) +
  geom_abline(lty = 3) + 
  coord_equal() + 
  scale_color_viridis_d(option = "plasma", end = .6)

########## LAST #########
 # the last model
last_rf_mod <- 
  rand_forest(mtry = 8, min_n = 7, trees = 1000) %>% 
  set_engine("ranger", num.threads = cores, importance = "impurity") %>%
  # setting importance as impurity provides variable importance score 
  set_mode("classification")

# the last workflow
last_rf_workflow <- 
  rf_workflow %>% 
  update_model(last_rf_mod)

# the last fit
set.seed(345)
last_rf_fit <- 
  last_rf_workflow %>% 
  last_fit(splits)

last_rf_fit

last_rf_fit %>% 
  collect_metrics()

last_rf_fit %>% 
  extract_fit_parsnip() %>% 
  vip::vip(num_features = 20)

last_rf_fit %>% 
  collect_predictions() %>% 
  roc_curve(children, .pred_children) %>% 
  autoplot()