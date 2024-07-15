##### CORRELATION #####
#--Overall - between predictors
grid_by_n_num <- grid_by_n_dist |>
    st_drop_geometry() |>
    select(-row.id)

cm <- cor(grid_by_n_num)
high_cm <- cm
high_cm[abs(high_cm) < 0.8] <- 0
corrplot(high_cm, tl.col = "black", tl.cex = 0.8)

find_high_cor <- function(high_cm){
    # Find pairs of row names and column names for cells with values greater than 0.8 but less than 1
    pairs <- which(abs(high_cm) > 0.8 & abs(high_cm) < 1, arr.ind = TRUE)

    # Filter out the diagonal elements where row index is equal to column index
    pairs <- pairs[pairs[, 1] != pairs[, 2], ]

    # Create a dataframe of these pairs with row names, column names, and correlation values
    result <- data.frame(
    Var1 = rownames(high_cm)[pairs[, 1]],
    Var2 = colnames(high_cm)[pairs[, 2]],
    Correlation = high_cm[pairs]
    ) |>
    arrange(Correlation) 

    result <- result[seq(2, nrow(result), 2),]
    return(result)
}

high_cor <- find_high_cor(high_cm)
map(poi_bnt_fin, ~nrow(.x)) |> as.data.frame() |> sort()
high_cor |> 
    arrange(Var1)

# post_depot-lawyer-post_office: post_office
# hosuseware-vet-warehouse: warehouse
# electronics-car_repair-fuel-garages-money_exchange_transfer: garages


#--Overall - between predictors and outcome
n_cm <- cm[dimnames(cm)[[1]] == "n",]
n_cm |> sort()

######### LINEAR REGRESSION #########
#--With all
grid_by_n_dist |> str()
pred <- names(grid_by_n_dist)[!names(grid_by_n_dist) %in% c("row.id", "n", "x")]
fx <- as.formula(paste0("n ~ ", paste(pred, collapse = " + ")))
lm1 <- lm(fx, data = grid_by_n_dist)
summary(lm1)
par(mfrow = c(2, 2))
plot(lm1)

outliers <- abs(rstudent(lm1)) > 3
lm1_no_outliers <- lm(fx, data = grid_by_n_dist[!outliers, ])
summary(lm1_no_outliers)
plot(lm1_no_outliers)

lmtest::bptest(lm1_no_outliers)
    #reject H0 that there is no heteroscedasticity
    #there is unequal variance in the residuals

wls <- lm(fx, data = grid_by_n_dist[!outliers, ], weights = 1/fitted(lm1_no_outliers)^2)
summary(wls)
plot(wls)


grid_by_n_log <- grid_by_n_dist[!outliers, ] |>
    filter(n != 0)
fx_log <- as.formula(paste0("log(n) ~ ", paste(pred, collapse = " + ")))
lm_log <- lm(fx_log, data = grid_by_n_log)
summary(lm_log)

#--With highly correlated variables excluded
pred2 <- names(grid_by_n_dist)[!names(grid_by_n_dist) %in% c("row.id", "n", "x", "post_depot", "lawyer", "houseware", "veterinary", "electronics", "car_repair", "fuel", "money_exchange_transfer")]
fx2 <- as.formula(paste0("n ~ ", paste(pred2, collapse = " + ")))
lm2 <- lm(fx2, data = grid_by_n_dist)
summary(lm2)
plot(lm2)

lm2_outliers <- abs(rstudent(lm2)) > 3
lm2_no_outliers <- lm(fx, data = grid_by_n_dist[!lm2_outliers, ])
summary(lm1_no_outliers)
plot(lm2_no_outliers)

#--With pcs 
pca <- caret::preProcess(grid_by_n_num[names(grid_by_n_num)!= "n"], method = 'pca', pcaComp = 4)
loadings <- pca$rotation
for (i in 1:ncol(loadings)) {
  cat("Principal Component", i, ":\n")
  print(sort(abs(loadings[, i]), decreasing = TRUE))
  cat("\n")
}
    #PC1: accountant, restaurant, bicycle_parking, atm, car_repair
    #PC2: bridge, alcohol, computer, lawyer, electronics
    #PC3: clinic, doctors, bar, houseware, lawyer
    #PC4: grave_yard, garage, bar, post_depot, warehouse

pca_data <- predict(pca, grid_by_n_num)
grid_by_n_pca <- cbind(grid_by_n, pca_data)
lm_pca <- lm(n ~ PC1 + PC2 + PC3 + PC4 + (PC1 + PC2 + PC3 + PC4)^2, data = grid_by_n_pca)
summary(lm_pca)

lm_pca_outliers <- abs(rstudent(lm_pca)) > 3
lm_pca_no_outliers <- lm(n ~ PC1 + PC2 + PC3 + PC4 + (PC1 + PC2 + PC3 + PC4)^2, data = grid_by_n_pca[!lm_pca_outliers, ])
summary(lm_pca_no_outliers)

lm_pca_no_outliers2 <- lm(n ~ PC1 + PC2 + PC3 + PC4 + PC1:PC4 + PC3:PC4, data = grid_by_n_pca[!lm_pca_outliers, ])
summary(lm_pca_no_outliers2)

# Load necessary libraries
library(dplyr)
library(car)
library(MASS)
library(corrplot)
library(glmnet)

# Assuming grid_by_n_dist is your data frame
# Check for missing values
sum(is.na(grid_by_n_dist))

# Handling missing values (if any)
grid_by_n_dist <- na.omit(grid_by_n_dist)

# Log transform crime count to handle skewness (if necessary)
grid_by_n_dist$n <- log1p(grid_by_n_dist$n)

# Calculate correlation matrix
cor_matrix <- cor(grid_by_n_dist %>% select(-row.id, -x))

# Plot correlation matrix to visualize high correlations
corrplot(cor_matrix, method = "color", tl.cex = 0.8)

pred <- names(grid_by_n_dist)[!names(grid_by_n_dist) %in% c("row.id", "n", "x")]
fx <- as.formula(paste0("n ~ ", paste(pred, collapse = " + ")))

# Use stepwise regression to identify important predictors
initial_model <- lm(fx, data = grid_by_n_dist)
step_model <- stepAIC(initial_model, direction = "both")
summary(step_model)
lm <- step_model$call

# Building a model with interaction terms and polynomial terms
interaction_poly_model <- lm(n ~ poly(pub, 2) * poly(car_wash, 2) + 
                                doctors * place_of_worship + 
                                post_depot * bridge + 
                                garage * car + 
                                atm * veterinary + 
                                poly(post_secondary, 2) * clothes, 
                             data = grid_by_n_dist)
summary(interaction_poly_model)

# Prepare data for glmnet
x <- model.matrix(fx, data = grid_by_n_dist)[, -1]
y <- grid_by_n_dist$n

# Fit Lasso model
lasso_cv <- cv.glmnet(x, y, alpha = 1)
best_lambda <- lasso_cv$lambda.min
lasso_model <- glmnet(x, y, alpha = 1, lambda = best_lambda)
lasso_coefs <- coef(lasso_model)
print(lasso_coefs)

# Extract the names of significant variables from stepwise regression
significant_vars <- names(step_model$coefficients)[-1] # Remove the intercept

# Combine the significant variables with interaction and polynomial terms explicitly
final_formula <- as.formula(paste("n ~ poly(pub, 2) * poly(car_wash, 2) +",
                                  "doctors * place_of_worship +",
                                  "post_depot * bridge +",
                                  "garage * car +",
                                  "atm * veterinary +",
                                  "poly(post_secondary, 2) * clothes +",
                                  paste(significant_vars, collapse = " + ")))

# Build the final model
final_model <- lm(final_formula, data = grid_by_n_dist)
summary(final_model)

