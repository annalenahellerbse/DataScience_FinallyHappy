# Load necessary libraries
library(caret)
library(randomForest)
library(dplyr)
library(ggplot2)
rm(list=ls())


finally_happy_final <- read.csv('https://raw.githubusercontent.com/annalenahellerbse/DataScience_FinallyHappy/refs/heads/main/transformed_finally_happy.csv')

finally_happy_final <- finally_happy_final[finally_happy_final$year != 2005, ]
any(finally_happy_final$year == 2005)  #Check: Should return FALSE

################################################################################
####   Splitting into Training, Validation, and Test Data - Correct one
################################################################################

# Step 1: Drop 'EU' and 'OECD' columns
finally_happy_final <- finally_happy_final %>%
  select(-EU, -OECD)

# Step 2: Create a unique list of countries
set.seed(18) # Set seed for reproducibility
countries <- unique(finally_happy_final$country)

# Step 3: Manually assign Spain and India to the test set
test_countries <- c("Spain", "India")

# Step 4: Randomly split the remaining countries
remaining_countries <- setdiff(countries, test_countries)
n_countries <- length(countries)

# Calculate the desired number of test countries
desired_test_size <- round(n_countries * 0.10)
additional_test_size <- max(desired_test_size - length(test_countries), 0)
additional_test_size <- min(additional_test_size, length(remaining_countries))

# Randomly assign additional countries to the test set
if (additional_test_size > 0) {
  sampled_test_countries <- sample(remaining_countries, size = additional_test_size)
  test_countries <- c(test_countries, sampled_test_countries)
  remaining_countries <- setdiff(remaining_countries, sampled_test_countries)
}

# Calculate the validation set size
val_size <- min(round(n_countries * 0.30), length(remaining_countries))

# Randomly assign countries to the validation set
val_countries <- sample(remaining_countries, size = val_size)
train_countries <- setdiff(remaining_countries, val_countries)

finally_happy_final <- finally_happy_final %>%
  mutate(
    split = case_when(
      country %in% test_countries ~ 0,
      country %in% train_countries ~ 1,
      country %in% val_countries ~ 2,
      TRUE ~ NA_real_
    )
  )

################################################################################
#### Create Training, Validation, and Test Datasets
################################################################################

# Create the training dataset and remove the split column
train_dataset_TH <- finally_happy_final %>%
  filter(split == 1) %>%
  select(-split)

# Create the validation dataset and remove the split column
val_dataset_TH <- finally_happy_final %>%
  filter(split == 2) %>%
  select(-split)

# Create the test dataset and remove the split column
test_dataset_TH <- finally_happy_final %>%
  filter(split == 0) %>%
  select(-split)

################################################################################
#### Normalize Continuous Variables
################################################################################

# Identify continuous variables
continuous_vars <- train_dataset_TH %>%
  select(where(is.numeric)) %>%
  select(-c(year, Africa, Asia, Europe, North_America, South_America, Oceania)) %>%
  colnames()

# Compute mean and standard deviation from the training data
train_means <- sapply(train_dataset_TH[continuous_vars], mean, na.rm = TRUE)
train_sds <- sapply(train_dataset_TH[continuous_vars], sd, na.rm = TRUE)

# Function to scale data using training parameters
scale_data <- function(data, means, sds) {
  data_scaled <- data
  data_scaled[continuous_vars] <- scale(data[continuous_vars], center = means, scale = sds)
  return(data_scaled)
}

# Apply scaling to training, validation, and test datasets
train_dataset_TH <- scale_data(train_dataset_TH, train_means, train_sds)
val_dataset_TH <- scale_data(val_dataset_TH, train_means, train_sds)
test_dataset_TH <- scale_data(test_dataset_TH, train_means, train_sds)

################################################################################
#### Check Splits
################################################################################

cat("Training set size:", nrow(train_dataset_TH), "\n")
cat("Validation set size:", nrow(val_dataset_TH), "\n")
cat("Test set size:", nrow(test_dataset_TH), "\n")

################################################################################
# 1. Data Preparation Final v1 el bueno
################################################################################


# Combine train and validation sets for cross-validation
combined_train <- bind_rows(train_dataset_TH, val_dataset_TH)

# Prepare training data
X_train <- combined_train %>% select(-income_inequ_top10_ag)
y_train <- combined_train$income_inequ_top10_ag

# Prepare test data
X_test <- test_dataset_TH %>% select(-income_inequ_top10_ag)
y_test <- test_dataset_TH$income_inequ_top10_ag

# Combine features and target into one data frame for caret
training_data <- combined_train

# Set seed for reproducibility
set.seed(18)

################################################################################
# 2. Hyperparameter Tuning with Cross-Validation using caret
################################################################################

# Define cross-validation method
train_control <- trainControl(
  method = "cv",
  number = 5,
  verboseIter = TRUE,
  returnResamp = "all"
)

# Define the grid of mtry values to search over
tune_grid <- expand.grid(
  mtry = 1:ncol(X_train)
)

# Train the model using caret's train() function with cross-validation
set.seed(18)
rf_model <- train(
  income_inequ_top10_ag ~ ., 
  data = training_data, 
  method = "rf", 
  trControl = train_control, 
  tuneGrid = tune_grid,
  ntree = 500,
  importance = TRUE
)

# View results
print(rf_model)
plot(rf_model)

# Get optimal mtry value
optimal_mtry <- rf_model$bestTune$mtry
cat("Optimal mtry:", optimal_mtry, "\n")

################################################################################
# 3. Tuning ntree with Cross-Validation
################################################################################

# Define the range of ntree values
ntree_values <- seq(50, 1000, by = 50)

# Initialize a data frame to store results
results <- data.frame()

# Loop through ntree values
for (ntree_val in ntree_values) {
  set.seed(18)
  rf_model <- train(
    income_inequ_top10_ag ~ ., 
    data = training_data, 
    method = "rf", 
    trControl = train_control, 
    tuneGrid = data.frame(mtry = optimal_mtry),
    ntree = ntree_val,
    importance = TRUE
  )
  
  # Extract performance metrics
  best_rmse <- min(rf_model$results$RMSE)
  
  # Store results
  results <- rbind(results, data.frame(ntree = ntree_val, RMSE = best_rmse))
}

# Find the ntree value with the lowest RMSE
best_result <- results[which.min(results$RMSE), ]
optimal_ntree <- best_result$ntree

cat("Optimal ntree:", optimal_ntree, "\n")

# Plot RMSE vs ntree
ggplot(results, aes(x = ntree, y = RMSE)) +
  geom_line(color = "red") +
  geom_point(color = "blue") +
  labs(title = "RMSE vs ntree", x = "Number of Trees (ntree)", y = "Cross-Validated RMSE") +
  theme_minimal()

################################################################################
# 4. Train Final Model with Optimal Hyperparameters
################################################################################

# Train the final model using the optimal hyperparameters
set.seed(18)
rf_final <- randomForest(
  x = X_train,
  y = y_train,
  mtry = optimal_mtry,
  ntree = optimal_ntree,
  importance = TRUE
)

# Display variable importance
var_imp <- importance(rf_final, type = 1)
var_imp <- var_imp[order(var_imp, decreasing = TRUE), , drop = FALSE]
print(var_imp)
varImpPlot(rf_final, type = 1, main = "Variable Importance")

################################################################################
# 5. Evaluate Final Model
################################################################################

# Predictions on the test dataset
RFPred_final <- predict(rf_final, newdata = X_test)

# Calculate evaluation metrics
test_mse <- mean((y_test - RFPred_final)^2)
test_rmse <- sqrt(test_mse)
test_mae <- mean(abs(y_test - RFPred_final))
test_r_squared <- 1 - (sum((y_test - RFPred_final)^2) / sum((y_test - mean(y_test))^2))

cat("Final Test RMSE:", test_rmse, "\n")
cat("Final Test MSE:", test_mse, "\n")
cat("Final Test MAE:", test_mae, "\n")
cat("Final Test R-squared:", test_r_squared, "\n")


# Predictions on the training dataset
RFPred_train <- predict(rf_final, newdata = X_train)

# Calculate evaluation metrics on training data
train_mse <- mean((y_train - RFPred_train)^2)
train_rmse <- sqrt(train_mse)
train_mae <- mean(abs(y_train - RFPred_train))
train_r_squared <- 1 - (sum((y_train - RFPred_train)^2) / sum((y_train - mean(y_train))^2))

cat("Final Training RMSE:", train_rmse, "\n")
cat("Final Training MSE:", train_mse, "\n")
cat("Final Training MAE:", train_mae, "\n")
cat("Final Training R-squared:", train_r_squared, "\n")