library(dplyr)
library(xgboost)
library(caret)

trimmed_aquasat <- read.csv("Data/prelim_aquasat.csv")

#Subset data to just predictor variables
trimmed_aquasat <- trimmed_aquasat %>% 
  dplyr::select(c(red, green, blue, nir, swir1, swir2, secchi))

#Create partition of data for training and testing (80/20)
trainIndex <- createDataPartition(trimmed_aquasat$secchi, p = 0.8, list = FALSE, times = 1)
train <- trimmed_aquasat[trainIndex, ]
test <- trimmed_aquasat[-trainIndex, ]

#Remove actual secchi from the dataframe, convert to matrix
train_matrix <- as.matrix(train %>% dplyr::select(-secchi))
test_matrix <- as.matrix(test %>% dplyr::select(-secchi))

#Isolate actual secchi in it's own element
train_label <- train$secchi
test_label <- test$secchi

# Create DMatrix objects for training and testing
dtrain <- xgb.DMatrix(data = train_matrix, label = train_label)
dtest <- xgb.DMatrix(data = test_matrix, label = test_label)

# Set parameters for XGBoost
params <- list(
  objective = "reg:squarederror",  # For regression
  eval_metric = "rmse",            # Root Mean Squared Error
  eta = 0.1,                       # Learning rate
  max_depth = 6,                   # Maximum depth of the trees
  subsample = 0.8,                 # Subsample ratio of the training instances
  colsample_bytree = 0.8           # Subsample ratio of columns when constructing each tree
)

xgb_model <- xgb.train(
  params = params,
  data = dtrain,
  nrounds = 100,          # Number of boosting rounds
  watchlist = list(train = dtrain, test = dtest),  # To monitor the training and test error
  early_stopping_rounds = 10  # Stop early if the test error doesn't improve
)

# Make predictions
predictions <- predict(xgb_model, test_matrix)

plot(test_label, predictions)
# Calculate RMSE (Root Mean Squared Error)
rmse <- sqrt(mean((predictions - test_label)^2))
print(paste("RMSE: ", rmse))

# Optional: Plot feature importance
importance <- xgb.importance(feature_names = colnames(train_matrix), model = xgb_model)
xgb.plot.importance(importance)
