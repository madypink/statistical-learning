# List of packages to install
packages <- c("caret", "corrplot", "randomForest", "glmnet", "xgboost", "e1071",
              "Metrics", "dplyr", "readr", "tidyverse", "ggplot2")

# Install packages that are not already installed
new_packages <- packages[!(packages %in% installed.packages()[,"Package"])]
if(length(new_packages)) install.packages(new_packages)

# Load all packages
lapply(packages, library, character.only = TRUE)

# Specify the file path
file_path <- "C:/Users/MAEDEH93/Desktop/supervised/laptops-supervised/laptops.csv"

# Import the dataset
data <- read.csv(file_path, stringsAsFactors = FALSE)

#overview of data

# Count the number of unique values in each column
unique_counts <- sapply(data, function(x) length(unique(x)))

# Print the number of unique values for each column
print(unique_counts)

# Display the first few rows of the data
head(data)
# Summary of the data
summary(data)
# Structure of the data
str(data)
# Missing Values
sum(is.na(data))
colSums(is.na(data))
# Replace blank entries and specific placeholders with NA
data[data == ""] <- NA
data[data == "NA"] <- NA

# Check for missing values in the dataset
missing_values <- colSums(is.na(data))
print(missing_values)
# Calculate the percentage of missing values in the GPU column
gpu_missing_percentage <- sum(is.na(data$GPU)) / nrow(data) * 100

# Step 1: Handle missing values in GPU consistently by filling with "Unknown"
data$GPU[is.na(data$GPU)] <- "Unknown"

#Impute missing values in 'Screen' with the mean
data$Screen[is.na(data$Screen)] <- mean(data$Screen, na.rm = TRUE)

# Calculate the mode of the 'Storage.type' column
mode_Storage.type <- as.character(stats::na.omit(names(sort(table(data$Storage.type), decreasing = TRUE)[1])))

# Impute missing values in 'Storage.type' with the mode
data$Storage.type[is.na(data$Storage.type)] <- mode_Storage.type

# Check for missing values in each column
missing_values <- colSums(is.na(data))

# Display the names of the columns
column_names <- names(data)
print(column_names)
# Boxplot to show Final.Price vs GPU
boxplot(data$Final.Price ~ data$GPU,
        main = "Final Price vs GPU",
        xlab = "GPU",
        ylab = "Final Price",
        col = "lightblue",
        las = 2,  # Rotate x-axis labels for readability
        notch = TRUE)
# Get unique values in the 'GPU' column
unique_gpus <- unique(data$GPU)

# Print the unique values
print(unique_gpus)

# Calculate the median value of 'Storage' excluding zeros
median_value <- median(data$Storage[data$Storage > 0], na.rm = TRUE)

# Replace 0 values in 'Storage' with the calculated median value
data$Storage[data$Storage == 0] <- median_value

#Outliers’ Detection

# Boxplot for RAM
boxplot(data$RAM,
        main = "Boxplot of RAM",
        col = "lightblue",
        ylab = "RAM",
                notch = FALSE)

# Histogram RAM
hist(data$RAM,
     main = "Histogram of RAM",
     xlab = "RAM",
     col = "lightblue",
     border = "black",
     breaks = 100)

count_laptops <- sum(data$RAM > 32)

# Display the count of laptops with RAM greater than 32
print(paste("Number of laptops with RAM greater than 32:", count_laptops))
# Remove laptops with more than 64 GB of RAM
data <- data[data$RAM <= 64, ]
print(max(data$RAM))

# Boxplot for Storage
boxplot(data$Storage,
        main = "Boxplot of Storage",
        col = "lightgreen",
        ylab = "Storage",
        notch = TRUE)
# Histogram for Storage
hist(data$Storage,
     main = "Histogram of Storage",
     xlab = "Storage (GB)",
     col = "lightblue",
     border = "black",
     breaks = 30)  # Adjust the number of bins if necessary

# Count the number of laptops with storage greater than 2000 GB
num_laptops_high_storage <- sum(data$Storage > 2000)

# Print the result
print(num_laptops_high_storage)
# Remove laptops with storage greater than 2000 GB
data <- data[data$Storage <= 2000, ]
print(max(data$Storage))

# Boxplot for Screen
boxplot(data$Screen,
        main = "Boxplot of Screen",
        col = "lightcoral",
        ylab = "Screen",
        notch = TRUE)
# Histogram for Screen
hist(data$Screen,
     main = "Histogram of Screen Size",
     xlab = "Screen Size (inches)",
     col = "lightblue",
     border = "black",
     breaks = 30)  # Adjust the number of bins if necessary
# Count the number of laptops with screen size less than 12 inches
num_laptops_small_screen <- sum(data$Screen < 12)

# Print the result
print(num_laptops_small_screen)

# Boxplot for Final Price
boxplot(data$Final.Price,
        main = "Boxplot of Final Price",
        col = "lightyellow",
        ylab = "Final Price",
        notch = TRUE)

# Histogram for Final.Price
hist(data$Final.Price,
     main = "Histogram of Final Price",
     xlab = "Final Price (Currency Unit)",
     col = "lightblue",
     border = "black",
     breaks = 30)  # Adjust the number of bins if necessary

# Filter and show laptops with Final.Price greater than 5000
laptops_high_price <- data[data$Final.Price > 5000, ]

# Cap the final prices at 5000 units
data$Final.Price <- ifelse(data$Final.Price > 5000, 5000, data$Final.Price)
max(data$Final.Price)
# Assuming 'data' is your dataset
nrows <- nrow(data)

# Display the number of rows
print(paste("Number of rows:", nrows))

# Original Skewness Values
original_skewness <- list(
  RAM = skewness(data$RAM, na.rm = TRUE),
  Storage = skewness(data$Storage, na.rm = TRUE),
  Screen = skewness(data$Screen, na.rm = TRUE),
  Final_Price = skewness(data$Final.Price, na.rm = TRUE)
)

# Apply Transformations
data$RAM <- log(data$RAM + 1)
data$Storage <- sqrt(data$Storage)
data$Screen <- log(max(data$Screen + 1) - data$Screen)
data$Final.Price <- log(data$Final.Price + 1)

# Skewness After Transformation
transformed_skewness <- list(
  RAM = skewness(data$RAM, na.rm = TRUE),
  Storage = skewness(data$Storage, na.rm = TRUE),
  Screen = skewness(data$Screen, na.rm = TRUE),
  Final_Price = skewness(data$Final.Price, na.rm = TRUE)
)

# Print Skewness Before and After Transformation
print("Skewness After Transformation:")(original_skewness)
print("Original Skewness:")
print
print(transformed_skewness)
# Define the numerical variables
numerical_vars <- c("RAM", "Storage", "Screen", "Final.Price")

# Scale the numerical variables and store them in a new data frame
scaled_data <- data.frame(scale(data[numerical_vars]))

# Add the scaled variables back to the original data frame if needed
data[numerical_vars] <- scaled_data
# Set up plotting area for histograms and QQ plots
par(mfrow = c(1,1))  # Arrange plots in a 1 grid (Histogram, QQ plot)
head (data[numerical_vars])
# Loop through each variable to plot histogram, QQ plot, and perform Shapiro-Wilk test
for (var in numerical_vars) {
  # Plot histogram
  hist(data[[var]],
       main = paste("Histogram of", var),
       xlab = var,
       col = "lightblue",
       border = "black",
       breaks = 30)

  # Plot QQ plot
  qqnorm(data[[var]], main = paste("QQ Plot of", var))
  qqline(data[[var]], col = "red")

  # Perform Shapiro-Wilk test
  shapiro_test <- shapiro.test(data[[var]])
  cat("Shapiro-Wilk test for", var, ": W =", shapiro_test$statistic, ", p-value =", shapiro_test$p.value, "\n")
}



#Multicollinearity between numerical features
# Compute the correlation matrix
cor_matrix <- cor(data[, sapply(data, is.numeric)])

# Visualize the correlation matrix using ggplot2
install.packages("psych")
install.packages("ggcorrplot")
# Load necessary libraries
library(dplyr)
library(ggcorrplot)
library(psych)


# Select these variables from your dataset
mydata <- data %>% select(all_of(numerical_vars))

# Compute the correlation matrix
cor_matrix <- cor(mydata)

# Print the rounded correlation matrix
print(round(cor_matrix, 2))


# Create the pairs panel plot
pairs.panels(data[numerical_vars],
             method = "pearson",  # Correlation method
             hist.col = "cyan",   # Histogram color
             density = TRUE,      # Show density plots
             ellipses = TRUE,     # Add correlation ellipses
             main = "Correlation Matrix with Scatter Plots and Density Plots")
library(car)
vif_model <- lm(Final.Price ~ RAM + Storage + Screen, data = mydata)
vif(vif_model)

# Load necessary libraries
library(ggplot2)


# Convert character variables to factors
categorical_vars <- c("Status", "Brand", "Model", "CPU", "Storage.type", "GPU", "Touch")
data[categorical_vars] <- lapply(data[categorical_vars], as.factor)

# Create bar plots for each categorical variable
for (var in categorical_vars) {
  p <- ggplot(data, aes_string(x = var)) +
    geom_bar(fill = "skyblue", color = "black") +
    labs(title = paste("Bar Plot of", var), x = var, y = "Frequency") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),  # Rotate x-axis labels for readability
          plot.title = element_text(hjust = 0.5))

  print(p)  # Print the plot
}
numerical_vars <- c("RAM", "Storage", "Screen", "Final.Price")


# Create scatter plots for each numerical variable against Final Price
for (var in numerical_vars) {
  p <- ggplot(data, aes_string(x = var, y = "Final.Price")) +
    geom_point(color = "blue") +
    geom_smooth(method = "lm", color = "red") +  # Add a linear regression line
    labs(title = paste("Relationship between", var, "and Final Price"),
         x = var,
         y = "Final Price") +
    theme_minimal()

  print(p)  # Explicitly print the plot
}


# Create box plots for each categorical variable against Final Price
for (var in categorical_vars) {
  p <- ggplot(data, aes_string(x = var, y = "Final.Price")) +
    geom_boxplot(fill = "lightblue") +
    labs(title = paste("Relationship between", var, "and Final Price"),
         x = var,
         y = "Final Price") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels

  print(p)  # Explicitly print the plot
}
#Encoding categorical variables
# Encoding Touch
data$Touch_encoded <- ifelse(data$Touch == "Yes", 1, 0)

# Encoding Status
data$Status_encoded <- ifelse(data$Status == "New", 0, 1)

# Encoding Storage.type (if it has an ordinal nature)
data$Storage.type_encoded <- ifelse(data$Storage.type == "SSD", 1, 0)

# View the encoded values
head(data[, c("Storage.type", "Storage.type_encoded")])
# Encoding Brand
data$Brand <- as.factor(data$Brand)
data$Brand_encoded <- as.numeric(data$Brand)

# Encoding Model
data$Model <- as.factor(data$Model)
data$Model_encoded <- as.numeric(data$Model)

# Encoding CPU
data$CPU <- as.factor(data$CPU)
data$CPU_encoded <- as.numeric(data$CPU)

# Encoding GPU
data$GPU <- as.factor(data$GPU)
data$GPU_encoded <- as.numeric(data$GPU)

# View the encoded values
head(data[, c("Brand", "Brand_encoded", "Model", "Model_encoded", "CPU", "CPU_encoded", "GPU", "GPU_encoded")])
str(data)
# Select the features and target variable
features <- c("RAM", "Storage", "Screen", "Touch_encoded", "Status_encoded", "Storage.type_encoded",
              "Brand_encoded", "Model_encoded", "CPU_encoded", "GPU_encoded")
target <- "Final.Price"

# Set a seed for reproducibility
set.seed(123)


# Combine features and target
data_subset <- data[, c(features, target)]

# Set seed for reproducibility
set.seed(123)

# Split the data into training and testing sets (80% training, 20% testing)
trainIndex <- sample(1:nrow(data_subset), size = 0.8 * nrow(data_subset))

# Create training and testing datasets
trainData <- data_subset[trainIndex, ]
testData  <- data_subset[-trainIndex, ]

# Check dimensions of the resulting datasets
dim(trainData)
dim(testData)

# Features and target for training and testing
trainX <- trainData[, features]
trainY <- trainData[[target]]
testX <- testData[, features]
testY <- testData[[target]]
# Linear Regression
lm_model <- lm(trainY ~ ., data = trainX)
summary(lm_model)
install.packages("lmtest")

# Make predictions with test data
predictions <- predict(lm_model, newdata = testX)

# Evaluate the model
mse <- mean((predictions - testY)^2)
rmse <- sqrt(mse)
r_squared <- cor(predictions, testY)^2

cat("Linear Regression - Mean Squared Error (MSE):", mse, "\n")
cat("Linear Regression - Root Mean Squared Error (RMSE):", rmse, "\n")
cat("Linear Regression - R-squared Value:", r_squared, "\n")

# Visualize the predictions
results <- data.frame(Actual_data = testY, Predicted_data = predictions)
ggplot(results, aes(x = Actual_data, y = Predicted_data)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, color = "red") +
  labs(x = "Actual Data", y = "Predicted Data") +
  ggtitle("Linear Regression - Actual vs Predicted Prices")
par(mfrow = c(1, 1))  # Reset to a 1x1 layout

# Residual plot and its normality test
# Calculate residuals
residuals <- testY - predictions


# Histogram and Q-Q plot of the residuals
hist(residuals, main = "Histogram of Residuals")
qqnorm(residuals)
qqline(residuals)
# Shapiro-Wilk test for normality
shapiro.test(residuals)

# Durbin-Watson test for autocorrelation
library(lmtest)
dw_test <- dwtest(lm_model)
print(dw_test)


# Load necessary library
library(e1071)

# Fit the SVM model
svm_model <- svm(trainY ~ ., data = trainX, type = 'eps-regression', kernel = "radial")

# Make predictions on the test data
y_pred_svm <- predict(svm_model, testX)

# Evaluate the model
mse_svm <- mean((y_pred_svm - testY)^2)
rmse_svm <- sqrt(mse_svm)
r_squared_svm <- cor(y_pred_svm, testY)^2

# Output the evaluation metrics
cat("SVM Regression - Mean Squared Error (MSE):", mse_svm, "\n")
cat("SVM Regression - Root Mean Squared Error (RMSE):", rmse_svm, "\n")
cat("SVM Regression - R-squared:", r_squared_svm, "\n")

# Load necessary library
library(randomForest)

# Set a seed for reproducibility
set.seed(123)

# Fit the Random Forest model
regressor_rf <- randomForest(trainY ~ ., data = trainX, ntree = 1000)

# Make predictions on the test data
y_pred_rf <- predict(regressor_rf, testX)

# Evaluate the model
mse_rf <- mean((y_pred_rf - testY)^2)
rmse_rf <- sqrt(mse_rf)
r_squared_rf <- cor(y_pred_rf, testY)^2

# Output the evaluation metrics
cat("Random Forest Regression - Mean Squared Error (MSE):", mse_rf, "\n")
cat("Random Forest Regression - Root Mean Squared Error (RMSE):", rmse_rf, "\n")
cat("Random Forest Regression - R-squared:", r_squared_rf, "\n")
# Load necessary library
library(gbm)

# Fit the Gradient Boosting model
gbm_model <- gbm(
  formula = trainY ~ .,
  data = trainX,
  distribution = "gaussian",
  n.trees = 1000,
  interaction.depth = 4,
  cv.folds = 10,
  shrinkage = 0.01,
  verbose = FALSE
)

# Determine the best number of iterations using cross-validation
best_iter <- gbm.perf(gbm_model, method = "cv")

# Make predictions on the test data
y_pred_gbm <- predict(gbm_model, newdata = testX, n.trees = best_iter)

# Evaluate the model
mse_gbm <- mean((y_pred_gbm - testY)^2)
rmse_gbm <- sqrt(mse_gbm)
r_squared_gbm <- cor(y_pred_gbm, testY)^2

# Output the evaluation metrics
cat("Gradient Boosting Regression - Mean Squared Error (MSE):", mse_gbm, "\n")
cat("Gradient Boosting Regression - Root Mean Squared Error (RMSE):", rmse_gbm, "\n")
cat("Gradient Boosting Regression - R-squared Value:", r_squared_gbm, "\n")


#unsupervised learning (clustering)
# Load necessary libraries
library(dplyr)
library(cluster)
library(ggplot2)

# Step 1: Define the dataset for clustering

# Select relevant features
features <- c("RAM", "Storage", "Screen", "Touch_encoded", "Status_encoded",
              "Storage.type_encoded", "Brand_encoded", "Model_encoded",
              "CPU_encoded", "GPU_encoded")
colnames(data)

# Create a new data with selected features
data_for_clustering <- data[features]

# Scale the data
data_for_clustering_scaled <- scale(data_for_clustering)
str(data_for_clustering)
library(cluster)
# Run the Elbow Method on the scaled data to find optimal k
wssplot(data_for_clustering_scaled, max_k = 10)
# Function to calculate average silhouette width for different values of k
silhouette_method <- function(data, max_k=10){
  silhouette_avg <- numeric(max_k)

  for (k in 2:max_k){
    km_res <- kmeans(data, centers=k, nstart=25)
    sil <- silhouette(km_res$cluster, dist(data))
    silhouette_avg[k] <- mean(sil[, 3])
  }

  plot(2:max_k, silhouette_avg[2:max_k], type="b", pch = 19,
       xlab="Number of Clusters (k)",
       ylab="Average Silhouette Width",
       main="Silhouette Method for Optimal k")
}

silhouette_method(data_for_clustering_scaled, max_k=10)
# Load necessary library
library(ggplot2)


# Perform K-Means clustering with k = 4
set.seed(123)
kmeans_result <- kmeans(data_for_clustering_scaled, centers=4, nstart=25)

# Add cluster assignments to the original dataset
data$KMeans_Cluster <- kmeans_result$cluster

# Print the K-Means clustering results
print(kmeans_result)

# Install the factoextra package if it's not already installed
install.packages("factoextra")

# Load the factoextra package
library(factoextra)

# Visualize the K-Means clusters
library(ggplot2)
fviz_cluster(kmeans_result, data = data_for_clustering_scaled,
             geom = "point", ellipse.type = "norm", main = "K-Means Clustering")



# Visualize RAM distribution across clusters
ggplot(data, aes(x = as.factor(KMeans_Cluster), y = RAM, fill = as.factor(KMeans_Cluster))) +
  geom_boxplot() +
  labs(x = "Cluster", y = "RAM", title = "RAM Distribution by Cluster")

# Visualize Storage distribution across clusters
ggplot(data, aes(x = as.factor(KMeans_Cluster), y = Storage, fill = as.factor(KMeans_Cluster))) +
  geom_boxplot() +
  labs(x = "Cluster", y = "Storage (GB)", title = "Storage Distribution by Cluster")

# Visualize Screen Size distribution across clusters
ggplot(data, aes(x = as.factor(KMeans_Cluster), y = Screen, fill = as.factor(KMeans_Cluster))) +
  geom_boxplot() +
  labs(x = "Cluster", y = "Screen Size (inches)", title = "Screen Size Distribution by Cluster")


# Visualize Final Price distribution across clusters
ggplot(data, aes(x = as.factor(KMeans_Cluster), y = Final.Price, fill = as.factor(KMeans_Cluster))) +
  geom_boxplot() +
  labs(x = "Cluster", y = "Final.Price", title = "Final Price Distribution by Cluster")


# Calculate the distance matrix using Euclidean distance
distance_matrix <- dist(data_for_clustering_scaled, method = "euclidean")

# Perform hierarchical clustering using Ward's method
hclust_result <- hclust(distance_matrix, method = "ward.D2")

# Plot the dendrogram to visualize the hierarchical clustering
# Set graphical parameters for better visibility
par(mar = c(5, 4, 4, 2) + 0.1)  # Adjust margins

# Plot the dendrogram 
plot(hclust_result, 
     main = "Hierarchical Clustering Dendrogram",
     xlab = "Laptops", 
     sub = "", 
     cex = 0.6,        # Reduce text size for readability
     hang = -1,        # Aligns leaves at the same height
     labels = FALSE)   # Hide labels if they overlap

#adding color to the dendrogram clusters 
rect.hclust(hclust_result, k = 4, border = 2:5)  # Draw rectangles around clusters


# Cut the tree to create 4 clusters
cutree_result <- cutree(hclust_result, k = 4)

# Add the hierarchical cluster assignments to the original dataset
data$Hierarchical_Cluster <- cutree_result

# Check the first few rows to ensure clustering worked
head(data)

# Visualize the Hierarchical clusters
fviz_cluster(list(data = data_for_clustering_scaled, cluster = cutree_result),
             geom = "point", ellipse.type = "norm", main = "Hierarchical Clustering")

# Compare the clusters by creating a contingency table
table(KMeans_Cluster = data$KMeans_Cluster, Hierarchical_Cluster = data$Hierarchical_Cluster)

#RAM Distribution by Cluster (Hierarchical Clustering)

ggplot(data, aes(x = as.factor(Hierarchical_Cluster), y = RAM, fill = as.factor(Hierarchical_Cluster))) +
  geom_boxplot() +
  labs(x = "Cluster", y = "RAM", title = "RAM Distribution by Cluster (Hierarchical Clustering)")

#Storage Distribution by Cluster (Hierarchical Clustering)
ggplot(data, aes(x = as.factor(Hierarchical_Cluster), y = Storage, fill = as.factor(Hierarchical_Cluster))) +
  geom_boxplot() +
  labs(x = "Cluster", y = "Storage (GB)", title = "Storage Distribution by Cluster (Hierarchical Clustering)")

#Screen Distribution by Cluster (Hierarchical Clustering)

ggplot(data, aes(x = as.factor(Hierarchical_Cluster), y = Screen, fill = as.factor(Hierarchical_Cluster))) +
  geom_boxplot() +
  labs(x = "Cluster", y = "Screen Size (inches)", title = "Screen Size Distribution by Cluster (Hierarchical Clustering)")

#Final.Price Distribution by Cluster (Hierarchical Clustering)

ggplot(data, aes(x = as.factor(Hierarchical_Cluster), y = Final.Price, fill = as.factor(Hierarchical_Cluster))) +
  geom_boxplot() +
  labs(x = "Cluster", y = "Final Price", title = "Final Price Distribution by Cluster (Hierarchical Clustering)")

