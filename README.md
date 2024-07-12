# Linear_Regression-Fires
Includes Linear Regression Analysis of climatic variables
# Load necessary libraries
library(ggplot2)
library(dplyr)

# Load the data
data <- read.csv("Correlation1.csv")

# View the first few rows of the data
head(data)
# Summary of the data
summary(data)

# Check for missing values
sum(is.na(data))
# Function to perform linear regression and return the trend
perform_trend_analysis <- function(df, variable) {
  lm_formula <- as.formula(paste(variable, "~ Year"))
  model <- lm(lm_formula, data = df)
  summary(model)
}

# List of variables to analyze
variables <- c("Fire_Occurrences", "Annual_Rainfall", "Wind_Speed", 
               "Specific_Humidity", "Max_Temp", "Min_Temp", 
               "Mean_Diurnal_Range", "Earth_Skin_Temp")

# Apply trend analysis for each variable
trend_results <- lapply(variables, function(var) perform_trend_analysis(data, var))

# Print the trend results
trend_results
# Function to plot trends
plot_trend <- function(df, variable) {
  ggplot(df, aes(x = Year, y = get(variable))) +
    geom_point() +
    geom_smooth(method = "lm", col = "blue") +
    labs(title = paste("Trend Analysis for", variable),
         x = "Year",
         y = variable) +
    theme_minimal()
}

# Plot trends for each variable
plots <- lapply(variables, function(var) plot_trend(data, var))

# Display the plots
for (plot in plots) {
  print(plot)
}
