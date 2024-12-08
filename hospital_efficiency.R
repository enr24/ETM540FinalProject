#working model
library(Benchmarking)
library(dplyr)

# Load the dataset
data <- read.csv("Sampled_Hospital_Data_OR.csv")

# Clean and prepare the data
cleaned_data <- data %>%
  mutate(
    Number.of.Discharges = as.numeric(Number.of.Discharges),
    Predicted.Readmission.Rate = as.numeric(Predicted.Readmission.Rate),
    Score.y = as.numeric(Score.y)
  ) %>%
  filter(
    !is.na(Number.of.Discharges),
    !is.na(Predicted.Readmission.Rate),
    !is.na(Score.y)
  ) %>%
  mutate(Output = 1 - Score.y)

# Prepare inputs and outputs for DEA
inputs <- as.matrix(cleaned_data[, c("Number.of.Discharges", "Predicted.Readmission.Rate")])
outputs <- as.matrix(cleaned_data[, "Output", drop = FALSE])

# Perform DEA analysis with VRS
dea_results <- dea(inputs, outputs, RTS = "crs")

# Get efficiency scores
efficiency_scores <- eff(dea_results)

# Add efficiency scores to the cleaned dataset
cleaned_data$DEA_Efficiency <- efficiency_scores

# Print summary of efficiency scores
summary(efficiency_scores)

# Plot the efficiency scores
hist(efficiency_scores, main = "Distribution of DEA Efficiency Scores", 
     xlab = "Efficiency Score", breaks = 20)

# Identify the most efficient hospitals (efficiency score = 1)
efficient_hospitals <- cleaned_data[cleaned_data$DEA_Efficiency == 1, ]
print(efficient_hospitals[, c("Facility.Name.x", "DEA_Efficiency")])

# Identify the least efficient hospitals (bottom 10%)
least_efficient_hospitals <- cleaned_data[cleaned_data$DEA_Efficiency <= quantile(cleaned_data$DEA_Efficiency, 0.1), ]
print("Least efficient hospitals (bottom 10%):")
print(least_efficient_hospitals[, c("Facility.Name.x", "DEA_Efficiency")])