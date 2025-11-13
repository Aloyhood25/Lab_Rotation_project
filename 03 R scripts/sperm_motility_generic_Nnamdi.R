

# Generic Sperm Motility Analysis Script

#clear R's brain
rm(list=ls())
#Load Required Packages

library(tidyverse)
library(broom)
library(ggplot2)
library(dplyr)

#Load and Inspect my_data
#----------------------------#
my_data <- read.csv('01 Raw data/sperm_age vs SF_age.csv')
head(my_data)
str(my_data)

# Clean column names
names(my_data) <- make.names(names(my_data))

cat("Variables detected:\n")
print(names(my_data))

#----------------------------#
#Identify Response Variable
#----------------------------#
response_candidates <- grep("sperm.*motil", names(my_data), ignore.case = TRUE, value = TRUE)
if (length(response_candidates) == 0) stop("No sperm motility variable found.")
response_var <- response_candidates[1]
cat("Detected response variable:", response_var, "\n")

#----------------------------#
#Identify Predictors and ID (Keyword-based)
#----------------------------#

predictors <- setdiff(names(my_data), response_var)

# Automatically detect ID column
id_col <- grep("id", names(my_data), ignore.case = TRUE, value = TRUE)
if (length(id_col) > 0) {
  id_col <- id_col[1]
  my_data[[id_col]] <- as.factor(my_data[[id_col]])
  cat("Detected ID column:", id_col, "\n")
} else {
  warning("No ID column detected — random effects will not be used.")
  id_col <- NULL
}

# Exclude ID column from predictors for fixed effects
predictors <- predictors[predictors != id_col]

#----------------------------#
# Keyword-Based Variable Detection
#----------------------------#

# Define keyword lists for different biological variables
keyword_list <- list(
  treatment  = c("treatment_combination","medium", "condition", 'treatment'),
  age        = c("male_age,'sperm_age", "age"),
  species    = c("species"),
  temperature = c("temp", "temperature"),
  time       = c("time_factor", "time"),
  concentration = c("conc", "concentration", "density"),
  id         = c("id", "male_ID", "subject"
))

# Function to search for variable name based on keyword list
detect_var <- function(keywords, data_names) {
  vars <- unlist(lapply(keywords, function(k) grep(k, data_names, ignore.case = TRUE, value = TRUE)))
  unique(vars)
}

# Apply detection for each variable type
treatment_var <- detect_var(keyword_list$treatment, names(my_data))
age_var <- detect_var(keyword_list$age, names(my_data))
species_var <- detect_var(keyword_list$species, names(my_data))
temp_var <- detect_var(keyword_list$temperature, names(my_data))
time_var <- detect_var(keyword_list$time, names(my_data))
conc_var <- detect_var(keyword_list$concentration, names(my_data))
id_var <- detect_var(keyword_list$id, names(my_data))

# Display what was found
cat("\nKeyword-based variable detection:\n")
cat("Treatment variable(s):", treatment_var, "\n")
cat("Age variable(s):", age_var, "\n")
cat("Species variable(s):", species_var, "\n")
cat("Temperature variable(s):", temp_var, "\n")
cat("Time variable(s):", time_var, "\n")
cat("Concentration variable(s):", conc_var, "\n")
cat("ID variable(s):", id_var, "\n")


#----------------------------#
# Combine detected variables
#----------------------------#
categorical_vars <- unique(c(treatment_var, species_var, age_var, time_var, id_var))
numeric_vars <- unique(c(temp_var, conc_var))

time_var
age_var
species_var
# Fallback if nothing detected by keywords    
if (length(categorical_vars) == 0) {
  categorical_vars <- predictors[sapply(my_data[predictors], function(x) is.character(x) || is.factor(x))]
}
if (length(numeric_vars) == 0) {
  numeric_vars <- predictors[sapply(my_data[predictors], is.numeric)]
}

# Convert categorical variables to factors
my_data[categorical_vars] <- lapply(my_data[categorical_vars], as.factor)

cat("\nFinal variable classification:\n")
cat("Categorical predictors:", categorical_vars, "\n")
cat("Numeric predictors:", numeric_vars, "\n")

#----------------------------#
# 5. Create Results Directory
#----------------------------#
dir.create("04 R plots2", showWarnings = FALSE)

#----------------------------#
# 6. Visualization with ggplot2
#----------------------------#
cat("Generating exploratory plots...\n")

# Example: Plot response vs each predictor
if (length(time_var) > 0) {
  for (v in time_var) {
    p <- ggplot(my_data, aes_string(x = v, y = response_var, color = treatment_var)) +
      geom_point(aes(group = treatment_var), alpha = 0.6, size = 2) +
      theme_minimal(base_size = 14) +
      labs(
        title = paste("Sperm motility vs", v, "(colored by treatment)"),
        x = "Time(mins)", y = response_var, color = "Treatments"
      )
    
  }
}
p

#For data with age variable, treatment variable, and time variable

if (length(time_var) > 0) {
  for (v in time_var) {
    p2 <- ggplot(my_data, aes_string(x = v, y = response_var, color = treatment_var)) +
      geom_point(aes(group = treatment_var), alpha = 0.6, size = 2) +
      theme_minimal(base_size = 14) +
      facet_wrap(as.formula(paste("~", age_var[1])), scales = "free_y") +
      labs(
        title = paste("Sperm motility vs", v, "(colored by treatment)"),
        x = "Time(mins)", y = response_var, color = "Treatments"
      )
    
  }
}
p2

#calculate mean sperm motility according to the available variables in the dataset

#create group means using detected predictor variables
general_mean_SM <- my_data %>%
  group_by(!!sym(age_var[1]), !!sym(treatment_var[1]), !!sym(time_var[1])) %>%
  summarise(
    mean_SM = mean(!!sym(response_var), na.rm = TRUE),
    se_SM = sd(!!sym(response_var)) / sqrt(n())
  )

glimpse(general_mean_SM)
levels(general_mean_SM[[time_var[1]]]) <- c("0", "2", "4", "6")



#Plot mean sperm motility over time variable, age variable, and treatment variables 
ggplot() +
  geom_point(data = general_mean_SM, aes_string(x = time_var[1], y = "mean_SM",
           color = treatment_var[1]), size = 2) +
  facet_wrap(as.formula(paste("~", age_var[1]))) +
  geom_line(data = general_mean_SM, aes_string(x = time_var[1], y = "mean_SM",
           group = treatment_var[1], color = treatment_var[1]),
           linewidth = 0.5,
           alpha = 0.5) +
  theme_minimal() +
  geom_errorbar(data = general_mean_SM,
                aes_string(x = time_var[1], y = "mean_SM",
                           ymin = "mean_SM - se_SM",
                           ymax = "mean_SM + se_SM",
                           color = treatment_var[1]), width = 0.1) +
  labs(title = "Sperm Motility Over Time by Age Group and Treatment",
       x = "Time Points (min)",
       y = response_var,
       color = "Treatment")

cat("Running mixed-effects model...\n")

if (length(predictors) == 1) {
  fixed_formula <- predictors[1]
} else {
  fixed_formula <- paste(predictors[1:min(2, length(predictors))], collapse = " * ")
}

if (!is.null(id_col)) {
  formula <- as.formula(paste(response_var, "~", fixed_formula, "+ (1|", id_col, ")"))
} else {
  formula <- as.formula(paste(response_var, "~", fixed_formula))
}

if (!is.null(id_col)) {
  model <- lmer(formula, data = my_data, REML = FALSE)
  model_type <- "Linear Mixed-Effects Model"
} else {
  model <- lm(as.formula(paste(response_var, "~", fixed_formula)), data = my_data)
  model_type <- "Linear Model (no random effects)"
}

cat("Model type:", model_type, "\n")
print(summary(model))
vcov(model)



summary_df <- tidy(model, effects = "fixed")
write.csv(summary_df, "results/model_summary.csv", row.names = FALSE)

#----------------------------#
# 8. Wrap up
#----------------------------#
cat("Analysis complete.\nResults and plots saved in 'results/' folder.\n")
