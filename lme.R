
# Libraries -----------------------------------------------------------------
library(dplyr)
library(janitor)
library(lme4) #main MLM package
library(lmerTest)
library(emmeans)


db <- read_csv("file_name.csv")
db <- clean_names(db)

# Variables ---------------------------------------------------------------
#Random
random1 <- factor(db$random_variable1) #assuming your random variable is called "random_variable" in the database
random2 <- factor(db$random_variable2)

#Predictors
predictor1 <- factor(db$predictor1) #Assuming this predictor variable is called "predictor 1" in the database
predictor2 <- factor(db$predictor2) #Assuming this predictor variable is called "predictor 1" in the database
#Dependent
outcome <- db$outcome #Assuming your outcome variable is called "outcome" in the database

##Check for NA values
any_na <- any(is.na(db))
# Check for NA values row-wise
any_na_row <- apply(db, 1, function(row) any(is.na(row)))
# Check for NA values column-wise
any_na_col <- apply(db, 2, function(col) any(is.na(col)))
if (any_na) {
  print("There are NA values in the matrix.")
} else {
  print("There are no NA values in the matrix.")
}
if (any(any_na_row)) {
  print("There are NA values in one or more rows of the matrix.")
} else {
  print("There are no NA values in any rows of the matrix.")
}
if (any(any_na_col)) {
  print("There are NA values in one or more columns of the matrix.")
} else {
  print("There are no NA values in any columns of the matrix.")
}


        

# Model ----------------------------------------------------------

#Design:
#Model 1a: intercept1  (null) #Check if it's relevant to include intercept 1
#Model 1b: intercept2  (null) #Check if it's relevant to include intercept 2 

#Model 2: main effect of predictor1 + intercepts
#Model 3: main effect of predictor2 + intercepts 
#Model 4: main effect of predictor1 + main effect of predictor2 + intercepts
#Model 5: interaction of predictor1 and predictor2 + intercepts 

model1a <- lmer(output ~ 1 + (1|random1), data = db, REML = FALSE)
model1b <- lmer(output ~ 1 + (1|random2), data = db, REML = FALSE)

model2 <- lmer(output ~ predictor1 + (1|random1) + (1|random2), data = db, REML = FALSE)
model3 <- lmer(output ~ predictor2 + (1|random1) + (1|random2), data = db, REML = FALSE)
model4 <- lmer(output ~ predictor1 + predictor2 + (1|random1) + (1|random2), data = db, REML = FALSE)
model5 <- lmer(output ~ predictor1 * predictor2 + (1|random1) + (1|random2), data = db, REML = FALSE)

## Comparisons ------------------------------------------------------------
anova(model4, model3) #main effect of predictor1
anova(model4, model2) #main effect of predictor2
anova(model5, model4) #interaction


# Post-hoc comparisons in case of interaction---------------------------------------------------------------
library(emmeans)
library(effects)
library(sjPlot)

emm <- emmeans(model5, ~ predictor1 * predictor2) # Obtain EMMs of the model with the interaction
pairwise_comparisons <- pairs(emm)# Perform pairwise comparisons
adjusted_comparisons <- summary(pairwise_comparisons)# Adjust p-values
results_df <- as.data.frame(adjusted_comparisons)# Convert to a data frame for easier manipulation
results_df$Significant <- results_df$p.value < 0.05# Add a column to indicate significance
print(results_df)# Print results with significance indication




