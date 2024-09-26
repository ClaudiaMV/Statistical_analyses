# Statistical_analyses
---
title: "LME"
author: "Claudia Morales Valiente"
date: "2024-09-26"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

This repository contains the R code for simulating future events based on two main predictors (`predictor1` and `predictor2`). The code performs data cleaning, checks for missing values, fits a series of linear mixed models (MLMs), and conducts post-hoc comparisons in case of interactions.

## Prerequisites

The following R libraries are required for the code to run:

- `dplyr`: Data manipulation
- `janitor`: Data cleaning
- `lme4`: Fitting Linear and Generalized Linear Mixed-Effects Models
- `lmerTest`: Tests in linear mixed-effects models
- `emmeans`: Estimated marginal means and pairwise comparisons

## Code Breakdown

### Data Loading and Cleaning
The dataset (`file_name.csv`) is loaded, and the variable names are cleaned using `janitor::clean_names()` for consistency.

```{r database}
db <- read_csv("file_name.csv")
db <- clean_names(db)
```

### Variable Definitions
The code defines key variables for the analysis:

- **Random effect**: `participant`
- **Predictors**: `predictor1` and `predictor2`
- **Outcome**: `outcome`

```{r variables}
participant <- factor(db$random_variable)
predictor1 <- factor(db$predictor1)
predictor2 <- factor(db$predictor2)
outcome <- db$outcome
```

### Checking for Missing Values
The code checks for missing values in the dataset row-wise and column-wise and prints messages accordingly.

```{r missing values}
any_na <- any(is.na(db))
any_na_row <- apply(db, 1, function(row) any(is.na(row)))
any_na_col <- apply(db, 2, function(col) any(is.na(col)))
```

### Model Building
The following models are fitted using linear mixed models (`lmer`):

- **Model 1**: Null model (intercept only)
- **Model 2**: Main effect of `predictor1`
- **Model 3**: Main effect of `predictor2`
- **Model 4**: Both main effects (`predictor1` and `predictor2`)
- **Model 5**: Interaction between `predictor1` and `predictor2`

```{r model}
model1 <- lmer(outcome ~ 1 + (1|participant), data = db, REML = FALSE)
model2 <- lmer(outcome ~ predictor1 + (1|participant), data = db, REML = FALSE)
model3 <- lmer(outcome ~ predictor2 + (1|participant), data = db, REML = FALSE)
model4 <- lmer(outcome ~ predictor1 + predictor2 + (1|participant), data = db, REML = FALSE)
model5 <- lmer(outcome ~ predictor1 * predictor2 + (1|participant), data = db, REML = FALSE)
```

### Model Comparisons
The `anova` function is used to compare models to determine the effects of the predictors.

```{r anovas}
anova(model4, model3)
anova(model4, model2)
anova(model5, model4)
```

### Post-Hoc Comparisons (in case of interaction)
If an interaction between the predictors is found, post-hoc comparisons are conducted using the `emmeans` package. Results are converted to a data frame and significance is indicated.

```{r pairwise comparisons}
emm_outcome <- emmeans(model5, ~ predictor1 * predictor2)
pairwise_comparisons2 <- pairs(emm_outcome)
adjusted_comparisons2 <- summary(pairwise_comparisons2)
results_df2 <- as.data.frame(adjusted_comparisons2)
results_df2$Significant <- results_df2$p.value < 0.05
print(results_df2)
```

## Usage

1. Ensure the dataset is saved as `file_name.csv` in the appropriate directory.
2. Install the required libraries.
3. Run the script to fit the models and generate results.
4. The script will print model comparison results and indicate significant post-hoc comparisons, if any.
