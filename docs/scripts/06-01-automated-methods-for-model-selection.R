##################################################
### Load libraries
##################################################

library(modelr)
library(patchwork)
library(tidyverse)
library(tidymodels) # Loads broom, rsample, parsnip, recipes, workflow, tune, yardstick, and dials



##################################################
### Import and prepare data
##################################################

usa = read_csv("https://raw.githubusercontent.com/zief0002/symmetrical-fiesta/main/data/states-2019.csv")

# View data
usa


# Create data frame that includes all rows/columns except the state names
usa2 = usa |>
  select(-state)

# View data
head(usa2)



##################################################
### Fit main effects model with all predictors
##################################################

# Use all variables as predictors
lm.all = lm(life_expectancy ~ ., data = usa2)

# Examine output
tidy(lm.all)




##################################################
### Computing model evaluation metrics
##################################################

# Descriptive: Compute SSE and RMS
anova(lm.all)


SSE = 31.668
RMS = 0.7197

# Descriptive: Compute R2 and adj. R2
glance(lm.all)


R2 = 0.379
AdjR2 = 0.280


# Assign values for k and n
k = 8 
n = 52

# Inference: Compute maximum t-value
max(tidy(lm.all)$statistic)

# Inference: Compute minimum p-value
min(tidy(lm.all)$p.value)

# Inference: Compute Mallow's Cp
SSE / RMS + 2 * k - n

# Inference: AIC
aic_mod = n * log(SSE / n) + 2 * k
aic_mod

# Inference: AICc
aic_mod + (2 * (k + 2) * (k + 3)) / (n - k - 3)

# Inference: BIC
n * log(SSE / n) + k * log(n)




##################################################
### Create standardized variables
##################################################

# Create standardized variables after removing state names
z_usa = usa |>
  select(-state) |>
  scale(center = TRUE, scale = TRUE) |>
  data.frame()

# View data
head(z_usa)



##################################################
### Forward selection
##################################################

# Step 0: Fit intercept-only model
# tidy(lm(life_expectancy ~ 1, data = z_usa))


# Step 1: Fit all one-predictor models
tidy(lm(life_expectancy ~ -1 + population, data = z_usa))
tidy(lm(life_expectancy ~ -1 + income,     data = z_usa))
tidy(lm(life_expectancy ~ -1 + illiteracy, data = z_usa))
tidy(lm(life_expectancy ~ -1 + murder,     data = z_usa))
tidy(lm(life_expectancy ~ -1 + hs_grad,    data = z_usa))
tidy(lm(life_expectancy ~ -1 + frost,      data = z_usa))
tidy(lm(life_expectancy ~ -1 + area,       data = z_usa))


# Step 2: Fit all two-predictor models that include income
tidy(lm(life_expectancy ~ -1 + income + population, data = z_usa))
tidy(lm(life_expectancy ~ -1 + income + illiteracy, data = z_usa))
tidy(lm(life_expectancy ~ -1 + income + murder,     data = z_usa))
tidy(lm(life_expectancy ~ -1 + income + hs_grad,    data = z_usa))
tidy(lm(life_expectancy ~ -1 + income + frost,      data = z_usa))
tidy(lm(life_expectancy ~ -1 + income + area,       data = z_usa))


# Step 3: Fit all three-predictor models that include income and population
tidy(lm(life_expectancy ~ -1 + income + population + illiteracy, data = z_usa))
tidy(lm(life_expectancy ~ -1 + income + population + murder,     data = z_usa))
tidy(lm(life_expectancy ~ -1 + income + population + hs_grad,    data = z_usa))
tidy(lm(life_expectancy ~ -1 + income + population + frost,      data = z_usa))
tidy(lm(life_expectancy ~ -1 + income + population + area,       data = z_usa))


# Step 4: Fit all four-predictor models that include income, population, and illiteracy
tidy(lm(life_expectancy ~ -1 + income + population + illiteracy + murder,  data = z_usa))
tidy(lm(life_expectancy ~ -1 + income + population + illiteracy + hs_grad, data = z_usa))
tidy(lm(life_expectancy ~ -1 + income + population + illiteracy + frost,   data = z_usa))
tidy(lm(life_expectancy ~ -1 + income + population + illiteracy + area,    data = z_usa))


# Step 5: Fit all five-predictor models that include income, population, illiteracy, and murder rate
tidy(lm(life_expectancy ~ -1 + income + population + illiteracy + murder + hs_grad, data = z_usa))
tidy(lm(life_expectancy ~ -1 + income + population + illiteracy + murder + frost,   data = z_usa))
tidy(lm(life_expectancy ~ -1 + income + population + illiteracy + murder + area,    data = z_usa))


# Step 6: Fit all six-predictor models that include income, population, illiteracy, murder rate, and frost
tidy(lm(life_expectancy ~ -1 + income + population + illiteracy + murder + frost + hs_grad, data = z_usa))
tidy(lm(life_expectancy ~ -1 + income + population + illiteracy + murder + frost + area,    data = z_usa))


# Step 7: Fit all seven-predictor models that include income, population, illiteracy, murder rate, frost, and hs_grad
tidy(lm(life_expectancy ~ -1 + income + population + illiteracy + murder + frost + area + hs_grad, data = z_usa))



##################################################
### Backward elimination
##################################################

# Step 0: Fit model with all predictors
glance(lm(life_expectancy ~ . - 1,  data = z_usa))$r.squared


# Step 1: Fit all models with one predictor removed
glance(lm(life_expectancy ~ . -1 - population, data = z_usa))$r.squared
glance(lm(life_expectancy ~ . -1 - income,     data = z_usa))$r.squared
glance(lm(life_expectancy ~ . -1 - illiteracy, data = z_usa))$r.squared
glance(lm(life_expectancy ~ . -1 - murder,     data = z_usa))$r.squared
glance(lm(life_expectancy ~ . -1 - hs_grad,    data = z_usa))$r.squared
glance(lm(life_expectancy ~ . -1 - frost,      data = z_usa))$r.squared
glance(lm(life_expectancy ~ . -1 - area,       data = z_usa))$r.squared


# Step 2: Fit all models with hs_grad and one other predictor removed
glance(lm(life_expectancy ~ . -1 - hs_grad - population, data = z_usa))$r.squared
glance(lm(life_expectancy ~ . -1 - hs_grad - income,     data = z_usa))$r.squared
glance(lm(life_expectancy ~ . -1 - hs_grad - illiteracy, data = z_usa))$r.squared
glance(lm(life_expectancy ~ . -1 - hs_grad - murder,     data = z_usa))$r.squared
glance(lm(life_expectancy ~ . -1 - hs_grad - frost,      data = z_usa))$r.squared
glance(lm(life_expectancy ~ . -1 - hs_grad - area,       data = z_usa))$r.squared


# Step 3: Fit all models with hs_grad, area, and one other predictor removed
glance(lm(life_expectancy ~ . -1 - hs_grad - area - population, data = z_usa))$r.squared
glance(lm(life_expectancy ~ . -1 - hs_grad - area - income,     data = z_usa))$r.squared
glance(lm(life_expectancy ~ . -1 - hs_grad - area - illiteracy, data = z_usa))$r.squared
glance(lm(life_expectancy ~ . -1 - hs_grad - area - murder,     data = z_usa))$r.squared
glance(lm(life_expectancy ~ . -1 - hs_grad - area - frost,      data = z_usa))$r.squared


# Step 4: Fit all models with hs_grad, area, frost, and one other predictor removed
glance(lm(life_expectancy ~ . -1 - hs_grad - area - frost - population, data = z_usa))$r.squared
glance(lm(life_expectancy ~ . -1 - hs_grad - area - frost - income,     data = z_usa))$r.squared
glance(lm(life_expectancy ~ . -1 - hs_grad - area - frost - illiteracy, data = z_usa))$r.squared
glance(lm(life_expectancy ~ . -1 - hs_grad - area - frost - murder,     data = z_usa))$r.squared


# Step 5: Fit all models with hs_grad, area, frost, murder rate, and one other predictor removed
glance(lm(life_expectancy ~ . -1 - hs_grad - area - frost - murder - population,  data = z_usa))$r.squared
glance(lm(life_expectancy ~ . -1 - hs_grad - area - frost - murder - income,      data = z_usa))$r.squared
glance(lm(life_expectancy ~ . -1 - hs_grad - area - frost - murder - illiteracy,  data = z_usa))$r.squared


# Step 6: Fit all models with hs_grad, area, frost, murder rate, illiteracy, and one other predictor removed
glance(lm(life_expectancy ~ . -1 - hs_grad - area - frost - murder - illiteracy - population,  data = z_usa))$r.squared
glance(lm(life_expectancy ~ . -1 - hs_grad - area - frost - murder - illiteracy - income,      data = z_usa))$r.squared



##################################################
### Automated forward selection
##################################################

#Load library
library(olsrr)

# Forward selection using AIC
fs_output = ols_step_forward_aic(lm.all, details = TRUE)

# View results from adopted model
fs_output

# Plot results
plot(fs_output)



##################################################
### All-subsets regression
##################################################

# Fit all subsets of predictors
all_output = ols_step_all_possible(lm.all)

# Output
all_output


# Add AICc to results
models = all_output$result |>
  mutate(
    aic_c = aic + (2 * (n + 2) * (n + 3)) / (nrow(z_usa) - n - 3)
  )


# Order from smallest to largest AICc metric
# Only show the best 20 models
models |>
  arrange(aic_c) |>
  head(20)



##################################################
### Select best model(s)
##################################################

# Get models within 4 of minimum AICc
models |>
  select(mindex, n, predictors, aic_c) |>
  filter(aic_c - min(aic_c) < 4) |>
  arrange(aic_c)


# Get best k-predictor models
models |>
  group_by(n) |>
  filter(aic_c == min(aic_c)) |>
  ungroup() |>
  select(mindex, n, predictors, aic_c) |>
  arrange(aic_c)


# Get models within 4 of minimum AICc
plausible = models |>
  select(mindex, n, predictors, aic_c) |>
  filter(aic_c - min(aic_c) < 4) |>
  arrange(aic_c)

# Load library for labeling
library(ggrepel)

# Plot the models
ggplot(data = plausible, aes(x = as.numeric(rownames(plausible)), y = aic_c)) +
  geom_line(group = 1) +
  geom_point() +
  geom_label_repel(aes(label = predictors), size = 3) +
  theme_bw() +
  scale_x_continuous(name = "Ten Best Models", breaks = 1:14) +
  ylab("AICc")

