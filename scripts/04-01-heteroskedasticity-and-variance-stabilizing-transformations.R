##################################################
### Load libraries
##################################################

library(broom)
library(car)
library(corrr)
library(patchwork)
library(tidyverse)



##################################################
### Import and prepare data
##################################################

slid = read_csv("https://raw.githubusercontent.com/zief0002/symmetrical-fiesta/main/data/slid.csv")
slid



##################################################
### Examine plots
##################################################

d1 = ggplot(data = slid, aes(x = wages)) +
  geom_density() +
  theme_bw() +
  xlab("Hourly wage rate")

d2 = ggplot(data = slid, aes(x = age)) +
  geom_density() +
  theme_bw() +
  xlab("Age (in years)")

d3 = ggplot(data = slid, aes(x = education)) +
  geom_density() +
  theme_bw() +
  xlab("Education (in years)")


p1 = ggplot(data = slid, aes(x = age, y = wages)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  theme_bw() +
  xlab("Age (in years)") +
  ylab("Hourly wage rate")

p2 = ggplot(data = slid, aes(x = education, y = wages)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  theme_bw() +
  xlab("Education (in years)") +
  ylab("Hourly wage rate")


p3 = ggplot(data = slid, aes(x = male, y = wages)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  theme_bw() +
  xlab("Male") +
  ylab("Hourly wage rate")

(d1 | d2 | d3) / (p1 | p2 | p3)



##################################################
### Function to create residual plots
##################################################

# Function to create residual plots
residual_plots = function(object){
  # Get residuals and fitted values
  aug_lm = broom::augment(object, se_fit = TRUE)
  
  # Create residual plot
  p1 = ggplot(data = aug_lm, aes(x =.resid)) +
    educate::stat_density_confidence(model = "normal") +
    geom_density() +
    theme_light() +
    xlab("Residuals") +
    ylab("Probability Density")
  
  # Create residual plot
  p2 = ggplot(data = aug_lm, aes(x =.fitted, y = .resid)) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    geom_point(alpha = 0.1) +
    geom_smooth(method = "lm", se = TRUE) +
    geom_smooth(method = "loess", se = FALSE, n = 50, span = 0.67) +
    theme_light() +
    xlab("Fitted values") +
    ylab("Residuals")
  
  
  return(p1 | p2)
}



##################################################
### Fit OLS model
##################################################

# Fit model
lm.1 = lm(wages ~ 1 + age + education + male, data = slid)


# Examine residual plots
residual_plots(lm.1)


# Fit and evaluate interaction model
lm.2 = lm(wages ~ 1 + age + education + male + age:education, data = slid)
residual_plots(lm.2)



##################################################
### Variance stabilizing transformations
##################################################

# Create transformed y-values
slid = slid |>
  mutate(
    sqrt_wages = sqrt(wages),
    ln_wages = log(wages)
  )

# Fit models
lm_sqrt = lm(sqrt_wages ~ 1 + age + education + male, data = slid)
lm_ln = lm(ln_wages ~ 1 + age + education + male, data = slid)

# Examine residual plots
residual_plots(lm_sqrt) / residual_plots(lm_ln)



##################################################
### Box-Cox transformation
##################################################

# Find optimal power transformation using Box-Cox
powerTransform(lm.1)

slid = slid |>
  mutate(
    bc_wages = (wages ^ 0.086 - 1) / 0.086
  )

# Fit models
lm_bc = lm(bc_wages ~ 1 + age + education + male, data = slid)

# Examine residual plots
residual_plots(lm_bc)

# Coeffifient-level output
tidy(lm_bc, conf.int = TRUE)



##################################################
### Profile plot
##################################################

# Plot of the log-likelihood for a given model versus a sequence of lambda values
boxCox(lm.1, lambda = seq(from = -2, to = 2, by = 0.1))

# Zoom in on confidence limits
boxCox(lm.1, lambda = seq(from = 0.03, to = 0.2, by = .001))



