##################################################
### Load libraries
##################################################

library(tidyverse)
library(broom)
library(corrr)



##################################################
### Input summary measures
##################################################

# Create correlation matrix
corrs = matrix(
  data = c(
    1.000, 0.737, 0.255,
    0.737, 1.000, 0.205,
    0.255, 0.205, 1.000
  ),
  nrow = 3
)


means = c(0, 0, 0) # Create mean vector
n = 1000           # Set sample size



##################################################
### Simulate data to use in path analysis
##################################################

# Make simulation reproducible
set.seed(1)


# Simulate the data and convert to data frame
sim_dat <- data.frame(MASS::mvrnorm(n = 1000, mu = means, Sigma = corrs, empirical = TRUE)) %>%
  rename(
    achievement = X1,
    ability = X2, 
    motivation = X3
  )


# View simulated data
head(sim_dat)



##################################################
### Correlations between ability, motivation, and achievement
##################################################

sim_dat %>%
  select(ability, motivation, achievement) %>%
  correlate()


##################################################
### Fit regression models to obtain path coefficients and error path coefficients
##################################################

# Path coefficients
tidy(lm(achievement ~ 0 + motivation + ability, data = sim_dat))
tidy(lm(motivation ~ 0 + ability, data = sim_dat))


# Paths to Disturbances/Errors
glance(lm(achievement ~ 0 + motivation + ability, data = sim_dat))
sqrt(1 - 0.554)

glance(lm(motivation ~ 0 + ability, data = sim_dat))
sqrt(1 - 0.0420)


##################################################
### Obtain indirect and total effects of ability on achievement
##################################################

# Indirect effects
0.205*0.108

# Total effects
0.715 + 0.205*0.108
