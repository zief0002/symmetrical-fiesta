##################################################
### Load libraries
##################################################

library(broom)
library(car)
library(corrr)
library(educate)
library(patchwork)
library(tidyverse)




##################################################
### Import data
##################################################

slid = read_csv("https://raw.githubusercontent.com/zief0002/symmetrical-fiesta/main/data/slid.csv")

# View data
slid


##################################################
### Fit OLS model 
##################################################

lm.1 = lm(wages ~ 1 + age + education + male, data = slid)



##################################################
### OLS model: Example 
##################################################

# Enter y vector
y = c(17.3, 17.1, 16.4, 16.4, 16.1, 16.2)

# Create design matrix
X = matrix(
  data = c(rep(1, 6), 21, 20 , 19, 18, 17, 16),
  ncol = 2
)

# Compute coefficients
b = solve(t(X) %*% X) %*% t(X) %*% y

# Compute SEs for coefficients
e = y - X %*% b
sigma2_e = t(e) %*% e / (6 - 1 - 1)
V_b = as.numeric(sigma2_e) * solve(t(X) %*% X)
sqrt(diag(V_b))

# Use built-in functions
lm.ols = lm(y ~ 1 + X[ , 2])
tidy(lm.ols, conf.int = TRUE)


##################################################
### WLS model: Example 
##################################################

# Set up weight matrix, W
class_sd = c(5.99, 3.94, 1.90, 0.40, 5.65, 2.59)
w_i = 1 / (class_sd ^ 2)
W = diag(w_i)
W

# Compute coefficients
b_wls = solve(t(X) %*% W %*% X) %*% t(X) %*% W %*% y
b_wls

# Compute standard errors for coefficients
e_wls = y - X %*% b_wls                                 #Compute errors from WLS
mse_wls = (t(W %*% e_wls) %*% e_wls) / (6 - 1 - 1)      #Compute MSE estimate
v_b_wls = as.numeric(mse_wls) * solve(t(X) %*% W %*% X) #Compute variance-covariance matrix for b
sqrt(diag(v_b_wls))                                     #Compute SEs for b



##################################################
### WLS estimation: Known error variances -- Using lm() function
##################################################

# Create weights vector
w_i = 1 / (class_sd ^ 2)

# Fit WLS model
lm_wls = lm(y ~ 1 + X[ , 2], weights = w_i)
tidy(lm_wls, conf.int = TRUE)



##################################################
### WLS estimation: Unknown error variances
##################################################

# Step 1: Fit the OLS regression
lm_step_1 = lm(wages ~ 1 + age + education + male + age:education, data = slid)


# Step 2: Obtain the residuals and square them
out_1 = augment(lm_step_1) |>
  mutate(
    e_sq = .resid ^ 2
  )


# Step 2: Regresss e^2 on the predictors from Step 1
lm_step_2 = lm(e_sq ~ 1 + age + education + male + age:education, data = out_1)


# Step 3: Obtain the fitted values from Step 2
y_hat = fitted(lm_step_2)


# Step 4: Create the weights
w_i = 1 / (y_hat ^ 2)


# Step 5: Use the fitted values as weights in the WLS
lm_step_5 = lm(wages ~ 1 + age + education + male + age:education, data = slid, weights = w_i)


# Examine residual plots
residual_plots(lm_step_5)


# Examine coefficient-level output
tidy(lm_step_5, conf.int = TRUE)



##################################################
### Huber-White sandwich estimates of the SEs
##################################################

# Fit OLS model
lm.1 = lm(wages ~ 1 + age + education + male, data = slid)


# Design matrix
X = model.matrix(lm.1)


# Sigma matrix
e_squared = augment(lm.1)$.resid ^ 2  
Sigma = e_squared * diag(3997)


# Variance-covariance matrix for B
V_b_huber_white = solve(t(X) %*% X) %*% t(X) %*% Sigma %*% X %*% solve(t(X) %*% X)


# Compute SEs
sqrt(diag(V_b_huber_white))



##################################################
### Huber-White (modified) sandwich estimates of the SEs
##################################################

# Sigma matrix
e_squared = augment(lm.1)$.resid ^ 2  / ((1 - augment(lm.1)$.hat) ^ 2)  
Sigma = e_squared * diag(3997)


# Variance-covariance matrix for B
V_b_huber_white_mod = solve(t(X) %*% X) %*% t(X) %*% Sigma %*% X %*% solve(t(X) %*% X)


# Compute SEs
sqrt(diag(V_b_huber_white_mod))

