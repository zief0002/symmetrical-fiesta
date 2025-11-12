##################################################
### Load libraries
##################################################

library(broom)
library(MASS)
library(tidyverse)



##################################################
### Import data
##################################################

eeo = read_csv("https://raw.githubusercontent.com/zief0002/symmetrical-fiesta/main/data/equal-education-opportunity.csv")

# View data
eeo



##################################################
### Fit standardized regression
##################################################

# Fit standardized model
lm.z = lm(scale(achievement) ~ 1 + scale(faculty) + scale(peer) + scale(school), data = eeo)

# Obtain the design matrix
X = model.matrix(lm.z)
head(X)



##################################################
### Compute condition number for X^T(X)
##################################################

# Get eigenvalues
eig_val = eigen(t(X) %*% X)$values

# Compute condition number
sqrt(max(eig_val) / min(eig_val))



##################################################
### What does it mean to be ill-conditioned?
##################################################

z_eeo = eeo |> 
  scale() |>
  data.frame()


tidy(lm(achievement ~ 1 + faculty + peer + school, data = z_eeo))


# Add small perturbations to inputs
set.seed(250)
z_eeo |>
  mutate(
    achievement = achievement + rnorm(70, mean = 0, sd = 0.01),
    faculty = faculty + rnorm(70, mean = 0, sd = 0.01),
    peer = peer + rnorm(70, mean = 0, sd = 0.01),
    school = school + rnorm(70, mean = 0, sd = 0.01),
    ) %>%
  lm(achievement ~ 1 + faculty + peer + school, data = .) |>
  tidy() |>
  mutate(
    delta = c(0, 0.525, 0.945, -1.03)  - estimate
  )




##################################################
### Compute condition number for  X^T(X) with inflated diagonal
##################################################

# Add 10 to each of the diagonal elements of X^T(X)
inflated = t(X) %*% X + 10*diag(4)

# Get eigenvalues
eig_val_inflated = eigen(inflated)$values

# Compute condition number
sqrt(abs(max(eig_val_inflated)) / abs(min(eig_val_inflated)))



##################################################
### Carry out ridge regression
##################################################

# Create y vector
y = scale(eeo$achievement)

# Compute and view lambda(I)
lambda_I = 0.1 * diag(4)
lambda_I

# Compute ridge regression coefficients
b = solve(t(X) %*% X + lambda_I) %*% t(X) %*% y
b



##################################################
### Use lm.ridge() function to fit ridge regression
##################################################

# Fit ridge regression (lambda = 0.1)
ridge_1 = lm.ridge(achievement ~ 1 + faculty + peer + school, 
                   data = z_eeo, 
                   lambda = 0.1)

# View coefficients
tidy(ridge_1)


##################################################
### Compare to OLS coefficients
##################################################

# Fit model
lm.1 = lm(achievement ~ 1 + faculty + peer + school, data = z_eeo)
coef(lm.1)


# Obtain coefficients
coef(lm.1)



##################################################
### Choosing lambda: Ridge trace
##################################################

# Fit ridge model across several lambda values
ridge_models = ridge_1 = lm.ridge(achievement ~ 1 + faculty + peer + school, data = z_data, 
                                  lambda = seq(from = 0, to = 100, by = 0.1))

# Get tidy() output
ridge_trace = tidy(ridge_models)
ridge_trace

# Ridge trace
ggplot(data = ridge_trace, aes(x = lambda, y = estimate)) +
  geom_line(aes(group = term, color = term)) +
  theme_bw() +
  xlab("lambda value") +
  ylab("Coefficient estimate") +
  ggsci::scale_color_d3(name = "Predictor")


# Try lambda=50
ridge_1 = lm.ridge(achievement ~ 1 + faculty + peer + school, data = z_data, lambda = 50)
tidy(ridge_1)


##################################################
### Compute AIC for ridge model with lambda = 0.1
##################################################

# Compute coefficients for ridge model
b = solve(t(X) %*% X + 0.1*diag(4)) %*% t(X) %*% y

# Compute residual vector
e = y - (X %*% b)

# Compute H matrix
H = X %*% solve(t(X) %*% X + 0.1*diag(4)) %*% t(X)

# Compute df
df = sum(diag(H))

# Compute AIC
aic = 70 * log(t(e) %*% e) + 2 * df
aic


# Function to compute AIC based on inputted lambda value
ridge_aic = function(lambda){
  b = solve(t(X) %*% X + lambda*diag(4)) %*% t(X) %*% y
  e = y - (X %*% b)
  H = X %*% solve(t(X) %*% X + lambda*diag(4)) %*% t(X)
  df = sum(diag(H))
  n = length(y)
  aic = n * log(t(e) %*% e) + 2 * df
  return(aic[[1]])
}

# Try function
ridge_aic(lambda = 0.1)
ridge_aic(lambda = 50)



##################################################
### Use AIC to select lambda
##################################################

# Create data frame with column of lambda values
my_models = data.frame(
  Lambda = seq(from = 0, to = 100, by = 0.01)
) |>
  rowwise() |>
  # Create a new column by usingthe ridge_aic() function for each row
  mutate(
    AIC = ridge_aic(Lambda)
  ) |>
  #Turn off the rowwise() operation
  ungroup() |> 
  data.frame()

# Find lambda associated with smallest AIC
my_models |> 
  filter(AIC == min(AIC)) |>
  data.frame()



##################################################
### Refit ridge regression with lambda = 43.97
##################################################

ridge_smallest_aic = lm.ridge(achievement ~ -1 + faculty + peer + school, 
                                data = z_eeo, 
                                lambda = 43.97)

# View coefficients
tidy(ridge_smallest_aic)



##################################################
### Estimate bias
##################################################

# OLS estimates
b_ols = solve(t(X) %*% X) %*% t(X) %*% y

# Compute lambda(I)
lambda_I = 43.97*diag(4)

# Estimate bias in ridge regression coefficients
-43.97 * solve(t(X) %*% X + lambda_I) %*% b_ols


# Ridge trace
ggplot(data = ridge_trace, aes(x = lambda, y = estimate)) +
  geom_line(aes(group = term, color = term)) +
  geom_vline(xintercept = 43.97, linetype = "dotted") +
  theme_bw() +
  xlab(expression(lambda)) +
  ylab("Coefficient estimate") +
  ggsci::scale_color_d3(name = "Predictor")


# Difference b/w OLS and ridge coefficients
# Remove intercept from OLS coefficients
tidy(ridge_smallest_aic)$estimate - b_ols[-1]



##################################################
### Sampling variation of the coefficients
##################################################

# Fit standardized model to obtain sigma^2_e
glance(lm(achievement ~ 1 + faculty + peer + school, data = z_eeo))$sigma

# Compute sigma^2_epsilon
resid_var = 0.910945 ^ 2

# Compute variance-covariance matrix of ridge estimates
W = solve(t(X) %*% X + 43.97*diag(4))
var_b = resid_var * W %*% t(X) %*% X %*% W

# Compute SEs
sqrt(diag(var_b))



##################################################
### Inference for school facilities coefficient
##################################################

# Compute t-value for school predictor
t = 0.103 / 0.03291127 
t

# Compute df residual
H = X %*% solve(t(X) %*% X + 43.97*diag(4)) %*% t(X)
df_model = sum(diag(H))
df_residual = 69 - df_model
df_residual

# Compute p-value
p = pt(-abs(t), df = df_residual) * 2
p

# Compute CI
0.103 - qt(p = 0.975, df = df_residual) * 0.03291127 
0.103 + qt(p = 0.975, df = df_residual) * 0.03291127  




