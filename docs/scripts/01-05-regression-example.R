##################################################
### Load libraries
##################################################

library(tidyverse)
library(broom)
library(patchwork)



##################################################
### Import and prepare data
##################################################

contraception = read_csv("https://raw.githubusercontent.com/zief0002/symmetrical-fiesta/main/data/contraception.csv")

# View data
contraception

# IF you want to see all the variables
#print(contraception, width = Inf)


# Create dummy variable for GNI
contraception = contraception |>
  mutate(
    high_gni = if_else(gni == "High", 1, 0)
  )


contraception |>
  select(contraceptive, educ_female, high_gni)


##################################################
### Fit model and obtain sigma_e, coefficients, SEs, etc.
##################################################

lm.1 = lm(contraceptive ~ 1 + educ_female + high_gni, data = contraception)
glance(lm.1)
tidy(lm.1)
summary(lm.1)



##################################################
### Examine the data
##################################################

# # Create density plot of contraception
# p1 = ggplot(data = contraception, aes(x = contraceptive)) + 
#   geom_density() +
#   theme_bw() +
#   labs(
#     x = "Contraceptive useage"
#   ) 
# 
# # Create density plot of female education level
# p2 = ggplot(data = contraception, aes(x = educ_female)) + 
#   geom_density() +
#   theme_bw() +
#   labs(
#     x = "Female education level"
#   ) 
# 
# 
# # Condition the relationship on GNI
# p3 = ggplot(data = contraception, aes(x = educ_female, y = contraceptive, color = gni)) + 
#   geom_point() +
#   geom_smooth(method = "lm", se = FALSE) +
#   theme_bw() +
#   labs(
#     x = "Female education level",
#     y = "Contraceptive useage"
#   ) +
#   facet_wrap(~gni)
# 
# # Layout plots with patchwork
# (p1 | p2) / p3




##################################################
### Coefficient estimates via matrix algebra
##################################################

# Store values
n = nrow(contraception) #Sample size
k = 2 #Number of predictors


# Create outcome vector
y = contraception$contraceptive


# Create design matrix
X = matrix(
  data = c(rep(1, n), contraception$educ_female, contraception$high_gni),
  ncol = 3
)

# Compute b vector
b = solve(t(X) %*% X) %*% t(X) %*% y
b



##################################################
### Compute residual standard error 
##################################################

# Compute e vector
e = y - X %*% b

# Compute s_e
s_e = sqrt((t(e) %*% e) / (n - k - 1))
s_e



##################################################
### Compute SEs for Bs
##################################################

# Compute variance-covariance matrix of b
V = as.numeric(s_e^2) * solve(t(X) %*% X)
V

# Compute SEs for b
sqrt(diag(V))



##################################################
### Coefficient-level inference
##################################################

# Compute t-value
t_0 = (b[1] - 0) / sqrt(V[1, 1])
t_0

# Evaluate t-value
df = n - k - 1
p = 2* (1 - pt(abs(t_0), df = df))
p

# Compute critical value
t_star = qt(.025, df = df)
t_star


# Compute CI
b[2] - abs(t_star) * sqrt(V[2, 2])
b[2] + abs(t_star) * sqrt(V[2, 2])



##################################################
### Compute ANOVA decomposition values
##################################################

# Compute needed values
mean_y = mean(y)
hat_y = X %*% b


# Compute SS_Total
ss_total = t(y - mean_y) %*% (y - mean_y)
ss_total

# Compute SS_model
ss_model = t(hat_y - mean_y) %*% (hat_y - mean_y)
ss_model

# Compute SS_residual
ss_residual = t(y - hat_y) %*% (y - hat_y)
ss_residual



##################################################
### Model-level inference
##################################################

# Compute R^2
r2 = ss_model / ss_total
r2

# Create L (hypothesis matrix)
L = matrix(
  data = c(0, 1, 0, 0, 0, 1),
  byrow = TRUE,
  ncol = 3
)

# Create vector of hypothesized values
C = matrix(
  data = c(0, 0),
  ncol = 1
)

q = 2

F_num = t(L %*% b - C) %*% solve(L %*% solve(t(X) %*% X) %*% t(L)) %*% (L %*% b - C)
F_denom = q * s_e^2

F_0 = F_num / F_denom
F_0

# Evaluate F_0
1 - pf(F_0, df1 = q, df2 = (n - k - 1))



##################################################
### Use R functions
##################################################

# Fit model
lm.1 = lm(contraceptive ~ 1 + educ_female + high_gni, data = contraception)

# Coefficient-level output
tidy(lm.1)

# Coempute confidence intervals for coefficients
confint(lm.1)

# Model-level output
glance(lm.1)

# ANOVA decomposition
anova(lm.1)

# Access design matrix
model.matrix(lm.1)

# Access coefficient estimates
coef(lm.1)

# Access variance-covariance matrix for b
vcov(lm.1)

# Access fitted values
fitted(lm.1)

# Access raw residuals
resid(lm.1)

