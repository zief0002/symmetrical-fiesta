##################################################
### Load libraries
##################################################

library(broom)
library(car)
library(corrr)
library(tidyverse)
library(patchwork)




##################################################
### Import and prepare data
##################################################

# Import data
contraception = read_csv(file = "https://raw.githubusercontent.com/zief0002/symmetrical-fiesta/main/data/contraception.csv")


# Create dummy variable for GNI indicator and single letter variable
contraception = contraception |>
  mutate(
    high_gni = if_else(gni == "High", 1, 0),
    gni2 = str_sub(contraception$gni, start = 1L, end = 1L)
  )


# View data
head(contraception)



##################################################
### Scatterplot
##################################################

ggplot(data = contraception, aes(x = educ_female, y = contraceptive, color = gni2)) +
  geom_text(aes(label = gni2)) +
  geom_smooth(method = "lm", se = FALSE) +
  theme_bw() +
  xlab("Female education level") +
  ylab("Contraceptive rate") +
  ggsci::scale_color_d3() +
  guides(color = FALSE)



##################################################
### Fit interaction model
##################################################

# Fit interaction model
lm.1 = lm(contraceptive ~ 1 + educ_female + high_gni + educ_female:high_gni, data = contraception)


# Model-level information
glance(lm.1)


# Coefficient-level information
tidy(lm.1, conf.int = 0.95)


##################################################
### Evaluate residuals
##################################################

# Augment model
out_1 = augment(lm.1)

# View augmented data
out_1

# Residual Plots
p1 = ggplot(data = out_1, aes(x = .resid)) +
  educate::stat_density_confidence(model = "normal") +
  geom_density() +
  theme_bw() +
  xlab("Residuals")

p2 = ggplot(data = out_1, aes(x = .fitted, y = .resid)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE) +
  geom_smooth(method = "loess", se = FALSE) +
  theme_bw() +
  xlab("Fitted values") +
  ylab("Residuals")

# Layout
p1 | p2


##################################################
### Leverage values: Example
##################################################

# Create X-matrix
X = matrix(
  data = c(rep(1, 7), 1, 2.0, 2.04, 3.0, 3.03, 4, 15),
  nrow = 7
)

# Compute and view H-matrix
H = X %*% solve(t(X) %*% X) %*% t(X)
H

# Show that h_ii = sum(h_ij^2)
# Compute sum of all the squared values in row 1
sum(H[1, 1:7] ^ 2)

# Find the h_ii element in row 1
H[1, 1]

# Extract the hat-values
diag(H)

# Create data to plot from
d = data.frame(
  case = 1:7,
  h_ii = diag(H)
)

# View d
d

# Create index plot
ggplot(data = d, aes(x = case, y = h_ii)) +
  geom_point(size = 4) +
  theme_bw() +
  xlab("Observation number") +
  ylab("Leverage value")


##################################################
### Leverage values: Contraception example
##################################################

# View augmented data
head(out_1)

# Add case number to the augmented data
out_1 = out_1 |>
  mutate(
    case = row_number()
  )

# View augmented data
out_1

# Create index plot
ggplot(data = out_1, aes(x = case, y = .hat)) +
  geom_point(size = 4) +
  theme_bw() +
  xlab("Observation number") +
  ylab("Leverage value")

# Compute cutoff
cutoff = 2 * mean(out_1$.hat)
cutoff

# Create index plot
ggplot(data = out_1, aes(x = case, y = .hat)) +
  geom_text(aes(label = case)) +
  geom_hline(yintercept = cutoff, color = "#cc79a7") +
  theme_bw() +
  xlab("Observation number") +
  ylab("Leverage value")

# Add country names to augmented data
out_1 = out_1 |>
  mutate(
    country = contraception$country
  )

# View data
out_1 |>
  print(width = Inf)

# Create index plot
ggplot(data = out_1, aes(x = case, y = .hat)) +
  geom_text(aes(label = country)) +
  geom_hline(yintercept = cutoff, color = "#cc79a7") +
  theme_bw() +
  xlab("Observation number") +
  ylab("Leverage value")





##################################################
### Measuring outlyingness
##################################################

# Obtain estimate of error variance
s2e = glance(lm.1)$sigma ^ 2
s2e

# Compute standardized residuals
out_1 |>
  mutate(
    standardized_resid = .resid / (sqrt(s2e * (1 - .hat)))
  )

# Create plot of standardized residuals versus
ggplot(data = out_1, aes(x = .fitted, y = .std.resid)) +
  geom_text(aes(label = country)) +
  geom_hline(yintercept = 0) +
  geom_hline(yintercept = c(-2, 2), color = "#cc79a7") +
  theme_bw() +
  xlab("Fitted values") +
  ylab("Residuals")



##################################################
### Compute studentized residuals
##################################################

# View augmented output: Studentized residuals
out_1 |>
  print(width = Inf)


# Compute studentized residuals
out_1 = out_1 |>
  mutate(
    t_i = .resid / (.sigma * sqrt(1 - .hat))
  )

# View output
out_1 |>
  print(width = Inf)


# Compute studentized residuals
out_1 = out_1 |>
  mutate(
    t_i = rstudent(lm.1)
  )

# View output
out_1 |>
  print(width = Inf)



##################################################
### Test of the studentized residual = 0
##################################################

# Compute p-value for Obs. 1
2 * pt(-0.097, df = 93)

# Bonferroni adjustment for Obs. 1
0.9229351 * 97

# Bonferroni adjustment for all observations
out_1 = out_1 |>
  mutate(
    p = 2 * pt(-abs(t_i), df = 97),
    p_adj = p.adjust(p, method = "bonferroni")
  )

# Order data by adjusted p-values
out_1 |>
  arrange(p_adj) |>
  print(width = Inf)



##################################################
### Influence: DFBETA and scaled DFBETA
##################################################

dfbeta(lm.1) |>
  data.frame()

# Obtain c_jj value corresponding to the interaction term
X = model.matrix(lm.1)
solve(t(X) %*% X)

#Compute scaled DFBETA for interaction term (Obs. 1)
-0.0299422430 / (14.5 * sqrt(0.009320611))

# Get scaled DFBETA values
dfbetas(lm.1) |>
  data.frame()

# Set up data with case number and scaled DFBETA values
scl_dfbeta = data.frame(
  case = 1:97
) |>
  cbind(dfbetas(lm.1))


# Create index plots
ggplot(data = scl_dfbeta, aes(x = case, y = abs(`educ_female:high_gni`))) +
  geom_text(aes(label = case), size = 3) +
  xlab("Observation number") +
  ylab("Scaled DFBETA value") +
  theme_bw() +
  geom_hline(yintercept = 2 / sqrt(97), color = "#cc79a7")



##################################################
### Influence: Cook's Distance
##################################################

# Compute D_i for Obs. 1
0.0976^2 / (3 + 1) * 0.0875 / (1 - 0.0875)

# View augmented output
out_1 |>
  print(width = Inf)


# Add case number to data
out_1 = out_1 |>
  mutate(case = row_number())

# Create index plot
ggplot(data = out_1, aes(x = case, y = .cooksd)) +
  geom_text(aes(label = case), size = 3) +
  xlab("Observation number") +
  ylab("Cook's D value") +
  theme_bw() +
  geom_hline(yintercept = 4 / (97 - 3- 1), color = "#cc79a7")






##################################################
### One last diagnostic plot
##################################################

# Create index plot
ggplot(data = out_1, aes(x = .hat, y = abs(t_i))) +
  geom_text(aes(label = case, size = .cooksd)) +
  xlab("Leverage value") +
  ylab("Absolute studentized residual") +
  theme_bw() +
  scale_size(
    name = "Cook's D",
    range = c(1, 10)
  ) +
  geom_hline(yintercept = 2, color = "#cc79a7", linetype = "dashed") +
  geom_vline(xintercept = 2 * mean(out_1$.hat), color = "#cc79a7", linetype = "dashed") +
  guides(size = "none")



##################################################
### Re-fit model, removing observations 53 and 84
##################################################

# Remove observations
contraception2 = contraception |>
  filter(!country %in% c("Tajikistan", "Maldives"))

# Fit interaction model
lm.2 = lm(contraceptive ~ 1 + educ_female + high_gni + educ_female:high_gni, data = contraception2)

# Augment model
out_2 = augment(lm.2)

# View augmented data
out_2


##################################################
### Residual plots
##################################################

# Residual Plots
p1 = ggplot(data = out_2, aes(x = .resid)) +
  educate::stat_density_confidence(model = "normal") +
  geom_density() +
  theme_bw() +
  xlab("Residuals")

p2 = ggplot(data = out_2, aes(x = .fitted, y = .resid)) +
  geom_point() +
  geom_hline(yintercept = 0) +
  theme_bw() +
  xlab("Fitted values") +
  ylab("Residuals")

# Layout
p1 | p2

# Model-level information
glance(lm.2)


# Add studentized residuals and country names
out_2 = out_2 |>
  mutate(
    t_i = .resid / (.sigma * sqrt(1 - .hat)),
    country = contraception2$country
  )


# Create index plot
ggplot(data = out_2, aes(x = .hat, y = abs(t_i))) +
  geom_text(aes(label = country, size = .cooksd)) +
  xlab("Leverage value") +
  ylab("Absolute studentized residual") +
  theme_bw() +
  scale_size(
    name = "Cook's D",
    range = c(1, 10)
  ) +
  geom_hline(yintercept = 2, color = "#cc79a7", linetype = "dashed") +
  geom_vline(xintercept = 2 * mean(out_2$.hat), color = "#cc79a7", linetype = "dashed") +
  guides(size = "none")



