##################################################
### Load libraries
##################################################

library(MASS)
library(modelr)
library(tidymodels)
library(tidyverse)




##################################################
### Import and prepare data
##################################################

# Read in data
eeo = read_csv("https://raw.githubusercontent.com/zief0002/symmetrical-fiesta/main/data/equal-education-opportunity.csv")

# Standardize all variables in the eeo data frame
z_eeo = eeo |>
  scale() |>
  data.frame()

# View data
head(z_eeo)



##################################################
### Set up data for 10-fold CV
##################################################

# Create a vector that includes 7 cases in each of the 10 folds
fold = rep(1:10, 7)

# Randomly assign each case to a fold
set.seed(100) #Set seed for reproducible results
z_eeo = z_eeo |>
  mutate(
    fold = sample(fold)
  )

# View data
head(z_eeo)



##################################################
### Function to do the CV
##################################################

ridge_mse = function(.training, .validation){
  
  # Fit ridge model across several lambda values to the training data
  ridge_models = lm.ridge(achievement ~ 1 + faculty + peer + school, 
                          data = .training,
                          lambda = seq(from = 0, to = 100, by = 0.01)
  )
  
  # Use coefficients from ridge_models with validation data
  # Get design matrix and outcome
  X = as.matrix(.validation[c("faculty", "peer", "school")])
  y = .validation$achievement
  
  # How many lambda values?
  n_lambda = length(ridge_models$lambda)
  
  # Create empty vector to store MSE values
  MSE = c()
  
  # Loop through different sets of coefficients to compute MSE
  for(i in 1:n_lambda){
    b = matrix(ridge_models$coef[ , i]) #Get coefficients for a given lambda
    e = y - (X %*% b) #Compute residual vector
    MSE[i] = sum(e ^ 2) / length(e) #Compute MSE
  }
  
  # Create data frame of lambda and associated MSE values
  d = data.frame(
    lambda = ridge_models$lambda,
    MSE = MSE
  )
  
  return(d)
}



##################################################
### Test function
##################################################

# Create datasets
train = z_eeo |> filter(fold != 1)
validate = z_eeo |> filter(fold == 1)

# Test function
ridge_mse(.training = train, .validation = validate)



##################################################
### Loop through all 10 folds
##################################################

# Create an empty list to store dataframes of the results for each of the 10 cross-validations
my_results = list()

for(i in 1:10){
  
  # Create datasets
  train = z_eeo |> filter(fold != i)
  validate = z_eeo |> filter(fold == i)
  
  # Carry out cross-validation
  my_results[[i]] = ridge_mse(.training = train, .validation = validate) |>
    mutate(
      v_fold = i
    )
}

# Stack the 10 datframes into a single dataframe
cv_results = do.call(rbind, my_results)



##################################################
### Obtain lambda with smallest CV-MSE
##################################################

cv_results |>
  group_by(lambda) |>
  summarize(
    CV_MSE = mean (MSE)
  ) |>
  arrange(CV_MSE) |>
  data.frame() |>
  head(10) #only show the first 10 rows



##################################################
### Use best lambda
##################################################

# Fit model with best lambda value
best_ridge = lm.ridge(achievement ~ 1 + faculty + peer + school, 
                      data = z_eeo,
                      lambda = 12.90
)

# View coefficients
tidy(best_ridge)



##################################################
### Use glmnet functions
##################################################

# Load library
library(glmnet)

# Get matrices for use in glmnet
X = scale(eeo)[ , 2:4]
Y = scale(eeo)[ , 1]

# Make results reproducible
set.seed(100)

# Carry out the cross-validation
ridge_cv = cv.glmnet(
  x = X,
  y = Y,
  nfolds = 10,
  type.measure = "mse",
  alpha = 0,
  intercept = FALSE,
  standardize = FALSE
)

# Extract the lambda value with the lowest mean error
ridge_cv$lambda.min


# Convert lambda to lm.ridge() lambda
0.2108243 * 70



##################################################
### Use glmnet() to fit best lambda
##################################################

# Fit ridge regression
ridge_best = glmnet(
  x = X,
  y = Y,
  alpha = 0,
  lambda = ridge_cv$lambda.min,
  intercept = FALSE,
  standardize = FALSE
)

# Show coefficients
tidy(ridge_best)


