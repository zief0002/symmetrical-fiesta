##################################################
### Load libraries
##################################################

library(tidyverse)    #Plotting, wrangling, basically everything (Loads dplyr, ggplot2, readr, and others)
library(broom)        #Fitted regression results, creating residuals
library(corrr)        #Correlations
library(educate)      #Evaluate residual plots; NEED VERSION 0.3.01 (or higher)
library(patchwork)    #For layout with more than one plot
library(skimr)        #To plot/describe many variables simultaneously




##################################################
### Import data
##################################################

pew = read_csv(file = "https://raw.githubusercontent.com/zief0002/fluffy-ants/main/data/pew.csv")


# View data
pew


##################################################
### Explore Outcome
##################################################

# Density plot of outcome
ggplot(data = pew, aes(x = knowledge)) +
  geom_density() +
  theme_light() +
  xlab("Political knowledge") +
  ylab("Probability density") +
  theme(
    text = element_text(size = 20)
  )

# Numerical Summaries
pew |> 
  summarize(
    M = mean(knowledge),
    SD = sd(knowledge)
  )


##################################################
### Explore Focal Predictor
##################################################

# Density plot
ggplot(data = pew, aes(x = news)) +
  geom_density() +
  theme_light() +
  xlab("News exposure") +
  ylab("Probability density") +
  theme(
    text = element_text(size = 20)
  )

# Numerical Summaries
pew |> 
  summarize(
    M = mean(news),
    SD = sd(news)
  )


##################################################
### Explore Covariates
##################################################

# Dummy code party attribute
pew = pew |>
  mutate(
    democrat = if_else(party == "Democrat", 1, 0),
    independent = if_else(party == "Independent", 1, 0),
    republican = if_else(party == "Republican", 1, 0)
  )

pew

# Compute quick summaries
pew |>
  select(age:republican, -party) |>
  skim()



##################################################
### Explore Relationships
##################################################

# Scatterplot
ggplot(data = pew, aes(x = news, y = knowledge)) +
  geom_point(size = 4, shape = 21, color = "black", fill = "skyblue") +
  theme_light() +
  xlab("News exposure") +
  ylab("Political knowledge") +
  theme(
    text = element_text(size = 20)
  )

# Correlation
pew |> 
  select(knowledge, news) |>
  correlate()


# Scatterplots between all Numeric attributes
# Scatterplot matrix
pew |>
  select(-id, -party) |>
  pairs()

# Correlations between all Numeric attributes
pew |> 
  select(-id, -party) |>
  correlate()



##################################################
### Evaluate RQ 1
##################################################

# Fit model
lm.1 = lm(knowledge ~ 1 + news, data = pew)

# model-level output
glance(lm.1) |> print(width = Inf)

# Coefficient-level output
tidy(lm.1)

# Assumptions
# Need educate and patchwork libraries loaded
residual_plots(lm.1) 



##################################################
### Evaluate RQ 2
##################################################

lm.2 = lm(knowledge ~ 1 +  news + age + education + male + engagement + ideology + democrat + republican, data = pew)

# model-level output
glance(lm.2)

# Coefficient-level output
tidy(lm.2)

# Assumptions
residual_plots(lm.2)



##################################################
### Evaluate RQ 3
##################################################

lm.3 = lm(knowledge ~ 1 + news + age + education + male + engagement + ideology + democrat + republican + 
            news:education, data = pew)

# model-level output
glance(lm.3)

# Coefficient-level output
tidy(lm.3)





##################################################
### Plot interaction effect
##################################################

# Intercept for education = 12: -79.9 + .107*50 + 6.28*12 + 11.3*0 +.0323*50 -.062*0 + 3.71*1 + 8.29*0
# Slope for education = 12: 1.14 -0.0651*12

# Intercept for education = 16: -79.9 + .107*50 + 6.28*16 + 11.3*0 +.0323*50 -.062*0 + 3.71*1 + 8.29*0
# Slope for education = 16: 1.14 -0.0651*16

ggplot(data = pew, aes(x = news, y = knowledge)) +
  geom_point(alpha = 0) +
  geom_abline(intercept = 6.135, slope = 0.3588, color = "#2dd7f8", linewidth = 1.5, linetype = "dashed") + #HS education
  geom_abline(intercept = 31.255, slope = 0.0984, color = "#6d92ee", linewidth = 1.5) + #UG education
  theme_light() +
  xlab("News exposure") +
  ylab("Political knowledge") +
  theme(
    text = element_text(size = 20)
  )



# Assumptions
residual_plots(lm.3)
