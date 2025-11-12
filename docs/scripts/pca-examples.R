##################################################
### Load libraries
##################################################

library(tidyverse)



##################################################
### Bank Example
##################################################

# Import data
bank = read_csv("https://statisticsbyjim.com/wp-content/uploads/2023/01/BankData.csv") |>
  select(Income:`Credit cards`)

bank


# Select quantitative predictors and standardize them
bank_pred = bank |>
  scale()


# Create princomp object
my_pca = princomp(bank_pred)


# View output
summary(my_pca, loadings = TRUE, cutoff = 0.4)


# Get PC scores
pc_scores = my_pca$scores


# Create data frame of scores
pc_scores = pc_scores |>
  data.frame() |>
  mutate(
    id = row_number(bank)
  )


# View PC scores
head(pc_scores)


# Plot the scores
ggplot(data = pc_scores, aes(x = Comp.1, y = Comp.2)) +
  #geom_point() +
  geom_text(aes(label = id)) +
  geom_hline(yintercept = 0, color = "lightgrey") +
  geom_vline(xintercept = 0, color = "lightgrey") +
  #scale_x_continuous(name = "Principal Component 1", limits = c(-4, 4)) +
  #scale_y_continuous(name = "Principal Component 2", limits = c(-4, 4)) +
  theme_bw() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  )




##################################################
### AKC (dog) example
##################################################

# Import data 
akc = read_csv("https://github.com/zief0002/symmetrical-fiesta/raw/main/data/akc.csv")


# Select quantitative predictors and standardize them
akc_pred = akc |>
  select_if(is.numeric) |>
  scale()


# Create princomp object
my_pca = princomp(akc_pred)


# View output
summary(my_pca, loadings = TRUE, cutoff = 0.3)


# Get PC scores
pc_scores = my_pca$scores

# View PC scores
head(pc_scores)

# Create data frame of scores
pc_scores = pc_scores |>
  data.frame() |>
  mutate(
    breed = akc$name,
    group = akc$group,
    trait = akc$trait
  )


# Plot the scores
ggplot(data = pc_scores, aes(x = Comp.1, y = Comp.2)) +
  #geom_point() +
  geom_text(aes(label = breed)) +
  geom_hline(yintercept = 0, color = "lightgrey") +
  geom_vline(xintercept = 0, color = "lightgrey") +
  #scale_x_continuous(name = "Principal Component 1", limits = c(-4, 4)) +
  #scale_y_continuous(name = "Principal Component 2", limits = c(-4, 4)) +
  theme_bw() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  )



##################################################
### Netflix (movies) example
##################################################

# Create user ratings matrix for 5 movies
movies = data.frame(
  Predator     = c(1, 3, 4, 5, 0, 0, 0),
  StarWars     = c(1, 3, 4, 5, 2, 0, 1),
  Terminator   = c(1, 3, 4, 5, 0, 0, 0),
  LoveActually = c(0, 0, 0, 0, 4, 5, 2),
  LadyTramp  = c(0, 0, 0, 0, 4, 5, 2)
)

# Add the user names
row.names(movies) = c("Taylor", "Rhianna", "Beyonce", "Justin", "Snoop", "Martha", "Jay-Z")

# View the matrix
movies


# Standardize the ratings and turn into matrix
M = movies |> 
  scale() |>
  data.matrix()


# Carry out the SVD
pca_movies = svd(M)


# Name the principal components
PC = c("SciFi/Romance", "General Movies", "Arnold/SciFi", "Romance - Anim./Live", "Arnold Campy/Cool")



# Name the rows and columns in the V matrix which correspond to the PC loadings
# These provide composite values for different movies
V = as.matrix(round(pca_movies$v, 2))
row.names(V) = names(movies)
colnames(V) = PC
V

# Name the rows and columns in the U matrix
# These provide composite values for different users
U = as.matrix(round(pca_movies$u, 2))
row.names(U) = c("Taylor", "Rhianna", "Beyonce", "Justin", "Snoop", "Martha", "Jay-Z")
colnames(U) = PC
U





