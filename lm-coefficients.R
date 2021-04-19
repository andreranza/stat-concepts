# The script simulates repeated sampling of the variables 'medv' and 'lstat'
# of the Boston data set contained in the MASS package, in order to see the 
# the behaviour of quantities associated to the linear regression coefficient 
# under two different scenarios: strong correlation and non-correlation of variables.

library(MASS)
library(tidyverse)

# load data set
data("Boston")

boston_df <- as_tibble(Boston)

# explain medv (y) on the basis of lstat (x)
boston_df <- select(boston_df, medv, lstat)

# population size
cat("The size of the population is:", nrow(boston_df))

# True Beta (= Beta of the entire population)  ------------------------------

compute_beta <- function(df, x, y) {
  # Returns coefficient for a linear regression model
  
  mean_x <- mean(df[[x]])
  mean_y <- mean(df[[y]])
  
  deviation_x <- df[[x]] - mean_x
  squared_dev_x <- deviation_x^2
  
  deviation_y <- df[[y]] - mean_y
  
  beta1 <- sum(deviation_x*deviation_y)/sum(squared_dev_x)
  
  return(beta1)
  
}

true_beta <- compute_beta(df = boston_df, x = "lstat", y = "medv")

cat("The 'true' beta computed on the population is:", true_beta)

# Extract samples ---------------------------------------------------------

sample_size <- 250
numb_samples <- 500

# extract 500 samples of size 250 from the population
set.seed(2021)
samples <- lapply(
  1:numb_samples, 
  function(x) slice_sample(boston_df, n = sample_size, replace = FALSE)
)


# compute beta coeff for each sample
betas <- map_dbl(samples, ~ compute_beta(.x, x = "lstat", y = "medv"))
mean(betas)
sd(betas)

# empirical distribution of the beta coefficients
ggplot(as_tibble_col(betas), aes(x = betas, y = ..density..)) +
  geom_histogram(bins = 40, alpha = 0.6) +
  geom_density() +
  theme_bw()

cat("The sample mean of the betas sampled is:", mean(betas))

# compare mean of betas computed on the 500 samples with true one
cat("True:", true_beta, ";", "Estimated:", mean(betas))

# Compute quantities ------------------------------------------------------

# per calcolare la stat t ci servono gli errori standard dei beta che a loro volta 
# dalla quantiaa ignota sigma quadro uguale var(y) varianzia campionara corretta

compute_ci <- function(df, x, y, alpha) {
  # This function computes the quantities needed to assess
  # the significance of the beta coefficient
  
  # sample size
  n = nrow(df)
  
  # degrees of freedom of student - t distribution
  dgf = n - 2
  
  # value for which the prob to observe values of the t student with n - 2 
  # dgf, greater or equal to t is alpha/2
  t = - qt(alpha/2, df = dgf)
  
  # compute quantities for each sample
  df %>% 
    summarise(
      # beta coefficient associated to predictor
      beta1 = compute_beta(df, x, y),
      
      # sample variance of the response y 
      var_y = var(df[[y]]),
      
      # sample mean of the response y
      mean_y = mean(df[[y]]),
      
      # sample mean of the predictor x
      mean_x = mean(df[[x]]),
      
      # squared deviation from the mean of the predictor x
      squared_dist_x = sum((df[[x]] - mean_x)^2),
      
      # sample variance beta1
      var_beta1 = var_y/squared_dist_x,
      
      # standard error beta1
      std_beta1 = sqrt(var_beta1),
      
      # stat t under the null hypothesis: beta1 = 0
      t_stat = (beta1 - 0)/std_beta1,
      
      # estimated confidence interval for given prob alpha
      lower_bound = if_else(
        beta1 > t,
        beta1 + t * std_beta1,
        beta1 - t * std_beta1
      ),
    
      upper_bound = if_else(
        beta1 > t,
        beta1 - t * std_beta1,
        beta1 + t * std_beta1
        ),
      
      # p-value given the observed sample
      p_value = if_else(
        t_stat > 0, 
        pt(q = t_stat, df = dgf, lower.tail = FALSE), 
        pt(q = t_stat, df = dgf)
        ),
      
      # compare p-value with alpha
      is_greater = p_value > alpha
      
    )
}

summary_df1 <- map_dfr(samples, compute_ci, x = "lstat", y = "medv", alpha = 0.05)
count(summary_df1)

# Let's mess things up! ---------------------------------------------------

# modify the response variable to be a normal with laaarge variance
samples2 <- map(samples, ~ mutate(.x, lstat = rnorm(250, mean = 25, sd = 6)))

summary_df2 <- map_dfr(samples2,compute_ci, x = "medv", y = "lstat", alpha = 0.05)

count(summary_df2, is_greater)

