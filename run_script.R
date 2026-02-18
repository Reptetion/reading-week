# CLT simulation in R (non-normal population)
set.seed(123)

# 1) Create a non-normal population: Exponential (right-skewed)
N_pop <- 100000
population <- rexp(N_pop, rate = 1)

# 2) Function to generate many sample means for a given n
sample_means <- function(n, reps = 5000) {
  replicate(reps, mean(sample(population, size = n, replace = TRUE)))
}

# 3) Generate sampling distributions of the mean
reps <- 5000
means_n5  <- sample_means(5, reps)
means_n30 <- sample_means(30, reps)
means_n50 <- sample_means(50, reps)

# 4) Plot: population vs sampling distributions
par(mfrow = c(2,2))

hist(population, breaks = 60, main = "Population (Exponential, skewed)", xlab = "Value")
hist(means_n5,  breaks = 40, main = "Sample Means (n = 5)",  xlab = "Mean")
hist(means_n30, breaks = 40, main = "Sample Means (n = 30)", xlab = "Mean")
hist(means_n50, breaks = 40, main = "Sample Means (n = 50)", xlab = "Mean")

par(mfrow = c(1,1))
