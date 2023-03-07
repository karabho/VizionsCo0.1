# Load required packages
library(rebmix)
library(bayesm)

# Load mortality data
data(mortality)

# Specify the number of age groups and years
K <- 20 # Number of age groups
T <- 10 # Number of years

# Prepare the data
ages <- seq(0, 100, length.out = K+1)[-1] # Age groups
years <- seq(2000, 2009) # Years
mortality <- as.matrix(mortality[, -1]) # Remove first column (age)

# Estimate the model parameters
beta0 <- rep(0, K) # Initial values for beta0
beta1 <- rep(0, K) # Initial values for beta1
beta2 <- rep(0, K) # Initial values for beta2
beta3 <- rep(0, K) # Initial values for beta3
sigma2 <- 1 # Initial value for sigma2
iter <- 5000 # Number of iterations
burn <- 1000 # Burn-in period

for (i in 1:T) {
  y <- mortality[, i] # Observed mortality rates
  z <- rep(1, K) # Vector of ones
  X <- cbind(z, log(ages), ages, ages^2) # Design matrix
  beta <- rnorm(4*K, 0, 1) # Initial values for beta
  sigma2 <- rgamma(1, 1, 1) # Initial value for sigma2
  out <- mcmc(regMix(X, y, beta, sigma2^(-0.5), nd = 2))
  beta0 <- c(beta0, out$beta[1, -1])
  beta1 <- c(beta1, out$beta[2, -1])
  beta2 <- c(beta2, out$beta[3, -1])
  beta3 <- c(beta3, out$beta[4, -1])
}

# Plot the estimated mortality rates
matplot(years, exp(beta0 + beta1*log(ages) + beta2*ages + beta3*ages^2),
        type = "l", lty = 1, xlab = "Year", ylab = "Mortality Rate",
        col = rainbow(K))
legend("topright", legend = paste("Age ", 1:K), col = rainbow(K), lty = 1, bty = "n")
