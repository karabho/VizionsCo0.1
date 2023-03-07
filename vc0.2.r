# Load data
data(claims)

# Define the development periods
dev.periods <- 1:10

# Calculate the cumulative claims
cum.claims <- t(apply(claims, 1, cumsum))

# Create a matrix to store the estimates
estimates <- matrix(NA, nrow = nrow(cum.claims), ncol = length(dev.periods))

# Apply the chain ladder method
for (i in 1:length(dev.periods)) {
  j <- dev.periods[i]
  w <- cum.claims[, j-1] / cum.claims[, j-2]
  estimates[, i] <- cum.claims[, j] * w
}

# Estimate the ultimate losses
ultimate <- rowSums(estimates)

# Plot the results
plot(1:length(ultimate), ultimate, type = "l", xlab = "Accident Year", ylab = "Ultimate Loss Amount")
