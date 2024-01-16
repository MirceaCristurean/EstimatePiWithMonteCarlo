n <- 100000
num_simulations <- 100
pi_estimates <- matrix(0, nrow = num_simulations, ncol = n)

# Perform 100 independent simulations
for (sim in 1:num_simulations) {
    x <- array(0, c(2, n))
    t <- array(0, c(1, n))

    for (i in 1:n) {
        # generate point in square
        x[1, i] <- 2 * runif(1) - 1
        x[2, i] <- 2 * runif(1) - 1

        # compute phi(x); test whether in disk
        if (x[1, i] * x[1, i] + x[2, i] * x[2, i] <= 1) {
            t[i] <- 1
        } else {
            t[i] <- 0
        }

        # Calculate pi estimate at each step
        pi_estimates[sim, i] <- 4 * sum(t) / i
    }
}

# Create a plot with different colors for each simulation
colors <- rainbow(num_simulations)
plot(1:n, pi_estimates[1, ], type = "l", xlab = "Number of Points", ylab = "Estimate of Pi", col = colors[1], ylim = c(3, 4))
abline(h = pi, col = "red", lty = 2)  # Plot a horizontal line at the actual value of pi

# Add lines for the remaining simulations with different colors
for (sim in 2:num_simulations) {
    lines(1:n, pi_estimates[sim, ], col = colors[sim])
}
