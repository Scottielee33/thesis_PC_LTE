library(nloptr)
library(ggplot2)
library(data.table)

# Define the performance decay function
performance <- function(t, t_upgrade, perf_initial, lambda, perf_old_replaced, perf_new) {
  ifelse(
    t < t_upgrade,
    perf_initial * exp(-lambda * t),
    {
      perf_at_upgrade <- (perf_initial - perf_old_replaced) * exp(-lambda * t_upgrade) + perf_new
      perf_at_upgrade * exp(-lambda * (t - t_upgrade))
    }
  )
}

# Define the average performance function over a period
average_performance <- function(t_upgrade, perf_initial, lambda, perf_old_replaced, perf_new, period) {
  time_steps <- seq(0, period, by = 0.01)
  performance_values <- performance(time_steps, t_upgrade, perf_initial, lambda, perf_old_replaced, perf_new)
  mean(performance_values)
}

# Parameters
perf_initial <- 1000  # Initial performance
lambda <- 0.1         # Decay rate
perf_old_replaced <- 395  # Initial performance of the old component
perf_new_values <- c(400, 405, 400, 410, 420)  # Performance of the new components for each upgrade year
initial_period <- 5  # Initial period (in years)

# Function to get the performance upgrade value based on the upgrade time
get_perf_new <- function(t_upgrade) {
  year <- ceiling(t_upgrade)
  if (year > length(perf_new_values)) {
    return(perf_new_values[length(perf_new_values)])
  } else {
    return(perf_new_values[year])
  }
}

# Calculate the average performance over the initial period without any upgrade
initial_avg_perf_no_upgrade <- average_performance(Inf, perf_initial, lambda, perf_old_replaced, 0, initial_period)
cat("Average performance over the first 5 years without any upgrades:", initial_avg_perf_no_upgrade, "\n")

# Objective function to maximize the extended period while maintaining the average performance
objective_function <- function(t_upgrade) {
  perf_new <- get_perf_new(t_upgrade)
  extended_period <- initial_period
  while (TRUE) {
    avg_perf <- average_performance(t_upgrade, perf_initial, lambda, perf_old_replaced, perf_new, extended_period)
    if (avg_perf < initial_avg_perf_no_upgrade) {
      break
    }
    extended_period <- extended_period + 0.01  # Increased precision
  }
  return(-extended_period)  # Maximize the extended period
}

# Set up the optimization problem
lb <- 0  # Lower bound for t_upgrade
ub <- initial_period  # Upper bound for t_upgrade
initial_guess <- initial_period / 2

# Run the optimization using a different algorithm
result <- nloptr::nloptr(
  x0 = initial_guess,
  eval_f = objective_function,
  lb = lb,
  ub = ub,
  opts = list(algorithm = "NLOPT_GN_ISRES", xtol_rel = 1e-6, maxeval = 1000)
)

# Extract the optimal upgrade time
optimal_t_upgrade <- result$solution[1]
optimal_perf_new <- get_perf_new(optimal_t_upgrade)

# Calculate the optimal extended period
optimal_extended_period <- initial_period
while (TRUE) {
  avg_perf <- average_performance(optimal_t_upgrade, perf_initial, lambda, perf_old_replaced, optimal_perf_new, optimal_extended_period)
  if (avg_perf < initial_avg_perf_no_upgrade) {
    break
  }
  optimal_extended_period <- optimal_extended_period + 0.01  # Increased precision
}

# Print the results
cat("Optimal upgrade time:", format(optimal_t_upgrade, digits = 10), "years\n")
cat("Optimal extended period:", format(optimal_extended_period, digits = 10), "years\n")

# Generate performance data for plotting
time_points <- seq(0, optimal_extended_period, by = 0.1)
performance_data <- data.frame(
  Time = time_points,
  Performance = performance(time_points, optimal_t_upgrade, perf_initial, lambda, perf_old_replaced, optimal_perf_new)
)

# Plot the performance over time
ggplot(performance_data, aes(x = Time, y = Performance)) +
  geom_line(color = "blue") +
  geom_vline(xintercept = optimal_t_upgrade, linetype = "dashed", color = "red") +
  labs(title = "Performance Over Time with Optimal Upgrade",
       x = "Time (years)",
       y = "Performance") +
  annotate("text", x = optimal_t_upgrade, y = max(performance_data$Performance), label = paste("Upgrade at", round(optimal_t_upgrade, 2), "years"), hjust = -0.1) +
  geom_hline(yintercept = initial_avg_perf_no_upgrade, linetype = "dotted", color = "green") +
  annotate("text", x = optimal_extended_period * 0.7, y = initial_avg_perf_no_upgrade, label = "Initial Avg Performance", vjust = -1)
