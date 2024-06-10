library(nloptr)
library(ggplot2)

# Define the performance decay function
performance <- function(t, t_upgrade, perf_initial, lambda, perf_old_replaced, perf_new) {
  if (t < t_upgrade) {
    return(perf_initial * exp(-lambda * t))
  } else {
    perf_at_upgrade <- (perf_initial - perf_old_replaced) * exp(-lambda * t_upgrade) + perf_new
    return(perf_at_upgrade * exp(-lambda * (t - t_upgrade)))
  }
}

# Define the average performance function over a period
average_performance <- function(t_upgrade, perf_initial, lambda, perf_old_replaced, perf_new, period) {
  total_perf <- 0
  time_steps <- seq(0, period, by = 0.01)
  for (t in time_steps) {
    total_perf <- total_perf + performance(t, t_upgrade, perf_initial, lambda, perf_old_replaced, perf_new)
  }
  return(total_perf / length(time_steps))
}

# Parameters
perf_initial <- 100  # Initial performance
lambda <- 0.1        # Decay rate
perf_old_replaced <- 5  # Initial performance of the old component
perf_new <- 10  # Performance of the new component
initial_period <- 5  # Initial period (in years)

# Calculate the average performance over the initial period without any upgrade
initial_avg_perf_no_upgrade <- average_performance(Inf, perf_initial, lambda, perf_old_replaced, 0, initial_period)

# Print the average performance over the first 5 years without any upgrades
cat("Average performance over the first 5 years without any upgrades:", initial_avg_perf_no_upgrade, "\n")

# Define the objective function to maximize the extended period while maintaining the average performance
objective_function <- function(params) {
  t_upgrade <- params[1]
  extended_period <- params[2]
  avg_perf <- average_performance(t_upgrade, perf_initial, lambda, perf_old_replaced, perf_new, extended_period)
  return(-extended_period)  # Maximize the extended period
}

# Constraint function to ensure the average performance is maintained
constraint_function <- function(params) {
  t_upgrade <- params[1]
  extended_period <- params[2]
  avg_perf <- average_performance(t_upgrade, perf_initial, lambda, perf_old_replaced, perf_new, extended_period)
  return(avg_perf - initial_avg_perf_no_upgrade)
}

# Set up the optimization problem
lb <- c(0, initial_period)  # Lower bounds for t_upgrade and extended_period
ub <- c(initial_period, 20)  # Upper bounds for t_upgrade and extended_period (arbitrary large number for extended period)
initial_guess <- c(initial_period / 2, initial_period * 2)

# Run the optimization
result <- nloptr::nloptr(
  x0 = initial_guess,
  eval_f = objective_function,
  lb = lb,
  ub = ub,
  eval_g_ineq = function(x) constraint_function(x),
  opts = list(algorithm = "NLOPT_LN_COBYLA", xtol_rel = 1e-6, maxeval = 1000)
)

# Extract the optimal upgrade time and extended period
optimal_t_upgrade <- result$solution[1]
optimal_extended_period <- result$solution[2]

# Print the results
cat("Optimal upgrade time:", optimal_t_upgrade, "years\n")
cat("Optimal extended period:", optimal_extended_period, "years\n")

# Generate performance data for plotting
time_points <- seq(0, optimal_extended_period, by = 0.1)
performance_data <- data.frame(
  Time = time_points,
  Performance = sapply(time_points, function(t) performance(t, optimal_t_upgrade, perf_initial, lambda, perf_old_replaced, perf_new))
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
