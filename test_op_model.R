# Define the performance decay function
performance_decay <- function(initial_perf, decay_rate, time) {
  return(initial_perf * exp(-decay_rate * time))
}

# Define the overall performance function
overall_performance <- function(initial_perf, old_replaced_perf, new_perf, decay_rate, total_time, upgrade_time) {
  # Time step for splitting each year into 10 parts
  time_step <- 0.1
  num_steps <- total_time / time_step
  upgrade_step <- round(upgrade_time / time_step)
  
  # Initialize a vector to store overall performance
  perf_overall <- numeric(num_steps + 1)
  time_points <- seq(0, total_time, by = time_step)
  
  # Calculate performance before the upgrade
  for (i in 1:upgrade_step) {
    t <- time_points[i]
    perf_overall[i] <- performance_decay(initial_perf, decay_rate, t)
  }
  
  # Calculate performance at the upgrade time
  t_upgrade <- time_points[upgrade_step + 1]
  perf_initial_decay <- performance_decay(initial_perf, decay_rate, t_upgrade)
  perf_old_replaced_decay <- performance_decay(old_replaced_perf, decay_rate, t_upgrade)
  perf_at_upgrade <- perf_initial_decay - perf_old_replaced_decay + new_perf
  
  # Ensure the performance jumps correctly at the upgrade point
  perf_overall[upgrade_step + 1] <- perf_at_upgrade
  
  # Calculate performance after the upgrade
  for (i in (upgrade_step + 2):(num_steps + 1)) {
    t <- time_points[i]
    time_after_upgrade <- t - t_upgrade
    perf_overall[i] <- perf_at_upgrade * exp(-decay_rate * time_after_upgrade)
  }
  
  return(list(perf_overall = perf_overall, time_points = time_points))
}

# Define the objective function to maximize average performance
objective_function <- function(upgrade_time, initial_perf, old_replaced_perf, new_perf, decay_rate, total_time, target_time) {
  result <- overall_performance(initial_perf, old_replaced_perf, new_perf, decay_rate, total_time, upgrade_time)
  perf_overall <- result$perf_overall
  time_points <- result$time_points
  
  # Calculate the average performance over the target time period
  avg_performance <- mean(perf_overall[time_points <= target_time])
  return(-avg_performance)  # Negative because we want to maximize this value
}

# Example values
initial_perf <- 1000
old_replaced_perf <- 200
new_perf <- 210
decay_rate <- 0.1
total_time <- 15

target_time <- 20

# Optimize the upgrade time to maximize the average performance over the target time period
opt_result <- optim(par = 5, fn = objective_function, method = "L-BFGS-B",
                    lower = 0, upper = total_time,
                    initial_perf = initial_perf, old_replaced_perf = old_replaced_perf,
                    new_perf = new_perf, decay_rate = decay_rate, total_time = total_time, target_time = target_time)

# Get the optimal upgrade time
optimal_upgrade_time <- opt_result$par

cat("Optimal upgrade time:", optimal_upgrade_time, "\n")

# Calculate the overall performance with the optimal upgrade time
result_optimal <- overall_performance(initial_perf, old_replaced_perf, new_perf, decay_rate, total_time, optimal_upgrade_time)
perf_overall_optimal <- result_optimal$perf_overall
time_points_optimal <- result_optimal$time_points

# Plot the performance
plot(time_points_optimal, perf_overall_optimal, type = "l", col = "blue", lwd = 2,
     xlab = "Time (years)", ylab = "Performance",
     main = "Optimized Computer Performance Over Time")
abline(v = optimal_upgrade_time, col = "red", lty = 2)
text(optimal_upgrade_time, max(perf_overall_optimal), labels = "Optimal Upgrade", pos = 4, col = "red")
