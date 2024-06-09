# Define the performance decay function
performance_decay <- function(initial_perf, decay_rate, time) {
  return(initial_perf * exp(-decay_rate * time))
}

# Define the overall performance function
overall_performance <- function(initial_perf, old_replaced_perf, new_perf, decay_rate, total_time, upgrade_time) {
  # Time step for splitting each year into 10 parts
  time_step <- 0.1
  num_steps <- total_time / time_step
  upgrade_step <- upgrade_time / time_step
  
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

# Example values
initial_perf <- 100
old_replaced_perf <- 30
new_perf <- 50
decay_rate <- 0.1
total_time <- 10
upgrade_time <- 5

# Calculate overall performance
result <- overall_performance(initial_perf, old_replaced_perf, new_perf, decay_rate, total_time, upgrade_time)
perf_overall <- result$perf_overall
time_points <- result$time_points

# Plot the performance
plot(time_points, perf_overall, type = "l", col = "blue", lwd = 2,
     xlab = "Time (years)", ylab = "Performance",
     main = "Computer Performance Over Time",
     pch = ifelse(time_points == upgrade_time, 19, 1), col.lab = "black")
abline(v = upgrade_time, col = "red", lty = 2)
text(upgrade_time, max(perf_overall), labels = "Upgrade", pos = 4, col = "red")
