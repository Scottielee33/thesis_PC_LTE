# Install and load necessary packages
if (!require("nloptr")) install.packages("nloptr")
if (!require("numDeriv")) install.packages("numDeriv")
if (!require("ggplot2")) install.packages("ggplot2")

library(nloptr)
library(numDeriv)
library(ggplot2)

# Example deterioration rates and prices for each component and each quartile
Q1_performances <- c(0.02, 0.03, 0.015, 0.025)
Q2_performances <- c(0.018, 0.028, 0.012, 0.022)
Q3_performances <- c(0.016, 0.025, 0.01, 0.02)

Q1_prices <- c(100, 200, 50, 75)
Q2_prices <- c(150, 300, 75, 100)
Q3_prices <- c(200, 400, 100, 125)

# Optimization function
My_optimization <- function(U_i, tier, x, lower_bound, upper_bound, initial_values) {
  
  # Parameters
  i <- c(1, 2, 3, 4) # 1 for CPU, 2 for GPU, 3 for Disks, 4 for Memory
  alpha <- 1 # alpha at 100%
  
  # Define deterioration rates and prices based on tier
  if (tier == 1) {
    deterioration_rate <- Q1_performances
    prices <- Q1_prices
  } else if (tier == 2) {
    deterioration_rate <- Q2_performances
    prices <- Q2_prices
  } else if (tier == 3) {
    deterioration_rate <- Q3_performances
    prices <- Q3_prices
  } else {
    stop("Invalid tier")
  }
  
  # Objective Function (T_CUP = z[1], T_UM = z[2])
  evaluation_function <- function(z) {
    return((sum(prices + prices * U_i)) / z[1])
  }
  
  # Constraint function
  constraint <- function(z) {
    sum_alpha_deterioration <- sum((alpha / deterioration_rate) * (alpha - exp(-deterioration_rate * z[2])))
    sum_performance_maintenance <- sum((alpha / deterioration_rate) * (exp(-deterioration_rate * z[2]) - exp(-deterioration_rate * z[1])) * (1 - U_i))
    sum_performance_upgrade <- sum((alpha / deterioration_rate) * (1 - exp(-deterioration_rate * (z[1] - z[2]))) * U_i)
    
    constraint_value <- (sum_alpha_deterioration + sum_performance_maintenance + sum_performance_upgrade) / (z[1] * 4) - 
      (sum((alpha / deterioration_rate) * (1 - exp(-deterioration_rate * x))) / (4 * x))
    
    return(constraint_value)
  }
  
  # Gradient of the objective function
  gradient_function <- function(z) {
    grad <- grad(evaluation_function, z)
    return(grad)
  }
  
  # Gradient of the constraint function
  constraint_gradient <- function(z) {
    jac <- jacobian(constraint, z)
    return(jac)
  }
  
  # Optimization settings
  local_opts <- list("algorithm" = "NLOPT_LD_SLSQP", "xtol_rel" = 1.0e-15)
  opts <- list(
    "algorithm" = "NLOPT_LD_SLSQP",
    "xtol_rel" = 1.0e-15,
    "maxeval" = 2560000,
    "local_opts" = local_opts,
    "print_level" = 0
  )
  
  result <- nloptr(
    x0 = initial_values,
    eval_f = evaluation_function,
    eval_grad_f = gradient_function,
    lb = lower_bound,
    ub = upper_bound,
    eval_g_eq = constraint,
    eval_jac_g_eq = constraint_gradient,
    opts = opts
  )
  
  return(result)
}

# Function to run the optimization
run_optimization <- function(U_i, tier, x_values) {
  optimization_results <- lapply(x_values, function(x) {
    lower_bound <- c(x, 0)         # TCUP >= TFRP (x), TUM >= 0
    upper_bound <- c(15, x)        # TCUP <= 15, TUM <= TCUP (which is x in this context)
    initial_values <- c((x + 15) / 2, x / 2)  # Start in the middle of the possible range
    result <- My_optimization(U_i, tier, x, lower_bound, upper_bound, initial_values)
    
    # Print debug information
    cat("TFRP:", x, "\n")
    print(result)
    
    return(result)
  })
  
  component_names <- c("cpu", "gpu", "disk", "memory")
  selected_names <- component_names[U_i == 1]
  quantile_names <- c("q1", "q2", "q3")
  quantile_name <- quantile_names[tier]
  
  if (sum(U_i) < 1) {
    var_name <- paste0("Replacement_Policy_optimization_result_", quantile_name)
  } else {
    var_name <- paste0(paste(selected_names, collapse = "_"), "_optimization_result_", quantile_name)
  }
  
  assign(var_name, optimization_results, envir = .GlobalEnv)
  
  # Return results for further inspection
  return(optimization_results)
}

# Example run of the optimization:
# Define the component to upgrade (U_i), time of upgrade (x_values), and the component tier (tier)
U_i <- c(1, 1, 1, 0)  # Upgrade CPU
x_values <- seq(1, 7.5, by = 0.1)
tier <- 1  # Q1

# Run the optimization
results <- run_optimization(U_i, tier, x_values)

# Inspect results
print(results)

# Extracting results for visualization
results_summary <- data.frame(
  TFRP = x_values,
  ObjectiveValue = sapply(results, function(res) res$objective),
  TCUP = sapply(results, function(res) res$solution[1]),
  TUM = sapply(results, function(res) res$solution[2])
)

# Visualizing the results
ggplot(results_summary, aes(x = TFRP)) +
  geom_line(aes(y = ObjectiveValue), color = "blue") +
  geom_point(aes(y = ObjectiveValue), color = "blue") +
  labs(title = "Objective Function Value vs TFRP",
       x = "TFRP (Full Replacement Policy Cycle Length)",
       y = "Objective Function Value (Cost per Unit Time)") +
  theme_minimal()

ggplot(results_summary, aes(x = TFRP)) +
  geom_line(aes(y = TCUP), color = "red") +
  geom_point(aes(y = TCUP), color = "red") +
  geom_line(aes(y = TUM), color = "green") +
  geom_point(aes(y = TUM), color = "green") +
  labs(title = "TCUP and TUM vs TFRP",
       x = "TFRP (Full Replacement Policy Cycle Length)",
       y = "Cycle Length / Upgrade Moment") +
  theme_minimal() +
  scale_y_continuous(sec.axis = sec_axis(~ ., name = "TUM", breaks = seq(0, 15, by = 1)))
