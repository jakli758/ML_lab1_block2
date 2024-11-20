library(randomForest)

# Function to generate data
generate_data <- function(seed, condition) {
  if (!is.null(seed)) {
    set.seed(seed)
  }
  
  x1 <- runif(1000)
  x2 <- runif(1000)
  y <- eval(condition)
  data <- data.frame(
    x1 = x1,
    x2 = x2,
    y = as.factor(y)
  )
  return(data)
}

# Initialize results list
results <- list()

# Define conditions
conditions <- list(
  "x1 < x2" = list(cond = quote(x1 < x2), nodesize = 25),
  "x1 < 0.5" = list(cond = quote(x1 < 0.5), nodesize = 25),
  "checkerboard" = list(cond = quote(((x1 < 0.5 & x2 < 0.5) | (x1 > 0.5 & x2 > 0.5))), 
                        nodesize=12)
)


run_experiment <- function(){
  # Iterate through conditions
  for (condition_name in names(conditions)) {
    condition <- conditions[[condition_name]]$cond
    nodesize <- conditions[[condition_name]]$nodesize
    # Generate test data
    test_data <- generate_data(seed = 1234, condition = condition)
    
    # Error matrix to store errors for each seed and tree count
    error_matrix <- matrix(0, nrow = 1000, ncol = 3)
    
    for (i in 1:1000) { # Iterate over seeds
      # Generate training data
  
      data_set <- generate_data(seed = i, condition = condition)
      
      for (j in 1:3) { # Iterate over tree counts (1, 10, 100)
        ntree <- 10^(j - 1)
        model <- randomForest(y ~ x1 + x2, data = data_set, ntree = ntree, nodesize = nodesize)
        predictions <- predict(model, newdata = test_data)
        misclassified <- sum(test_data$y != predictions)
        error_matrix[i, j] <- misclassified / 1000
      }
    }
    
    # Calculate means and variances
    error_means <- colMeans(error_matrix)
    error_variances <- apply(error_matrix, 2, var)
    
    # Store results
    results[[condition_name]] <- list(
      means = error_means,
      variances = error_variances
    )
  }
  return(results)
}

