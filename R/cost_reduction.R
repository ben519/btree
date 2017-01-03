cost_reduction <- function(left.vals, right.vals, costFunc="mse", verbose=FALSE){
  # Calculate the cost reduction by splitting a node into the given left and right nodes
  # Right now the only implemented costFunc is "mse"

  if(length(left.vals) == 0 | length(right.vals) == 0){
    if(verbose) print("One of left.vals, right.vals is empty so reduction is 0")
    return(0)
  }

  if(costFunc == "mse"){

    # Get the MSE in the root
    root.vals <- c(left.vals, right.vals)
    root.pred <- mean(root.vals)
    root.cost <- mean((root.vals - root.pred)^2)

    # MSE in left child
    left.pred <- mean(left.vals)
    left.cost <- mean((left.vals - left.pred)^2)
    left.weight <- length(left.vals)/(length(left.vals) + length(right.vals))

    # MSE in right child
    right.pred <- mean(right.vals)
    right.cost <- mean((right.vals - right.pred)^2)
    right.weight <- length(right.vals)/(length(left.vals) + length(right.vals))
  }

  # Weighted MSE in children
  children.cost <- left.weight * left.cost + right.weight * right.cost

  # Get the cost reduction
  reduction <- root.cost - children.cost

  if(verbose){
    print(paste0(
      "MSE in root: ", root.cost,
      " | MSE in left: ", left.cost, ", weight: ", left.weight,
      " | MSE in right: ", right.cost, ", weight: ", right.weight,
      " | Weighted MSE in children: ", children.cost
    ))
  }

  return(reduction)
}

# cost_reduction(numeric(0), c(1, 2), verbose=TRUE)
# cost_reduction(c(1), c(2), verbose=TRUE)
# cost_reduction(c(1,1), c(2,2), verbose=TRUE)
# cost_reduction(c(1,2), c(1,2), verbose=TRUE)
# cost_reduction(c(1,2), c(3,4), verbose=TRUE)
#
# cost_reduction(c(15, 18, 22, 55), c(68, 71), verbose=TRUE)  # 392
# cost_reduction(c(22, 55, 68, 71), c(15, 18), verbose=TRUE)  # 312.5
#
# cost_reduction(c(12.5, 9.5, 5.5, -27.5), c(1.5, -1.5), verbose=TRUE)  # 0
# cost_reduction(c(5.5, -27.5, 1.5, -1.5), c(12.5, 9.5), verbose=TRUE)  # 60.5
