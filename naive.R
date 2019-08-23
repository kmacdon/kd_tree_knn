euclidean_dist <- function(a, b){
  sqrt(sum((a-b)^2))
}

nearest_neighbor_naive <- function(training, true, testing){
  neighbors <- numeric(nrow(testing))
  
  # loop through all testing and compare to all training
  for(i in 1:nrow(testing)){
    min <- NULL
    neighbor <- NULL
    for(j in 1:nrow(training)){
      dist <- euclidean_dist(testing[i, ], training[j, ])
      if(is.null(min)|| dist < min){
        min <- dist
        neighbor <- j
      }
    }
    neighbors[i] <- true[neighbor]
    
  }
  
  neighbors
}
