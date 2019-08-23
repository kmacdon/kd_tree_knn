kd_tree <- function(data, classes, depth = 1){
  
  # Check if data is matrix or cvector
  if(class(data) == "matrix"){
    depth <- depth %% ncol(data)
    
    if(!depth){
      # In case depth is 0 
      depth <- ncol(data)
    }
  } else {
    depth <- depth %% length(data)
    
    if(!depth){
      # In case depth is 0 
      depth <- length(data)
    }
  }
  
  
  # If only one point, make node
  if(is.null(dim(data))){
    
    # Check to see if data is empty
    if(is.na(data[depth])){
      return(NULL)
    }
    
    # Else return node
    return(list(
      axis = depth,
      value = data[depth],
      point = data,
      class = classes,
      left = NULL,
      right = NULL
    ))
  }
  
  
  # Use sorting and integer division to find median
  # Need to ensure median corresponds to actual data point
  
  middle <- nrow(data) %/% 2
  data_order <- order(data[, depth])
  
  # Split data excluding median point
  left_data <- data[data_order[1:(middle-1)], ]
  right_data <- data[data_order[(middle+1):nrow(data)], ]
  middle_point <- data[data_order[middle], ]
  
  return(list(
    axis = depth,
    value = middle_point[depth],
    point = middle_point,
    class = classes[data_order[middle]],
    left = kd_tree(left_data, classes[data_order[1:(middle-1)]], depth + 1),
    right = kd_tree(right_data, classes[data_order[(middle+1):nrow(data)]], depth + 1)
  ))
  
}

euclidean_dist <- function(a, b){
  sqrt(sum((a-b)^2))
}

find_neighbor <- function(tree, point, best = list(class = NULL, dist = NULL)){
  
  # Go back up one if null node
  if(is.null(tree)){
    return(best)
  }
  
  if(point[tree$axis] < tree$value){
    best <- find_neighbor(tree$left, point, best)
  } else {
    best <- find_neighbor(tree$right, point, best)
  }
  

  # Compare current node to best or set it as best if none set
  if(is.null(best$dist) || euclidean_dist(tree$point, point) < best$dist){
    best <- list(class = tree$class,
                 dist = euclidean_dist(tree$point, point))
  }
  
  # Check to see if the other branch needs to be checked 
  
  if(abs(tree$value - point[tree$axis]) < best$dist){
    if(point[tree$axis] > tree$value){
      best <- find_neighbor(tree$left, point, best)
    } else {
      best <- find_neighbor(tree$right, point, best)
    }
  }
  
  best
}

nearest_neighbor_tree <- function(training, classes, testing){
  neighbors <- numeric(nrow(testing))
  tree <- kd_tree(training, classes)
  for(i in 1:nrow(testing)){
      
    neighbors[i] <- find_neighbor(tree, testing[i, ])$class
    
  }
  
  neighbors
}
  