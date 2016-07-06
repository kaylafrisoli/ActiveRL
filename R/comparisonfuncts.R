


StandardizeBetween01 <- function(x, our.max, our.min=NULL){
  if(is.null(our.min)){
    our.min <- 0
  } else{
    our.min <- our.min
  }
  our.min <- as.numeric(our.min)
  our.max <- as.numeric(our.max)
  x <- as.vector(x)
  (x-our.max)/(our.min-our.max)
}



AbsoluteDistance <- function(vec1, vec2){
  vec1 <- as.numeric(vec1)
  vec2 <- as.numeric(vec2)
  max.val <- max(c(vec1, vec2))
  ab.dist <- abs(vec1 - vec2)
  min.dist <- pmin(ab.dist, max.val-ab.dist)
  return(min.dist)
}

StandardizedAbsoluteDistance <- function(vec1, vec2){
  vec1 <- as.numeric(vec1)
  vec2 <- as.numeric(vec2)
  max.val <- max(c(vec1, vec2))
  ab.dist <- abs(vec1 - vec2)
  min.dist <- pmin(ab.dist, max.val-ab.dist)

  # Standardize
  return(StandardizeBetween01(min.dist))
}



StandardizedAbsoluteDistanceM <- function(vec1, vec2){
  vec1 <- as.numeric(vec1)
  vec2 <- as.numeric(vec2)
  max.val <- max(c(vec1, vec2))
  ab.dist <- abs(vec1 - vec2)
  min.dist <- pmin(ab.dist, max.val-ab.dist)

  # Standardize
  return(StandardizeBetween01(min.dist, 12))
}


StandardizedAbsoluteDistanceD <- function(vec1, vec2){
  vec1 <- as.numeric(vec1)
  vec2 <- as.numeric(vec2)
  max.val <- max(c(vec1, vec2))
  ab.dist <- abs(vec1 - vec2)
  min.dist <- pmin(ab.dist, max.val-ab.dist)

  # Standardize
  return(StandardizeBetween01(min.dist, 31))
}



AbsoluteDifference <- function(vec1, vec2){
  ab.dif <- as.numeric(abs(as.numeric(vec1)-as.numeric(vec2)))
  return(ab.dif)
}

AbsoluteDifferenceDate <- function(vec1, vec2){
  ab.dif <- round(as.numeric(abs(difftime(vec1 , vec2, units="days"))))
  return(ab.dif)
}



StandardizedAbsoluteDifference <- function(vec1, vec2){
  ab.dif <- abs(as.numeric(vec1)-as.numeric(vec2))
  return(StandardizeBetween01(ab.dif))
}

StandardizedAbsoluteDifferenceY <- function(vec1, vec2){
  ab.dif <- abs(as.numeric(vec1)-as.numeric(vec2))
  return(StandardizeBetween01(ab.dif, max(c(vec1, vec2, 65))))
}



ExactMatch <- function(vec1, vec2){

  exact.match <- 1* (vec1 == vec2)
  return(exact.match)

}






