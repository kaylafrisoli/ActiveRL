#' Block by variable
#'
#' Block a dataset by the factor levels of one variable
#'
#' @param records Data Frame containing records to be linked
#' @param var.name String of variable you want to block by
#' @return A printed vector of integers coresponding to the block
#' @examples
#' BlockByVariable(iris, "Species")
BlockByVariable <- function(records, var.name) {
  # we could either make the user put quotes on "species" or fix it in the function using deparse(substitute(var.name)) or even do an if statement
  as.integer(as.factor(records[, var.name]))
}

#' Block by substring
#'
#' Block by substrings of up to 4 variables
#'
#' @param records Data Frame containing records to be linked
#' @param var.name String of variable you want to block by
#' @param n.chars Number of characters you want to compare, default is 1
#' @return A printed vector of integers coresponding to the block
#' @examples
#' BlockByInitials(iris, "Species") #identifies 2 blocks
#' BlockByInitials(iris, "Species", 2) #identifies 3 blocks
BlockBySubstrMV <- function(records, var1.name, n1.chars=1, var2.name=NULL, n2.chars=1, var3.name=NULL, n3.chars=1, var4.name=NULL, n4.chars=1) {
  as.integer(as.factor(paste(substr(records[, var1.name], 1, n1.chars),
                             substr(records[, var2.name], 1, n2.chars),
                             substr(records[, var3.name], 1, n3.chars),
                             substr(records[, var4.name], 1, n4.chars), sep="")))
}


#' Block by substrings of multiple variables
#'
#' Block by substrings of any length of multiple variables
#'
#' @param records Data Frame containing records to be linked
#' @param var.names Vector of strings of variables you want to block by
#' @param n.chars Vector of number of the characters you want to compare, respective to var.names
#' @return A printed vector of integers coresponding to the block
#' @examples
#' BlockBySubstrMV2(iris, "Species") #identifies 2 blocks
#' BlockBySubstrMV2(iris, "Species", 2) #identifies 3 blocks
#' BlockBySubstrMV2(iris, c("Species", "Sepal.Length"), c(2,1)) #identifies 3 blocks
BlockBySubstrMV2 <- function(records, var.names, n.chars=1) {
  f1 <- function(x){substr(x, start=1, stop=n.chars)}
  new.mat <- t(apply(as.matrix(records[,var.names]), 1, f1))
  if (length(var.names) == 1){
    as.integer(as.factor(apply(new.mat, 2, paste, collapse="")))
  } else {
    as.integer(as.factor(apply(new.mat, 1, paste, collapse="")))
  }
}





