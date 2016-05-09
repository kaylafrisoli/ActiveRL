


#' Split data into training and testing
#'
#' Split a dataset with unique identifiers into training and testing datasets with a specified probability
#'
#' @param RLdata a data frame containing the records to be matched
#'
#' @param unique.ids a vector containing the true unique identifiers of the records in RLdata. It should be of length nrow(RLdata)
#'
#' @param seed an integer specifying the seed to set before randomly dividing RLdata
#'
#' @param prob.of.train a probability between 0 and 1 that a given record is in the training dataset
#'
#' @return
#' \item{seed}{the seed used}
#' \item{prob.of.train}{the probability a record is in the training dataset}
#' \item{train}{a vector of TRUE/FALSE values corresponding to whether the record is in the training dataset}
#' \item{test}{a vector of TRUE/FALSE values corresponding to whether the record is in the testing dataset}
#' \item{training.data}{the training dataset}
#' \item{testing.data}{the testing dataset}
#' \item{train.ids}{unique ids for the training dataset}
#' \item{test.ids}{unique ids for the testing dataset}
#' @export
SplitIntoTrainTest <- function(RLdata,
                               unique.ids,
                               seed=NULL,
                               prob.of.train=NULL){

  if(is.null(seed)){
    seed <- sample(1:100000, 1)
  } else{
    seed <- seed
  }

  if(is.null(prob.of.train)){
    prob.of.train <- .5
  } else{
    prob.of.train <- prob.of.train
  }

  set.seed(seed)
  train <- sample(c(TRUE,FALSE), nrow(RLdata), rep=TRUE, prob = c(prob.of.train, 1-prob.of.train))
  test <- (! train)

  RLdata$PreSplitRecord <- 1:nrow(RLdata)
  training.data <- RLdata[train, ]
  testing.data  <- RLdata[test, ]
  train.id <- unique.ids[train]
  test.id <- unique.ids[test]

  results <- list(seed=seed,
                  prob.of.train=prob.of.train,
                  train=train,
                  test=test,
                  training.data=training.data,
                  testing.data=testing.data,
                  train.id=train.id,
                  test.id=test.id)

  return(results)

}
