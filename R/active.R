

#' Ask the user for the probability two records match
#'
#' Prompt the user to input the probability that two records match
#'
#' @param record1 a row of data corresponding to the first record we are interested in
#'
#' @param record2 a row of data corresponding to the second record we are interested in
#'
#' @return The probability that the two records match
#'
#' @examples
#' ProbItsAMatch(RLdata500[1, ], RLdata[2, ])
ProbItsAMatch <- function(record1, record2){

  print("If two records definitely do not come from the same entity then their probability of matching is 0.")
  print("If two records most likely come from the same entity then their probability of matching is 0.8")
  print("If two records definitely come from the same entity then their probability of matching is 1.")

  print(record1)
  print(record2)

  user.prob.of.match <- readline("What is the probability that these two records match?  ")


}


# user wants unique ids for 100 records
# we have her tell us how many record pairs she wants to test... make a recommendation here
# we build a *model* on those trained record matches
# we use her predictions as the ground truth for trained records
# apply combination of *model* + similarity scores to the untrained records
# spit out unique ids
# ask how many matches we think there would be?
# doesn't make sense to have a training set of 30% matches if dataset has 1%
# how many records would you like to match at first?
# you've matched __ records, would you like to keep going?
# do we trust user as ground truth?


#' Ask the user if two records match
#'
#' Prompt the user to input yes or no (y/n) if two records match
#'
#' @param record1 a row of data corresponding to the first record we are interested in
#'
#' @param record2 a row of data corresponding to the second record we are interested in
#'
#' @return y/n if the two records match
#'
#' @examples
#' IsItAMatch(RLdata500[1, ], RLdata[2, ])
#'
#' @export
IsItAMatch <- function(record1, record2){

  print("If two records do not come from the same entity then they are not a match.")
  print("If two records come from the same entity then they are a match.")

  print(record1)
  print(record2)

  y.n.match <- readline("Are the following two records a match? y/n? ")


}







#' Build a training dataset
#'
#' Build a training dataset from user input about whether records in the dataset match
#'
#' @param RLdata a data frame containing the records to be matched
#'
#' @param n.pairs.to.test an integer corresponding to the number of pairs of records the user wants to test
#'
#' @param variables.to.match a vector of strings containing the variables of interest for this linkage. Default is all variables in RLdata. Can repeat variables to use different comparators on same variable.
#'
#' @param string.comparators a vector of strings containing the comparator to be used for each variable. Default is jarowinkler. This should be same length as variables.to.match.
#'
#' @param current.record.ids a vector of strings corresponding to the variable names that will contain the pairwise combinations of records. The default (which is produced using CompareUniqueCombinations) is c("CurrentRecord1", "CurrentRecord2").
#'
#' @param standardized.variables a vector of strings containing the names of all standardized variables. The comparison values for these variables will be averaged. The default is all factor variables in RLdata.
#'
#' @return A list with the elements
#' \item{comparisons}{a data frame containing the comparisons of RLdata}
#' \item{tested.comparisons}{a data frame containing the comparison values of records the user tested}
#' \item{tested.data}{a data frame containing the original data of records the user tested}
#' @export
BuildATrainingDataset <- function(RLdata,
                                  n.pairs.to.test,
                                  variables.to.match=NULL,
                                  string.comparators=NULL,
                                  current.record.ids=NULL,
                                  standardized.variables=NULL,
                                  seed=NULL){

  if(is.null(current.record.ids)){
    current.record.ids <- c("CurrentRecord1", "CurrentRecord2")
  } else{
    current.record.ids <- current.record.ids
  }

  if(is.null(seed)){
    seed <- sample(1:100000, 1)
  } else{
    seed <- seed
  }


  comparisons <- CompareUniqueCombinations(RLdata = RLdata,
                                           variables.to.match = variables.to.match,
                                           string.comparators = string.comparators)
  comparisons <- as.data.frame(comparisons)

  if(is.null(standardized.variables)){
    factor.vars <- names(which(sapply(RLdata[, unique(variables.to.match)], is.factor) == TRUE))
    factor.vars <- variables.to.match %>%
                    unique() %>%
                     RLdata[, .] %>%
                      sapply(., is.factor) %>%
                       which(. == TRUE) %>%
                        names()
    std.cols <- which(variables.to.match %in% factor.vars)
  } else{
    std.cols <- which(variables.to.match %in% standardized.variables)
  }


  average.similarity <- apply(comparisons[, std.cols], 1, mean, na.rm=T)
  avg.sim.n <- length(average.similarity)

  avg.sims <- data.frame(n=1:avg.sim.n, avg.sim=average.similarity)

  avg.sims.ordered <- avg.sims[order(avg.sims$avg.sim),]


  bot <- 1:round(n.pairs.to.test/4)

  top <- (avg.sim.n - round(n.pairs.to.test/4)):avg.sim.n

  mid <- n.pairs.to.test - length(c(bot, top))

  set.seed(seed)


  rest <- sample((max(bot) + 1):(min(top) -1), mid, replace = FALSE)


  sims.to.test <- avg.sims.ordered[c(bot, top, rest), ]


  user.matchYN <- c()

  orig.recs <- c()

  for(i in sims.to.test$n){
    r1 <- comparisons[i , which(colnames(comparisons) %in% current.record.ids[1])]
    r2 <- comparisons[i , which(colnames(comparisons) %in% current.record.ids[2])]
    orig.recs <- c(orig.recs, r1, r2)

    user.mat <- IsItAMatch(RLdata[r1, ], RLdata[r2, ])
    user.matchYN <- c(user.matchYN, user.mat)

  }

  user.match <- ifelse(user.matchYN == "y", 1, 0)

  comparisons$Active_Match <- rep(NA, nrow(comparisons))
  comparisons$Active_Match[sims.to.test$n] <- user.match

  results <- list(comparisons=comparisons,
                  tested.comparisons=comparisons[sims.to.test$n, ],
                  tested.data=RLdata[unique(orig.recs), ],
                  seed=seed)
  return(results)

}






BuildATrainingDatasetAuto <- function(RLdata,
                                      n.pairs.to.test,
                                      unique.ids,
                                      variables.to.match=NULL,
                                      string.comparators=NULL,
                                      current.record.ids=NULL,
                                      standardized.variables=NULL,
                                      seed=NULL,
                                      props.from.each.bin=NULL){

  if(is.null(current.record.ids)){
    current.record.ids <- c("CurrentRecord1", "CurrentRecord2")
  } else{
    current.record.ids <- current.record.ids
  }

  if(is.null(seed)){
    seed <- sample(1:100000, 1)
  } else{
    seed <- seed
  }

  if(is.null(props.from.each.bin)){
    props.from.each.bin <- c(.1, 0, 0, 0, .4, .4, 0, 0, 0, .1)
  } else{
    props.from.each.bin <- props.from.each.bin
  }


  comparisons <- CompareUniqueCombinations(RLdata = RLdata,
                                           unique.ids = unique.ids,
                                           variables.to.match = variables.to.match,
                                           string.comparators = string.comparators)
  comparisons <- as.data.frame(comparisons)

  if(is.null(standardized.variables)){
    factor.vars <- names(which(sapply(RLdata[, unique(variables.to.match)], is.factor) == TRUE))
    factor.vars <- variables.to.match %>%
      unique() %>%
      RLdata[, .] %>%
      sapply(., is.factor) %>%
      which(. == TRUE) %>%
      names()
    std.cols <- which(variables.to.match %in% factor.vars)
  } else{
    std.cols <- which(variables.to.match %in% standardized.variables)
  }


  average.similarity <- apply(comparisons[, std.cols], 1, mean, na.rm=T)
  avg.sim.n <- length(average.similarity)

  avg.sims <- data.frame(n=1:avg.sim.n, avg.sim=average.similarity)

  avg.sims.ordered <- avg.sims[order(avg.sims$avg.sim),]


  bot <- 1:round(n.pairs.to.test/4)

  top <- (avg.sim.n - round(n.pairs.to.test/4)):avg.sim.n

  mid <- n.pairs.to.test - length(c(bot, top))

  set.seed(seed)


  rest <- sample((max(bot) + 1):(min(top) -1), mid, replace = FALSE)


  sims.to.test <- avg.sims.ordered[c(bot, top, rest), ]


  user.matchYN <- c()

  orig.recs <- c()

  for(i in sims.to.test$n){
    r1 <- comparisons[i , which(colnames(comparisons) %in% current.record.ids[1])]
    r2 <- comparisons[i , which(colnames(comparisons) %in% current.record.ids[2])]
    orig.recs <- c(orig.recs, r1, r2)

    user.mat <- IsItAMatch(RLdata[r1, ], RLdata[r2, ])
    user.matchYN <- c(user.matchYN, user.mat)

  }

  user.match <- ifelse(user.matchYN == "y", 1, 0)

  comparisons$Active_Match <- rep(NA, nrow(comparisons))
  comparisons$Active_Match[sims.to.test$n] <- user.match

  results <- list(comparisons=comparisons,
                  tested.comparisons=comparisons[sims.to.test$n, ],
                  tested.data=RLdata[unique(orig.recs), ],
                  seed=seed)
  return(results)

}





