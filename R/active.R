

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

  recs <- rbind(record1, record2)

  print(recs)

  y.n.match <- readline("Are the previous two records a match? y/n? ")


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
                  seed=seed,
                  user.match=user.match)
  return(results)

}


which.n.closest.to <- function(n, vec, k){
  ab.dif <- abs(vec - k)
  which.in <- which(ab.dif %in% sort(ab.dif)[1:n])
  return(sample(which.in, n))

}


#'
#' #' Build a training dataset automatically
#' #'
#' #' Build a training dataset from user input about whether records in the dataset match
#' #'
#' #' @param RLdata a data frame containing the records to be matched
#' #'
#' #' @param n.pairs.to.test an integer corresponding to the number of pairs of records the user wants to test
#' #'
#' #' @param record.ids a vector of strings corresponding to the variable names that will contain the pairwise combinations of records. The default (which is produced using CompareUniqueCombinations) is c("CurrentRecord1", "CurrentRecord2").
#' #'
#' #' @param standardized.variables a vector of strings containing the names of all standardized variables. The comparison values for these variables will be averaged. The default is all factor variables in RLdata.
#' #'
#' #' @return A list with the elements
#' #' \item{comparisons}{a data frame containing the comparisons of RLdata}
#' #' \item{tested.comparisons}{a data frame containing the comparison values of records the user tested}
#' #' \item{untested.comparisons}{a data frame containing the comparison values of records the user did not test}
# BuildATrainingDatasetAuto <- function(RLdata,
#                                       block.comparisons,
#                                       standardized.variables,
#                                       model.formula,
#                                       n.pairs.to.test=NULL,
#                                       n.per.stage=NULL,
#                                       n.initial.stage=NULL,
#                                       initial.matching.scheme=NULL,
#                                       record.id=NULL,
#                                       user.accuracy=NULL,
#                                       uncertainty.param=NULL,
#                                       seed=NULL,
#                                       cut.threshold=NULL){
#
#   if(is.null(cut.threshold)){
#     cut.threshold <- .5
#   } else{
#     cut.threshold <- cut.threshold
#   }
#
#   if(is.null(seed)){
#     seed <- sample(1:100000, 1)
#   } else{
#     seed <- seed
#   }
#
#   set.seed(seed)
#
#   if(is.null(n.pairs.to.test)){
#     n.pairs.to.test <- 100
#   } else{
#     n.pairs.to.test <- n.pairs.to.test
#   }
#
#   if(is.null(n.per.stage)){
#     n.per.stage <- 20
#   } else{
#     n.per.stage <- n.per.stage
#   }
#
#   if(is.null(n.initial.stage)){
#     n.initial.stage <- 1000
#   } else{
#     n.initial.stage <- n.initial.stage
#   }
#
#   if(is.null(uncertainty.param)){
#     uncertainty.param <- .5
#   } else{
#     uncertainty.param <- uncertainty.param
#   }
#
#   stages <- n.pairs.to.test/n.per.stage
#
#   if(is.null(record.id)){
#     record.id <- c("PreBlockRecord1", "PreBlockRecord2")
#   } else{
#     record.id <- record.id
#   }
#
#
#
#   #comparisons
#   comparisons <- MergeAllBlocks(block.comparisons)
#
#   # get average similarity scores of standardized (0-1) variables
#   std.cols <- which(colnames(comparisons) %in% standardized.variables)
#   average.similarity <- apply(comparisons[, std.cols], 1, mean, na.rm=T)
#
#   #add the stage and avg.sim to comparisons
#   comparisons$average.similarity <- average.similarity
#   comparisons$stage <- NA
#
#   # order the average similarities
#   avg.sim.n <- length(average.similarity)
#   avg.sims <- data.frame(n=1:avg.sim.n, avg.sim=average.similarity)
#   avg.sims.ordered <- avg.sims[order(avg.sims$avg.sim), ]
#
#
#   # figure out which 1000 rows we want to use by weighting
#   # want uniform weights in the top 75% and also the last 20
#   rows.to.test <- c(round(seq(avg.sim.n/5, avg.sim.n-20, length.out=n.initial.stage-20)),
#                     (avg.sim.n-19):(avg.sim.n))
#   sims.to.test <- avg.sims.ordered[rows.to.test, ]
#
#   # get comparison matrix for initial stage
#   initial.sample <- comparisons[sims.to.test$n, ]
#   initial.sample$stage <- 'initial'
#
#   # we need the untested comparisons
#   initial.untested <- comparisons[-c(sims.to.test$n), ]
#
#   # how are we going to initially match the 1000
#   if (is.null(initial.matching.scheme)) {
#     initial.matching.scheme <- (initial.sample$average.similarity > .98) |
#       (initial.sample$average.similarity > .95 & initial.sample$date_of_death.Abs < 40) |
#       (initial.sample$average.similarity > .90 & initial.sample$date_of_death.Abs < 10) |
#       (initial.sample$average.similarity > .8 & initial.sample$date_of_death.Abs == 0)
#   } else {
#     initial.matching.scheme <- initial.matching.scheme
#   }
#
#
#   # initial matches are 1, non matches are 0
#   initial.sample$Active_Match <- ifelse(initial.matching.scheme, 1, 0)
#
#   # in order to introduce some randomness we change 5 randomly
#   change.some <- sample(which(initial.sample$Active_Match == 1), 5)
#   initial.sample$Active_Match[change.some] <- 0
#
#
#   # we model our initial sample
#   glm.model <- glm(model.formula,
#                    data = initial.sample,
#                    family = binomial)
#
#   # get predictions on the untested comparisons from our original model
#   test.preds <- predict(glm.model, initial.untested, type="response")
#
#   # max(test.preds[which(test.preds < .5)])
#
#   # test.preds[which(test.preds < .75 & test.preds > .25)]
#
#
#   for(i in 1:stages){
#     # we want the n.per.stage closest values to .5
#     closest.preds <- which.n.closest.to(n.per.stage, test.preds, uncertainty.param)
#     stage.comparisons <- initial.untested[closest.preds, ]
#     stage.comparisons$stage <- paste('stage', i, sep="")
#
#     if(is.null(user.accuracy)){
#       stage.comparisons$Active_Match <- stage.comparisons$True_Match
#     } else{
#       true.match <- stage.comparisons$True_Match
#       switch <- sample(1:length(true.match), round((1 - user.accuracy) * length(true.match)))
#       true.match[switch] <- ifelse(true.match[switch] == 1, 0, 1)
#       stage.comparisons$Active_Match <- true.match
#     }
#
#     # now we want to update our initial.sample
#
#     stage.comparison.match <- sum(stage.comparisons$Active_Match == 1)
#     stage.comparison.nonmatch <- sum(stage.comparisons$Active_Match == 0)
#
#     init.sample.match <- which(initial.sample$Active_Match == 1 & initial.sample$stage == 'initial')
#     init.sample.nonmatch <- which(initial.sample$Active_Match == 0 & initial.sample$stage == 'initial')
#
#     if((length(init.sample.match) > stage.comparison.match) &&
#        (length(init.sample.nonmatch) > stage.comparison.nonmatch)){
#       get.rid.of <- c(sample(init.sample.match, stage.comparison.match),
#                       sample(init.sample.nonmatch, stage.comparison.nonmatch))
#       initial.sample <- initial.sample[-get.rid.of, ]
#     } else if((length(init.sample.match) < stage.comparison.match) &&
#               (length(init.sample.nonmatch) < stage.comparison.nonmatch)){
#       initial.sample <- initial.sample
#     } else if((length(init.sample.match) < stage.comparison.match)){
#       get.rid.of <- c(sample(init.sample.nonmatch, stage.comparison.nonmatch))
#       initial.sample <- initial.sample[-get.rid.of, ]
#     } else{
#       get.rid.of <- c(sample(init.sample.match, stage.comparison.match))
#       if(length(get.rid.of) > 0){
#         initial.sample <- initial.sample[-get.rid.of, ]
#       } else{
#         initial.sample <- initial.sample
#       }
#     }
#
#
#     initial.sample <- rbind(initial.sample, stage.comparisons)
#
#
#     # update untested sample
#
#     initial.untested <- initial.untested[-closest.preds, ]
#
#
#     #build model
#
#     glm.model <- glm(model.formula,
#                      data = initial.sample,
#                      family = binomial)
#
#     test.preds <- predict(glm.model, initial.untested, type="response")
#
#
#   }
#
#   # NOW WE HAVE A FINAL MODEL
#
#   hclust.all <- AllBlocksHclustCutGLM(glm.model,
#                                       block.comparisons,
#                                       RLdata,
#                                       cut.threshold)
#
#
#   true.match <- initial.untested$True_Match
#   predicted.match <- round(test.preds)
#   untested.eval <- evaluation(true.match, predicted.match)
#
#   #merged.block.data
#   #merged.comparison.data
#
#   merged.block.data <- hclust.all$merged.block.data
#   merged.comparison.data <- hclust.all$merged.comparison.data
#
#
#   results <- list(merged.block.data=merged.block.data,
#                   merged.comparison.data=merged.comparison.data,
#                   untested.comparisons = initial.untested,
#                   tested.comparisons = initial.sample,
#                   final.glm = glm.model,
#                   final.test.preds = test.preds,
#                   untested.eval = untested.eval,
#                   seed = seed)
#   return(results)
#
#
# }
#
#
#
#
#
#
