


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
#' ProbItsAMatch(RLdata500[1, ], RLdata500[2, ])
ProbItsAMatch <- function(record1, record2) {
  print(
    "If two records definitely do not come from the same entity then their probability of matching is 0."
  )
  print(
    "If two records most likely come from the same entity then their probability of matching is 0.8"
  )
  print(
    "If two records definitely come from the same entity then their probability of matching is 1."
  )

  print(record1)
  print(record2)

  user.prob.of.match <-
    readline("What is the probability that these two records match?  ")

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
IsItAMatch <- function(record1, record2) {
  print("If two records do not come from the same entity then they are not a match.")
  print("If two records come from the same entity then they are a match.")

  recs <- rbind(record1, record2)

  print(recs)

  y.n.match <-
    readline("Are the previous two records a match? y/n? ")

}

ApplyIsItAMatch <- function(x, data, record.id){
  y.n <- IsItAMatch(data[x[ record.id[1]], ], data[x[record.id[2]], ])
  return(y.n)
}


#' @export
AreTheyMatches <- function(records) {
  print("If two records do not come from the same entity then they are not a match.")
  print("If two records come from the same entity then they are a match.")

  n <- nrow(records)
  y.n.match <- rep(NA, n)
  # can be data.frame or data.table
  records <- data.frame(Line = 1:n, records)

  print(records)
  for(i in 1:n){
    y.n.match[i] <-
      readline(paste0("Are the records in line ", i, " a match? y/n? "))
  }
  return(y.n.match)
}

ApplyIsItAMatch <- function(x, data, record.id){
  y.n <- IsItAMatch(data[x[ record.id[1]], ], data[x[record.id[2]], ])
  return(y.n)
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
# BuildATrainingDataset <- function(RLdata,
#                                   n.pairs.to.test,
#                                   variables.to.match = NULL,
#                                   string.comparators = NULL,
#                                   current.record.ids = NULL,
#                                   standardized.variables = NULL,
#                                   seed = NULL) {
#   if (is.null(current.record.ids)) {
#     current.record.ids <- c("CurrentRecord1", "CurrentRecord2")
#   } else{
#     current.record.ids <- current.record.ids
#   }
#
#   if (is.null(seed)) {
#     seed <- sample(1:100000, 1)
#   } else{
#     seed <- seed
#   }
#
#
#   comparisons <- CompareUniqueCombinations(
#     RLdata = RLdata,
#     variables.to.match = variables.to.match,
#     string.comparators = string.comparators
#   )
#   comparisons <- as.data.frame(comparisons)
#
#   if (is.null(standardized.variables)) {
#     factor.vars <-
#       names(which(sapply(RLdata[, unique(variables.to.match)], is.factor) == TRUE))
#     factor.vars <- variables.to.match %>%
#       unique() %>%
#       RLdata[, .] %>%
#       sapply(., is.factor) %>%
#       which(. == TRUE) %>%
#       names()
#     std.cols <- which(variables.to.match %in% factor.vars)
#   } else{
#     std.cols <- which(variables.to.match %in% standardized.variables)
#   }
#
#
#   average.similarity <-
#     apply(comparisons[, std.cols], 1, mean, na.rm = T)
#   avg.sim.n <- length(average.similarity)
#
#   avg.sims <- data.frame(n = 1:avg.sim.n, avg.sim = average.similarity)
#
#   avg.sims.ordered <- avg.sims[order(avg.sims$avg.sim), ]
#
#
#   bot <- 1:round(n.pairs.to.test / 4)
#
#   top <- (avg.sim.n - round(n.pairs.to.test / 4)):avg.sim.n
#
#   mid <- n.pairs.to.test - length(c(bot, top))
#
#   set.seed(seed)
#
#
#   rest <- sample((max(bot) + 1):(min(top) - 1), mid, replace = FALSE)
#
#
#   sims.to.test <- avg.sims.ordered[c(bot, top, rest),]
#
#
#   user.matchYN <- c()
#
#   orig.recs <- c()
#
#   for (i in sims.to.test$n) {
#     r1 <-
#       comparisons[i , which(colnames(comparisons) %in% current.record.ids[1])]
#     r2 <-
#       comparisons[i , which(colnames(comparisons) %in% current.record.ids[2])]
#     orig.recs <- c(orig.recs, r1, r2)
#
#     user.mat <- IsItAMatch(RLdata[r1,], RLdata[r2,])
#     user.matchYN <- c(user.matchYN, user.mat)
#
#   }
#
#   user.match <- ifelse(user.matchYN == "y", 1, 0)
#
#   comparisons$Active_Match <- rep(NA, nrow(comparisons))
#   comparisons$Active_Match[sims.to.test$n] <- user.match
#
#   results <- list(
#     comparisons = comparisons,
#     tested.comparisons = comparisons[sims.to.test$n,],
#     tested.data = RLdata[unique(orig.recs),],
#     seed = seed,
#     user.match = user.match
#   )
#   return(results)
#
# }


#'
#' #' Build a training dataset automatically
#' #'
#' #' Build a training dataset from user input about whether records in the dataset match
#' #'
#' #' @param RLdata a data frame containing the records to be matched
#' #'
#' #' @param n.pairs.to.test an integer corresponding to the number of pairs of records the user wants to test
#' #'
#' #' @param record.ids a vector of strings corresponding to the variable names that will contain the pairwise combinations of records in the data. The default (which is produced using CompareUniqueCombinations) is c("CurrentRecord1", "CurrentRecord2").
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



WhichNClosestTo <- function(n, vec, k) {
  ab.dif <- abs(vec - k)
  which.in <- which(ab.dif %in% sort(ab.dif)[1:n])
  return(sample(which.in, n))
}




# make sure to change initial.matching.scheme
# use initial.sample and average.similarity

# initial.matching.scheme <-
#   parse(text= "(initial.sample$average.similarity > .98) |
#   (initial.sample$average.similarity > .95 &
#      initial.sample$date_of_death.Abs < 40) |
#   (initial.sample$average.similarity > .90 &
#      initial.sample$date_of_death.Abs < 10) |
#   (initial.sample$average.similarity > .8 &
#      initial.sample$date_of_death.Abs == 0)")


# with true training data

BuildATrainingDatasetAuto <- function(RLdata,
                                         block.comparisons,
                                         standardized.variables,
                                         test.data = NULL,
                                         test.comparisons = NULL,
                                         model.formula,
                                         true.match.name=NULL,
                                         n.pairs.to.test = NULL,
                                         n.per.stage = NULL,
                                         n.initial.stage = NULL,
                                         initial.matching.scheme = NULL,
                                         record.id = NULL,
                                         user.accuracy = NULL,
                                         uncertainty.param = NULL,
                                         seed = NULL,
                                         cut.threshold = NULL) {
  ###############
  # NULL business
  ###############

  if(is.null(true.match.name)){
    true.match.name <- "True_Match"
  } else{
    true.match.name <- true.match.name
  }

  if (is.null(cut.threshold)) {
    cut.threshold <- .5
  } else{
    cut.threshold <- cut.threshold
  }

  if (is.null(seed)) {
    seed <- sample(1:100000, 1)
  } else{
    seed <- seed
  }

  set.seed(seed)

  if (is.null(n.pairs.to.test)) {
    n.pairs.to.test <- 100
  } else{
    n.pairs.to.test <- n.pairs.to.test
  }

  if (is.null(n.per.stage)) {
    n.per.stage <- 20
  } else{
    n.per.stage <- n.per.stage
  }

  if (is.null(n.initial.stage)) {
    n.initial.stage <- 1000
  } else{
    n.initial.stage <- n.initial.stage
  }

  if (is.null(uncertainty.param)) {
    uncertainty.param <- .5
  } else{
    uncertainty.param <- uncertainty.param
  }

  stages <- n.pairs.to.test / n.per.stage

  print(stages)

  if (is.null(record.id)) {
    record.id <- c("PreBlockRecord1", "PreBlockRecord2")
  } else{
    record.id <- record.id
  }

  ##################################
  # Average standardized comparisons
  ##################################

  comparisons <- MergeAllBlocks(block.comparisons)

  # get average similarity scores of standardized (0-1) variables
  std.cols <-
    which(colnames(comparisons) %in% standardized.variables)
  # add the average similarities to comparisons
  comparisons$average.similarity <-
    apply(comparisons[, std.cols], 1, mean, na.rm = T)

  # add the stage to comparisons
  comparisons$stage <- NA

  # order the average similarities
  avg.sim.n <- length(comparisons$average.similarity)
  # print(avg.sim.n)
  avg.sims <- data.frame(n = 1:avg.sim.n,
                         avg.sim = comparisons$average.similarity)
  avg.sims.ordered <- avg.sims[order(avg.sims$avg.sim), ]


  #############################
  # Create initial stage data
  #############################

  # Make sure we have the top 20 largest similarity scores
  # Sample from the rest uniformly
  rows.to.test <-
    c(round(seq(1, avg.sim.n - 20, length.out = n.initial.stage - 20)),
      (avg.sim.n - 19):(avg.sim.n))
  sims.to.test <- avg.sims.ordered[rows.to.test, ]

  # Subset comparison matrix for initial stage
  initial.sample <- comparisons[sims.to.test$n, ]
  initial.sample$stage <- 'initial'

  # Save untested comparisons
  initial.untested <- comparisons[-c(sims.to.test$n),]

  # how are we going to initially match the 1000
  if (is.null(initial.matching.scheme)) {
    initial.matching.scheme <- initial.untested$average.similarity > .9
  } else {
    initial.matching.scheme <- eval(initial.matching.scheme)
  }


  # initial matches are 1, non matches are 0
  initial.sample$Active_Match <- rep(NA, nrow(initial.sample))
  initial.sample$Active_Match <-
    ifelse(initial.matching.scheme, 1, 0)

  # in order to introduce some randomness we change 5 randomly
  change.some <- sample(which(initial.sample$Active_Match == 1), 5)
  initial.sample$Active_Match[change.some] <- 0


  # we model our initial sample
  glm.model <- glm(model.formula,
                   data = initial.sample,
                   family = binomial)

  # get predictions on the untested comparisons from our original model
  test.preds <-
    predict(glm.model, initial.untested, type = "response")



  for (i in 1:stages) {
    # we want the n.per.stage closest values to .5
    closest.preds <-
      WhichNClosestTo(n.per.stage, test.preds, uncertainty.param)

    stage.comparisons <- initial.untested[closest.preds,]
    stage.comparisons$stage <- paste('stage', i, sep = "")

    if (is.null(user.accuracy)) {
      stage.comparisons$Active_Match <- stage.comparisons[, true.match.name]
    } else{
      true.match <- stage.comparisons[, true.match.name]
      switch <-
        sample(1:length(true.match), round((1 - user.accuracy) * length(true.match)))
      true.match[switch] <- ifelse(true.match[switch] == 1, 0, 1)
      stage.comparisons$Active_Match <- true.match
    }

    # now we want to update our initial.sample

    stage.comparison.match <-
      sum(stage.comparisons$Active_Match == 1)
    stage.comparison.nonmatch <-
      sum(stage.comparisons$Active_Match == 0)

    init.sample.match <-
      which(initial.sample$Active_Match == 1 &
              initial.sample$stage == 'initial')
    init.sample.nonmatch <-
      which(initial.sample$Active_Match == 0 &
              initial.sample$stage == 'initial')

    print(i)
    print(c(
      stage.comparison.match,
      stage.comparison.nonmatch,
      length(init.sample.match),
      length(init.sample.nonmatch)
    ))

    if ((length(init.sample.match) > stage.comparison.match) &&
        (length(init.sample.nonmatch) > stage.comparison.nonmatch)) {
      get.rid.of <- c(
        sample(init.sample.match, stage.comparison.match),
        sample(init.sample.nonmatch, stage.comparison.nonmatch)
      )
      initial.sample <- initial.sample[-get.rid.of,]
    } else if ((length(init.sample.match) < stage.comparison.match) &&
               (length(init.sample.nonmatch) < stage.comparison.nonmatch)) {
      initial.sample <- initial.sample
    } else if ((length(init.sample.match) < stage.comparison.match)) {
      get.rid.of <-
        c(sample(init.sample.nonmatch, stage.comparison.nonmatch))
      if (length(get.rid.of) > 0) {
        initial.sample <- initial.sample[-get.rid.of,]
      } else{
        initial.sample <- initial.sample
      }
    } else{
      get.rid.of <- c(sample(init.sample.match, stage.comparison.match))
      if (length(get.rid.of) > 0) {
        initial.sample <- initial.sample[-get.rid.of,]
      } else{
        initial.sample <- initial.sample
      }

    }


    initial.sample <- rbind(initial.sample, stage.comparisons)
    initial.untested <- initial.untested[-closest.preds,]

    # build model
    glm.model <- glm(model.formula,
                     data = initial.sample,
                     family = binomial)

    test.preds <-
      predict(glm.model, initial.untested, type = "response")

  }

  # NOW WE HAVE A FINAL MODEL

  if (!is.null(test.comparisons) & !is.null(test.data)) {
    hclust.all <- AllBlocksHclustCutGLMNew(glm.model,
                                           test.comparisons,
                                           test.data,
                                           cut.threshold)

  } else{
    hclust.all <- AllBlocksHclustCutGLMNew(glm.model,
                                           block.comparisons,
                                           RLdata,
                                           cut.threshold)

  }

  # print(head(hclust.all$merged.block.data))

  #data.blocks.hclust <- hclust.all$test.data.blocks
  blocks.hclust.IDS <- hclust.all$block.hclust.ids
  blocks.true.IDS <- hclust.all$block.unique.ids

  true.match <- initial.untested$True_Match
  predicted.match <- round(test.preds)
  untested.eval <- evaluation(true.match, predicted.match)

  results <- list(
    # merged.block.data = merged.block.data.done,
    # merged.comparison.data = merged.comparison.data.done,
    # data.blocks.hclust = data.blocks.hclust,
    blocks.hclust.IDS = blocks.hclust.IDS,
    blocks.true.IDS = blocks.true.IDS,
    # untested.comparisons = initial.untested,
    # tested.comparisons = initial.sample,
    untested.eval = untested.eval,
    # final.glm = glm.model,
    final.test.preds = test.preds,
    seed = seed
  )
  return(results)


}








# with no training data
# not trying to simulate any type of accuracy

# RLdata should be NOT block data

#' Build training data actively
#'
#' @export
BuildTrainingDataAndModel <- function(RLdata,
                                         block.comparisons,
                                         standardized.variables,
                                         model.formula,
                                         n.pairs.to.test = NULL,
                                         n.per.stage = NULL,
                                         n.initial.stage = NULL,
                                         initial.matching.scheme = NULL,
                                         record.id = NULL,
                                         uncertainty.param = NULL,
                                         seed = NULL,
                                         cut.threshold = NULL) {
  ###############
  # NULL business
  ###############


  if (is.null(cut.threshold)) {
    cut.threshold <- .5
  } else{
    cut.threshold <- cut.threshold
  }

  if (is.null(seed)) {
    seed <- sample(1:100000, 1)
  } else{
    seed <- seed
  }

  set.seed(seed)

  if (is.null(n.pairs.to.test)) {
    n.pairs.to.test <- 100
  } else{
    n.pairs.to.test <- n.pairs.to.test
  }

  if (is.null(n.per.stage)) {
    n.per.stage <- 20
  } else{
    n.per.stage <- n.per.stage
  }

  if (is.null(n.initial.stage)) {
    n.initial.stage <- 1000
  } else{
    n.initial.stage <- n.initial.stage
  }

  if (is.null(uncertainty.param)) {
    uncertainty.param <- .5
  } else{
    uncertainty.param <- uncertainty.param
  }

  stages <- n.pairs.to.test / n.per.stage

  print(stages)

  if (is.null(record.id)) {
    record.id <- c("PreBlockRecord1", "PreBlockRecord2")
  } else{
    record.id <- record.id
  }

  ##################################
  # Average standardized comparisons
  ##################################

  comparisons <- MergeAllBlocks(block.comparisons)

  # get average similarity scores of standardized (0-1) variables
  std.cols <-
    which(colnames(comparisons) %in% standardized.variables)
  # add the average similarities to comparisons
  comparisons$average.similarity <-
    apply(comparisons[, std.cols], 1, mean, na.rm = T)

  # add the stage to comparisons
  comparisons$stage <- NA

  # order the average similarities
  avg.sim.n <- length(comparisons$average.similarity)
  # print(avg.sim.n)
  avg.sims <- data.frame(n = 1:avg.sim.n,
                         avg.sim = comparisons$average.similarity)
  avg.sims.ordered <- avg.sims[order(avg.sims$avg.sim), ]


  #############################
  # Create initial stage data
  #############################

  # Make sure we have the top 20 largest similarity scores
  # Sample from the rest uniformly
  rows.to.test <-
    c(round(seq(1, avg.sim.n - 20, length.out = n.initial.stage - 20)),
      (avg.sim.n - 19):(avg.sim.n))
  sims.to.test <- avg.sims.ordered[rows.to.test, ]

  # Subset comparison matrix for initial stage
  initial.sample <- comparisons[sims.to.test$n, ]
  initial.sample$stage <- 'initial'

  # Save untested comparisons
  initial.untested <- comparisons[-c(sims.to.test$n),]

  # how are we going to initially match the 1000
  if (is.null(initial.matching.scheme)) {
    initial.matching.scheme <- initial.untested$average.similarity > .9
  } else {
    initial.matching.scheme <- eval(initial.matching.scheme)
  }


  # initial matches are 1, non matches are 0
  initial.sample$Active_Match <- rep(NA, nrow(initial.sample))
  initial.sample$Active_Match <-
    ifelse(initial.matching.scheme, 1, 0)

  # in order to introduce some randomness we change 5 randomly
  change.some <- sample(which(initial.sample$Active_Match == 1), 5)
  initial.sample$Active_Match[change.some] <- 0


  # we model our initial sample
  glm.model <- glm(model.formula,
                   data = initial.sample,
                   family = binomial)

  # get predictions on the untested comparisons from our original model
  test.preds <-
    predict(glm.model, initial.untested, type = "response")



  for (i in 1:stages) {

    ###############
    # Prompt user
    ###############

    # we want the n.per.stage closest values to .5
    closest.preds <-
      WhichNClosestTo(n.per.stage, test.preds, uncertainty.param)

    stage.comparisons <- initial.untested[closest.preds,]
    stage.comparisons$stage <- paste('stage', i, sep = "")

    stage.matches <- apply(stage.comparisons, 1, ApplyIsItAMatch,
                           data=RLdata, record.id=record.id)

    stage.comparisons$Active_Match <-
      as.numeric(ifelse(stage.matches == "y", 1, 0 ))

    ##################
    # Update samples
    ##################

    stage.comparison.match <-
      sum(stage.comparisons$Active_Match == 1)
    stage.comparison.nonmatch <-
      sum(stage.comparisons$Active_Match == 0)

    init.sample.match <-
      which(initial.sample$Active_Match == 1 &
              initial.sample$stage == 'initial')
    init.sample.nonmatch <-
      which(initial.sample$Active_Match == 0 &
              initial.sample$stage == 'initial')

    print(i)
    print(c(
      stage.comparison.match,
      stage.comparison.nonmatch,
      length(init.sample.match),
      length(init.sample.nonmatch)
    ))

    if ((length(init.sample.match) > stage.comparison.match) &&
        (length(init.sample.nonmatch) > stage.comparison.nonmatch)) {
      get.rid.of <- c(
        sample(init.sample.match, stage.comparison.match),
        sample(init.sample.nonmatch, stage.comparison.nonmatch)
      )
      initial.sample <- initial.sample[-get.rid.of,]
    } else if ((length(init.sample.match) < stage.comparison.match) &&
               (length(init.sample.nonmatch) < stage.comparison.nonmatch)) {
      initial.sample <- initial.sample
    } else if ((length(init.sample.match) < stage.comparison.match)) {
      get.rid.of <-
        c(sample(init.sample.nonmatch, stage.comparison.nonmatch))
      if (length(get.rid.of) > 0) {
        initial.sample <- initial.sample[-get.rid.of,]
      } else{
        initial.sample <- initial.sample
      }
    } else{
      get.rid.of <- c(sample(init.sample.match, stage.comparison.match))
      if (length(get.rid.of) > 0) {
        initial.sample <- initial.sample[-get.rid.of,]
      } else{
        initial.sample <- initial.sample
      }

    }

    initial.sample <- rbind(initial.sample, stage.comparisons)
    initial.untested <- initial.untested[-closest.preds,]

    # build model
    glm.model <- glm(model.formula,
                     data = initial.sample,
                     family = binomial)

    test.preds <-
      predict(glm.model, initial.untested, type = "response")
  }

  results <- list(
    untested.comparisons = initial.untested,
    tested.comparisons = initial.sample,
    final.glm = glm.model,
    final.test.preds = test.preds,
    seed = seed
  )

  return(results)
}


#########################################################
# Build Training, Predict on Remaining, HCLUST, Give UIDS
#########################################################

#' Get unique ids actively
#'
#' @export
BuildTrainingClusterFull <- function(RLdata,
                                     RLdata.blocked,
                                  block.comparisons,
                                  standardized.variables,
                                  model.formula,
                                  n.pairs.to.test = NULL,
                                  n.per.stage = NULL,
                                  n.initial.stage = NULL,
                                  initial.matching.scheme = NULL,
                                  record.id = NULL,
                                  uncertainty.param = NULL,
                                  seed = NULL,
                                  cut.threshold = NULL) {
  ###############
  # NULL business
  ###############


  if (is.null(cut.threshold)) {
    cut.threshold <- .5
  } else{
    cut.threshold <- cut.threshold
  }

  if (is.null(seed)) {
    seed <- sample(1:100000, 1)
  } else{
    seed <- seed
  }

  set.seed(seed)

  if (is.null(n.pairs.to.test)) {
    n.pairs.to.test <- 100
  } else{
    n.pairs.to.test <- n.pairs.to.test
  }

  if (is.null(n.per.stage)) {
    n.per.stage <- 20
  } else{
    n.per.stage <- n.per.stage
  }

  if (is.null(n.initial.stage)) {
    n.initial.stage <- 1000
  } else{
    n.initial.stage <- n.initial.stage
  }

  if (is.null(uncertainty.param)) {
    uncertainty.param <- .5
  } else{
    uncertainty.param <- uncertainty.param
  }

  stages <- n.pairs.to.test / n.per.stage

  print(stages)

  if (is.null(record.id)) {
    record.id <- c("PreBlockRecord1", "PreBlockRecord2")
  } else{
    record.id <- record.id
  }

  ##################################
  # Average standardized comparisons
  ##################################

  comparisons <- MergeAllBlocks(block.comparisons)

  # get average similarity scores of standardized (0-1) variables
  std.cols <-
    which(colnames(comparisons) %in% standardized.variables)
  # add the average similarities to comparisons
  comparisons$average.similarity <-
    apply(comparisons[, std.cols], 1, mean, na.rm = T)

  # add the stage to comparisons
  comparisons$stage <- NA

  # order the average similarities
  avg.sim.n <- length(comparisons$average.similarity)
  # print(avg.sim.n)
  avg.sims <- data.frame(n = 1:avg.sim.n,
                         avg.sim = comparisons$average.similarity)
  avg.sims.ordered <- avg.sims[order(avg.sims$avg.sim), ]


  #############################
  # Create initial stage data
  #############################

  # Make sure we have the top 20 largest similarity scores
  # Sample from the rest uniformly
  rows.to.test <-
    c(round(seq(1, avg.sim.n - 20, length.out = n.initial.stage - 20)),
      (avg.sim.n - 19):(avg.sim.n))
  sims.to.test <- avg.sims.ordered[rows.to.test, ]

  # Subset comparison matrix for initial stage
  initial.sample <- comparisons[sims.to.test$n, ]
  initial.sample$stage <- 'initial'

  # Save untested comparisons
  initial.untested <- comparisons[-c(sims.to.test$n),]

  # how are we going to initially match the 1000
  if (is.null(initial.matching.scheme)) {
    initial.matching.scheme <- initial.untested$average.similarity > .9
  } else {
    initial.matching.scheme <- eval(initial.matching.scheme)
  }


  # initial matches are 1, non matches are 0
  initial.sample$Active_Match <- rep(NA, nrow(initial.sample))
  initial.sample$Active_Match <-
    ifelse(initial.matching.scheme, 1, 0)

  # in order to introduce some randomness we change 5 randomly
  change.some <- sample(which(initial.sample$Active_Match == 1), 5)
  initial.sample$Active_Match[change.some] <- 0


  # we model our initial sample
  glm.model <- glm(model.formula,
                   data = initial.sample,
                   family = binomial)

  # get predictions on the untested comparisons from our original model
  test.preds <-
    predict(glm.model, initial.untested, type = "response")



  for (i in 1:stages) {

    ###############
    # Prompt user
    ###############

    # we want the n.per.stage closest values to .5
    closest.preds <-
      WhichNClosestTo(n.per.stage, test.preds, uncertainty.param)

    stage.comparisons <- initial.untested[closest.preds,]
    stage.comparisons$stage <- paste('stage', i, sep = "")

    stage.matches <- apply(stage.comparisons, 1, ApplyIsItAMatch,
                           data=RLdata, record.id=record.id)

    stage.comparisons$Active_Match <-
      as.numeric(ifelse(stage.matches == "y", 1, 0 ))

    ##################
    # Update samples
    ##################

    stage.comparison.match <-
      sum(stage.comparisons$Active_Match == 1)
    stage.comparison.nonmatch <-
      sum(stage.comparisons$Active_Match == 0)

    init.sample.match <-
      which(initial.sample$Active_Match == 1 &
              initial.sample$stage == 'initial')
    init.sample.nonmatch <-
      which(initial.sample$Active_Match == 0 &
              initial.sample$stage == 'initial')

    print(i)
    print(c(
      stage.comparison.match,
      stage.comparison.nonmatch,
      length(init.sample.match),
      length(init.sample.nonmatch)
    ))

    if ((length(init.sample.match) > stage.comparison.match) &&
        (length(init.sample.nonmatch) > stage.comparison.nonmatch)) {
      get.rid.of <- c(
        sample(init.sample.match, stage.comparison.match),
        sample(init.sample.nonmatch, stage.comparison.nonmatch)
      )
      initial.sample <- initial.sample[-get.rid.of,]
    } else if ((length(init.sample.match) < stage.comparison.match) &&
               (length(init.sample.nonmatch) < stage.comparison.nonmatch)) {
      initial.sample <- initial.sample
    } else if ((length(init.sample.match) < stage.comparison.match)) {
      get.rid.of <-
        c(sample(init.sample.nonmatch, stage.comparison.nonmatch))
      if (length(get.rid.of) > 0) {
        initial.sample <- initial.sample[-get.rid.of,]
      } else{
        initial.sample <- initial.sample
      }
    } else{
      get.rid.of <- c(sample(init.sample.match, stage.comparison.match))
      if (length(get.rid.of) > 0) {
        initial.sample <- initial.sample[-get.rid.of,]
      } else{
        initial.sample <- initial.sample
      }

    }


    initial.sample <- rbind(initial.sample, stage.comparisons)
    initial.untested <- initial.untested[-closest.preds,]

    # build model
    glm.model <- glm(model.formula,
                     data = initial.sample,
                     family = binomial)

    test.preds <-
      predict(glm.model, initial.untested, type = "response")

  }

  # NOW WE HAVE A FINAL MODEL

  hclust.all <- AllBlocksHclustCutGLMNew(glm.model,
                                         block.comparisons,
                                         RLdata.blocked,
                                         cut.threshold)

  blocks.hclust.IDS <- hclust.all$block.hclust.ids
  blocks.true.IDS <- hclust.all$block.unique.ids

  true.match <- initial.untested[, true.match.name]
  predicted.match <- round(test.preds)
  untested.eval <- evaluation(true.match, predicted.match)

  results <- list(
    # merged.block.data = merged.block.data.done,
    # merged.comparison.data = merged.comparison.data.done,
    # data.blocks.hclust = data.blocks.hclust,
    blocks.hclust.IDS = blocks.hclust.IDS,
    blocks.true.IDS = blocks.true.IDS,
    # untested.comparisons = initial.untested,
    # tested.comparisons = initial.sample,
    untested.eval = untested.eval,
    final.glm = glm.model,
    seed = seed
  )
  return(results)


}












#########################################################
# Fellegi to initialize, Build Training, Predict on Remaining, HCLUST, Give UIDS
#########################################################

#' Get unique ids actively
#'
#' make sure n initial stage is > 200
#' #' #' @param record.ids a vector of strings corresponding to the variable names that will contain the pairwise combinations of records in the data. The default (which is produced using CompareUniqueCombinations) is c("CurrentRecord1", "CurrentRecord2").
#' I think we want it to be PreBlockRecord if we've blocked?
#' @export
FSActiveFull <- function(RLdata,
                         RLdata.blocked,
                         block.comparisons,
                         model.formula,
                         cols.for.fs,
                         n.pairs.to.test = NULL,
                         n.per.stage = NULL,
                         n.initial.stage = NULL,
                         record.id = NULL,
                         uncertainty.param = NULL,
                         seed = NULL,
                         cut.threshold = NULL,
                         save.self.matched=FALSE) {
  ###############
  # NULL business
  ###############
  if (is.null(cut.threshold)) {
    cut.threshold <- .5
  } else{
    cut.threshold <- cut.threshold
  }

  if (is.null(seed)) {
    seed <- sample(1:100000, 1)
  } else{
    seed <- seed
  }

  set.seed(seed)

  if (is.null(n.pairs.to.test)) {
    n.pairs.to.test <- 100
  } else{
    n.pairs.to.test <- n.pairs.to.test
  }

  if (is.null(n.per.stage)) {
    n.per.stage <- 20
  } else{
    n.per.stage <- n.per.stage
  }

  if (is.null(n.initial.stage)) {
    n.initial.stage <- 1000
  } else{
    n.initial.stage <- n.initial.stage
  }

  if (is.null(uncertainty.param)) {
    uncertainty.param <- .5
  } else{
    uncertainty.param <- uncertainty.param
  }

  stages <- n.pairs.to.test / n.per.stage

  print(stages)

  if (is.null(record.id)) {
    record.id <- c("PreBlockRecord1", "PreBlockRecord2")
  } else{
    record.id <- record.id
  }

  ##################################
  # Fellegi Sunter
  ##################################

  comparisons <- MergeAllBlocks(block.comparisons)

  data.for.fs <- comparisons[, cols.for.fs]

  mywts2 <- FellegiSunter(data.for.fs,
                          initial.m = rep(.97, length(cols.for.fs)),
                          initial.u = apply(data.for.fs, 2, function(x) sum(x)/length(x)),
                          initial.p = 1/sqrt(nrow(data.for.fs)) * 0.1)

  # add fellegi sunter predictions to our comparisons
  comparisons$FS <- ifelse(mywts2$log2.ratios > 0, 1, 0)

  # add the stage to comparisons
  comparisons$stage <- NA

  #############################
  # Create initial stage data
  #############################

  # Make sure we have at least 100 1's in initial sample
  fs.ones <- which(comparisons$FS == 1)
  fs.zeros <- which(comparisons$FS == 0)

  # make sure n.initial.stage > 200
  if(length(fs.ones) < 100){
    rows.to.test <- c(fs.ones,
                      sample(fs.zeros, n.initial.stage-length(fs.ones)))
  } else{
    rows.to.test <- c(sample(fs.ones, 100),
                      sample(fs.zeros, n.initial.stage-100))
  }

  # Subset comparison matrix for initial stage
  initial.sample <- comparisons[rows.to.test, ]
  initial.sample$stage <- 'initial'

  # Save untested comparisons
  initial.untested <- comparisons[-c(rows.to.test),]


  # initial matches are 1, non matches are 0
  initial.sample$Active_Match <- initial.sample$FS

  # in order to introduce some randomness we change 5 randomly
  change.some <- sample(which(initial.sample$Active_Match == 1), 5)
  initial.sample$Active_Match[change.some] <- 0

  # we model our initial sample
  glm.model <- glm(model.formula,
                   data = initial.sample,
                   family = binomial)

  # get predictions on the untested comparisons from our original model
  test.preds <- predict(glm.model, initial.untested, type = "response")


  for (i in 1:stages) {

    ###############
    # Prompt user
    ###############

    # we want the n.per.stage closest values to .5
    closest.preds <- WhichNClosestTo(n.per.stage,
                                     test.preds,
                                     uncertainty.param)

    stage.comparisons <- initial.untested[closest.preds, ]
    stage.comparisons$stage <- paste('stage', i, sep = "")

    stage.matches <- apply(stage.comparisons, 1, ApplyIsItAMatch,
                           data=RLdata, record.id=record.id)

    stage.comparisons$Active_Match <-
      as.numeric(ifelse(stage.matches == "y", 1, 0 ))

    ##################
    # Update samples
    ##################

    stage.comparison.match <-
      sum(stage.comparisons$Active_Match == 1)
    stage.comparison.nonmatch <-
      sum(stage.comparisons$Active_Match == 0)

    init.sample.match <-
      which(initial.sample$Active_Match == 1 &
              initial.sample$stage == 'initial')
    init.sample.nonmatch <-
      which(initial.sample$Active_Match == 0 &
              initial.sample$stage == 'initial')

    print(i)
    print(c(
      stage.comparison.match,
      stage.comparison.nonmatch,
      length(init.sample.match),
      length(init.sample.nonmatch)
    ))

    if ((length(init.sample.match) > stage.comparison.match) &&
        (length(init.sample.nonmatch) > stage.comparison.nonmatch)) {
      get.rid.of <- c(
        sample(init.sample.match, stage.comparison.match),
        sample(init.sample.nonmatch, stage.comparison.nonmatch)
      )
      initial.sample <- initial.sample[-get.rid.of,]
    } else if ((length(init.sample.match) < stage.comparison.match) &&
               (length(init.sample.nonmatch) < stage.comparison.nonmatch)) {
      initial.sample <- initial.sample
    } else if ((length(init.sample.match) < stage.comparison.match)) {
      get.rid.of <-
        c(sample(init.sample.nonmatch, stage.comparison.nonmatch))
      if (length(get.rid.of) > 0) {
        initial.sample <- initial.sample[-get.rid.of,]
      } else{
        initial.sample <- initial.sample
      }
    } else{
      get.rid.of <- c(sample(init.sample.match, stage.comparison.match))
      if (length(get.rid.of) > 0) {
        initial.sample <- initial.sample[-get.rid.of,]
      } else{
        initial.sample <- initial.sample
      }
    }

    initial.sample <- rbind(initial.sample, stage.comparisons)
    initial.untested <- initial.untested[-closest.preds,]

    if(save.self.matched){
      write.csv(initial.sample, 'self-matched-responses')
    }

    # build model
    glm.model <- glm(model.formula,
                     data = initial.sample,
                     family = binomial)

    test.preds <-
      predict(glm.model, initial.untested, type = "response")

  }

  # NOW WE HAVE A FINAL MODEL

  hclust.all <- AllBlocksHclustCutFS(glm.model,
                                     block.comparisons,
                                     RLdata.blocked,
                                     cut.threshold)

  blocks.hclust.IDS <- hclust.all$block.hclust.ids

  results <- list(
    blocks.hclust.IDS = blocks.hclust.IDS,
    final.glm = glm.model,
    seed = seed
  )
  return(results)
}





