
#' @export
evaluation <- function(true.matches, predicted.matches){
  # In some situations there will only be matches or non-matches
  # So we must control for this

#   test.v.pred <- as.data.frame(cbind(true.matches, predicted.matches))
#   colnames(test.v.pred) <- c("truth", "predicted")
#   contingency.table <- table(test.v.pred)

  test.v.pred <- data.frame(truth     =factor(true.matches, levels=c(0,1)),
                            predicted =factor(predicted.matches, levels=c(0,1)))
  contingency.table <- table(test.v.pred)

  true.positive <- contingency.table["1", "1"]
  true.negative <- contingency.table["0", "0"]
  false.positive <- contingency.table["0", "1"]
  false.negative <- contingency.table["1", "0"]

  false.positive.error <- false.positive/(false.positive + true.negative)
  false.negative.error <- false.negative/(false.negative + true.positive)
  false.discovery.rate <- false.positive/(false.positive + true.positive)
  sensitivity <- true.positive/ (true.positive + false.negative)
  specificity <- true.negative/ (true.negative + false.positive)
  precision <- true.positive/ (true.positive + false.positive)
  negative.predictive.value <- true.negative/ (true.negative + false.negative)
  accuracy <- (true.positive + true.negative)/(true.positive + true.negative +
                                                 false.positive + false.negative)

  return(list(contingency.table = contingency.table,
              false.positive.error = false.positive.error,
              false.negative.error = false.negative.error,
              false.discovery.rate = false.discovery.rate,
              sensitivity = sensitivity,
              specificity = specificity,
              precision = precision,
              negative.predictive.value = negative.predictive.value,
              accuracy = accuracy))

}




PickingN <- function(comparison.train, comparison.test, n, evaluation.params, glm.formula){

  eval.response <- rep(NA, length(evaluation.params))
  training.match <- which(comparison.train$True_Match == 1)
  training.nonmatch <- which(comparison.train$True_Match != 1)

  training.nonmatch.sample <- sample(training.nonmatch, (length(training.match)*n))

  training.adapted <- comparison.train[c(training.match, training.nonmatch.sample), ]


  glm.train <- glm(glm.formula,
                   data = training.adapted,
                   family = binomial)

  test.preds <- round(predict(glm.train, comparison.test, type="response"))
  for(i in 1:length(evaluation.params)){
    eval.response[i] <- get(evaluation.params[i], evaluation(comparison.test$True_Match, test.preds))
  }
  return(eval.response)
}



evaluate_fast <- function(rl_ids, true_ids, outer_block_ids = NULL, verbose = F, mod = 100) {

  #  Some basic checks
  ob_null <- F
  if (is.null(outer_block_ids))  ob_null <- T
  if (is.null(outer_block_ids))  outer_block_ids <- rep(1, length(rl_ids))
  if (length(rl_ids) != length(true_ids))  stop("lengths differ")
  if (length(outer_block_ids) != length(true_ids))  stop("lengths differ")

  #  Initialize contingency table
  comp_table <- matrix(0, 2, 2)

  #  Shorten the list of IDs to only the ones we need
  #  (i.e. IDs that are duplicated in either the rl_ids or the true_ids)
  #  find the indices of the IDs that are duplicated in either RL or true
  #  get indices of the corresponding IDs
  #  take union
  #  subset to only those IDs
  #  This process essentially just removes the singletons in both sets of IDs
  need <- which(duplicated(rl_ids) | duplicated(true_ids))
  need_rl <- rl_ids[need]
  need_true <- true_ids[need]
  keep <- which(rl_ids %in% need_rl | true_ids %in% need_true)
  rl_ids <- rl_ids[keep]
  true_ids <- true_ids[keep]
  outer_block_ids <- outer_block_ids[keep]
  nn <- length(rl_ids)

  #  Get indices for faster checking in the for loop
  uniq_ob_ids <- unique(outer_block_ids)
  get_ind <- function (id, nn) {
    return(outer_block_ids == id)
  }
  uniq_ind <- lapply(uniq_ob_ids, get_ind, nn)
  get_this_ind <- function (xx, uniq_ind, link, nn) {
    #if (verbose & xx %% mod == 1)  print(paste("Stage 1 of 2:  Done with", xx, "of", nn))
    this_index <- link[xx]
    temp <- uniq_ind[[this_index]]
    #temp[temp > xx]
  }
  link <- match(outer_block_ids, uniq_ob_ids)

  #  Loop through each ID.
  #  Get the pairwise comparisons for that ID vs. the rest
  #  Then calculate the temporary contingency table
  #  Stop at nn-1 since we don't compare the last ID to itself.
  #  CHANGE THIS TO SOMETHING OTHER THAN A FOR LOOP FOR FASTER EVALUATION!!!!!
  for (ii in 1:(nn-1)){
    #  Every mod IDs, print a status update.
    if (ii %% mod == 1 & verbose) {
      print(c(ii, nn))
      print(comp_table)
      if (verbose & ii %% mod == 1)  print(paste("Done with", ii, "of", nn))
    }

    #  Subset IDs to only that share the outer_block_ids
    if (ob_null) {
      ind <- (ii+1):nn
    } else {
      ind <- get_this_ind(ii, uniq_ind, link, nn)
      ind <- ind[(ii+1):nn]
    }
    temp_rl <- rl_ids[ind]
    temp_true <- true_ids[ind]

    #  Get pairwise comparisons for this ID
    comp_true <- as.factor(true_ids[ii] == temp_true)
    comp_rl <- as.factor(rl_ids[ii] == temp_rl)
    levels(comp_true) <- c(FALSE, TRUE)
    levels(comp_rl) <- c(FALSE, TRUE)

    #  Get contingency table for this ID
    temp_table <- table(comp_true, comp_rl)

    #  Add temp_table to the full comp_table
    comp_table <- comp_table + temp_table
  }

  return(comp_table)
}







