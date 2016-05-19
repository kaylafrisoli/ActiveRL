
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





