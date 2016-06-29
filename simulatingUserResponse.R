library(RecordLinkage)
library("devtools")
devtools::load_all()
data("RLdata500")
data("RLdata10000")


########################
# Split in to Train/Test
########################

fullRL <- rbind(RLdata500, RLdata10000)
fullID <- c(identity.RLdata500, identity.RLdata10000)

set.seed(2468)
train <- sample(c(TRUE,FALSE), nrow(fullRL), rep=TRUE, prob = c(.5, .5))
test <- (! train)
training.data <- fullRL[train, ]
testing.data  <- fullRL[test, ]
train.id <- fullID[train]
test.id <- fullID[test]


train.block.compare <- BlockAndCompareCombinations(RLdata=training.data,
                                                   var.names = c("fname_c1"),
                                                   n.chars = c(1),
                                                   ids=train.id,
                                                   variables.to.match = c("fname_c1",
                                                                          "lname_c1",
                                                                          "by",
                                                                          "bm",
                                                                          "bd"),
                                                   string.comparators = c("jarowinkler",
                                                                          "jarowinkler",
                                                                          "AbsoluteDifference",
                                                                          "AbsoluteDistance",
                                                                          "AbsoluteDistance"))


comparison.train <- train.block.compare$full.comparisons


test.block.compare <- BlockAndCompareCombinations(RLdata=testing.data,
                                                   var.names = c("fname_c1"),
                                                   n.chars = c(1),
                                                   ids=train.id,
                                                   variables.to.match = c("fname_c1",
                                                                          "lname_c1",
                                                                          "by",
                                                                          "bm",
                                                                          "bd"),
                                                   string.comparators = c("jarowinkler",
                                                                          "jarowinkler",
                                                                          "AbsoluteDifference",
                                                                          "AbsoluteDistance",
                                                                          "AbsoluteDistance"))


comparison.test <- test.block.compare$full.comparisons


if(is.null(standardized.variables)){
  factor.vars <- names(which(sapply(training.data[, unique(variables.to.match)], is.factor) == TRUE))
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


comparison.test$average.similarity <- apply(comparison.test[, std.cols], 1, mean, na.rm=T)

training.match <- which(comparison.test$True_Match == 1)
training.nonmatch <- which(comparison.test$True_Match != 1)

min.true <- min(comparison.test$average.similarity[training.match])
max.false <- max(comparison.test$average.similarity[training.nonmatch])

min.true
max.false



standardized.variables=c("fname_c1",
                         "lname_c1")

userAcur <- function(comparison.train,
                     comparison.test,
                     user.accuracy,
                     evaluation.params,
                     glm.formula,
                     standardized.variables=NULL){

  if(is.null(standardized.variables)){
    factor.vars <- names(which(sapply(training.data[, unique(variables.to.match)], is.factor) == TRUE))
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


  comparison.train$average.similarity <- apply(comparison.train[, std.cols], 1, mean, na.rm=T)

  training.match <- which(comparison.train$True_Match == 1)
  training.nonmatch <- which(comparison.train$True_Match != 1)

  min.true <- min(comparison.data.train$average.similarity[training.match])
  max.false <- max(comparison.data.train$average.similarity[training.nonmatch])





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











if(is.null(props.from.each.bin)){
  props.from.each.bin <- c(.1, 0, 0, 0, .4, .4, 0, 0, 0, .1)
} else{
  props.from.each.bin <- props.from.each.bin
}


RLdata=RLdata500
n.pairs.to.test=5
variables.to.match = c("fname_c1",
                       "lname_c1",
                       "by",
                       "bm",
                       "bd")
string.comparators = c("jarowinkler",
                       "jarowinkler",
                       "AbsoluteDifference",
                       "AbsoluteDistance",
                       "AbsoluteDistance")
standardized.variables=c("fname_c1",
                         "lname_c1")



BuildATrainingDatasetAuto <- function(RLdata,
                                      n.pairs.to.test,
                                      unique.ids,
                                      variables.to.match=NULL,
                                      string.comparators=NULL,
                                      current.record.ids=NULL,
                                      standardized.variables=NULL,
                                      seed=NULL,
                                      n.per.stage=NULL){

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

  if(is.null(n.per.stage)){
    n.per.stage <- 10
  } else{
    n.per.stage <- n.per.stage
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

  rows.to.test <- c(round(seq(1, avg.sim.n, length.out=n.per.stage)), (avg.sim.n-5):(avg.sim.n-1))

  set.seed(seed)

  sims.to.test <- avg.sims.ordered[rows.to.test, ]

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

  tested.comparisons <- comparisons[sims.to.test$n, ]
  nontested.comparisons <- comparisons[-sims.to.test$n, ]

  library(e1071)

  model.formula <- as.factor(Active_Match) ~ fname_c1.jar + lname_c1.jar + by.Abs + bm.Abs + bd.Abs

  build.glm <- glm(model.formula,
                   data=tested.comparisons,
                   family = binomial)

  test.the.rest <- predict(build.glm, nontested.comparisons, type="response")


   results <- list(comparisons=comparisons,
                  tested.comparisons=tested.comparisons,
                  tested.data=RLdata[unique(orig.recs), ],
                  seed=seed)
  return(results)

}





