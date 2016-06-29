

# record.id are the colnames of block.comparisons that have the rows for block.data
BuildATrainingDataset <- function(RLdata,
                                  block.comparisons,
                                  n.pairs.to.test=NULL,
                                  n.per.stage=NULL,
                                  record.id=NULL,
                                  standardized.variables){

  if(is.null(n.pairs.to.test)){
    n.pairs.to.test <- 100
  } else{
    n.pairs.to.test <- n.pairs.to.test
  }

  if(is.null(n.per.stage)){
    n.per.stage <- 20
  } else{
    n.per.stage <- n.per.stage
  }

  stages <- n.pairs.to.test/n.per.stage

  if(is.null(record.id)){
    record.id <- c("PreBlockRecord1", "PreBlockRecord2")
  } else{
    record.id <- record.id
  }

  comparisons <- block.comparisons

  std.cols <- which(colnames(comparisons) %in% standardized.variables)

  average.similarity <- apply(comparisons[, std.cols], 1, mean, na.rm=T)

  avg.sim.n <- length(average.similarity)
  avg.sims <- data.frame(n=1:avg.sim.n, avg.sim=average.similarity)
  avg.sims.ordered <- avg.sims[order(avg.sims$avg.sim), ]

  rows.to.test <- c(round(seq(1, avg.sim.n-5, length.out=n.per.stage-5)),
                    (avg.sim.n-4):(avg.sim.n))

  sims.to.test <- avg.sims.ordered[rows.to.test, ]

  user.matchYN <- c()
  orig.recs <- c()

  for(i in sims.to.test$n){
    r1 <- comparisons[i , which(colnames(comparisons) %in% record.id[1])]
    r2 <- comparisons[i , which(colnames(comparisons) %in% record.id[2])]
    orig.recs <- c(orig.recs, r1, r2)

    user.mat <- IsItAMatch(RLdata[r1, ], RLdata[r2, ])
    user.matchYN <- c(user.matchYN, user.mat)

  }

  user.match <- ifelse(user.matchYN == "y", 1, 0)

  comparisons$Active_Match <- rep(NA, nrow(comparisons))
  comparisons$Active_Match[sims.to.test$n] <- user.match
  untested.comparisons <- comparisons[-c(sims.to.test$n), ]

  results <- list(comparisons=comparisons,
                  tested.comparisons=comparisons[sims.to.test$n, ],
                  tested.data=RLdata[unique(orig.recs), ],
                  untested.comparisons=untested.comparisons)
  return(results)

}

################
####### TEST IT
#################


test.active1 <- BuildATrainingDataset(splitSyria$training.data,
                      training.merged,
                      n.pairs.to.test=40,
                      n.per.stage=20,
                      record.id=c("PreBlockRecord1", "PreBlockRecord2"),
                      standardized.variables= c("name.jar", "location.jar" ))


head(test.active1$comparisons)
head(test.active1$tested.comparisons)
head(test.active1$tested.data)



# KEEEP WORKING ON NEXT PART


# model formula y should be Active_Match
BuildATrainingDataset <- function(RLdata,
                                  block.comparisons,
                                  standardized.variables,
                                  model.formula,
                                  n.pairs.to.test=NULL,
                                  n.per.stage=NULL,
                                  record.id=NULL){

  if(is.null(n.pairs.to.test)){
    n.pairs.to.test <- 100
  } else{
    n.pairs.to.test <- n.pairs.to.test
  }

  if(is.null(n.per.stage)){
    n.per.stage <- 20
  } else{
    n.per.stage <- n.per.stage
  }

  stages <- n.pairs.to.test/n.per.stage

  if(is.null(record.id)){
    record.id <- c("PreBlockRecord1", "PreBlockRecord2")
  } else{
    record.id <- record.id
  }

  comparisons <- block.comparisons

  std.cols <- which(colnames(comparisons) %in% standardized.variables)

  average.similarity <- apply(comparisons[, std.cols], 1, mean, na.rm=T)

  avg.sim.n <- length(average.similarity)
  avg.sims <- data.frame(n=1:avg.sim.n, avg.sim=average.similarity)
  avg.sims.ordered <- avg.sims[order(avg.sims$avg.sim), ]

  n.initial.stage <- 1000

  rows.to.test <- c(round(seq(avg.sim.n/2.5, avg.sim.n-20, length.out=n.initial.stage-20)),
                    (avg.sim.n-19):(avg.sim.n))

  sims.to.test <- avg.sims.ordered[rows.to.test, ]

  initial.sample <- comparisons[sims.to.test$n, ]
  initial.untested <- comparisons[-c(sims.to.test$n), ]



  initial.sample$Initial_Match <- ifelse(average.similarity[sims.to.test$n] > .90, 1, 0)
  sample.matches <- sample(which(initial.sample$Initial_Match == 1), 6)
  initial.sample$Initial_Match[sample.matches] <- 0


  glm.model <- glm(Initial_Match ~ name.jar + name.lev + location.jar + date_of_death.Abs + month.Abs + day.Abs,
                   data = initial.sample,
                   family = binomial)

  test.preds <- predict(glm.model, initial.untested, type="response")





  glm.model <- glm(model.formula,
                   data = tested.comparisons,
                   family = binomial)

  test.preds <- round(predict(glm.model, untested.comparisons, type="response"))




  }



  results <- list(comparisons=comparisons,
                  tested.comparisons=tested.comparisons,
                  tested.data=tested.data)
  return(results)

}


RLdata <- splitSyria$training.data
block.comparisons <- training.merged

record.id = c("PreBlockRecord1", "PreBlockRecord2")
standardized.variables= c("name.jar", "location.jar" )


glm.model <- glm(Initial_Match ~ name.jar + name.lev + location.jar + date_of_death.Abs + month.Abs + day.Abs,
                 data = test.active1$tested.comparisons,
                 family = binomial)

test.preds <- predict(glm.model, test.active1$untested.comparisons, type="response")


