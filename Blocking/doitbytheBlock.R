
#########
# In order to start utilizing parallelization we need independent runs
#########

# this should return information about the blocks too!





################
# Block
################

BlockAndPrepareforComparison <- function(RLdata,
                                        var.names,
                                        n.chars=NULL,
                                        ids=NULL){


  options(expressions = 100000) # really should figure out what this means

  # full.comparisons <- matrix(NA, ncol = length(variables.to.match) + 3, nrow = choose(nrow(RLdata), 2))
  init.blocks <- BlockBySubstr(RLdata, var.names, n.chars)$factors

  RLdata$OriginalID <- 1:nrow(RLdata)
  dsplit <- split(RLdata, init.blocks)
  dsplit <- dsplit[which(as.numeric(table(init.blocks)) >= 2)]

  original.ids <- 1:nrow(RLdata)
  orig.id.split <- split(original.ids, init.blocks)
  orig.id.split <- orig.id.split[which(as.numeric(table(init.blocks)) >= 2)]

  if(is.null(ids)){
    ids <- rep(NA, nrow(RLdata))
    id.split <- split(ids, init.blocks)
    id.split <- id.split[which(as.numeric(table(init.blocks)) >= 2)]
  } else{
    id.split <- split(ids, init.blocks)
    id.split <- id.split[which(as.numeric(table(init.blocks)) >= 2)]
  }

  results <- list(Blocks = init.blocks, DataSplit = dsplit, IdSplit = id.split)
  return(results)
}

################
# Compare
################


CompareInABlock <- function(Dsplit,
                          Idsplit,
                          variables.to.match=NULL,
                          string.comparators=NULL){

  if(is.data.frame(Dsplit)){
    Dsplit <- list(Dsplit)
    Idsplit <- list(Idsplit)
  }


  block.comparison.lists <- vector("list", length(Dsplit))

  for(i in seq_along(Dsplit)){
    matrix.wo.orig.ids <- CompareUniqueCombinations(as.data.frame(Dsplit[[i]]),
                                                    as.vector(Idsplit[[i]]),
                                                    variables.to.match,
                                                    string.comparators)
    mat.wo.ncol <- ncol(matrix.wo.orig.ids)
    colnames(matrix.wo.orig.ids)[(mat.wo.ncol-1) : mat.wo.ncol] <- c("Record1", "Record2")

    OP1 <- as.vector(Dsplit[[i]]$OriginalID)[matrix.wo.orig.ids[, mat.wo.ncol - 1]]
    OP2 <- as.vector(Dsplit[[i]]$OriginalID)[matrix.wo.orig.ids[, mat.wo.ncol]]
    matrix.wo.orig.ids <- cbind(matrix.wo.orig.ids, OriginalRecord1=OP1, OriginalRecord2=OP2)

    block.comparison.lists[[i]] <- as.data.frame(matrix.wo.orig.ids)
  }
  full.comparisons <- plyr::rbind.fill(block.comparison.lists, rbind)


  results <- list(block.comparison.lists = block.comparison.lists,
                  block.data=Dsplit,
                  full.comparisons=full.comparisons)
  return(results)

}



#######################
# Predict and assign
#######################

AdaptedGLM <- function(formula, comparison.train, n=NULL){

  comparison.train <- as.data.frame(comparison.train)

  print(n)

    if(is.null(n)){
      training.adapted <- comparison.train
      adapted.glm.train <- glm(formula,
                               data = training.adapted,
                               family = binomial)
    } else{
      n <- n
      training.match <- which(comparison.train$True_Match == 1)
      training.nonmatch <- which(comparison.train$True_Match != 1)

      training.nonmatch.sample <- sample(training.nonmatch, (length(training.match)*n))
      training.adapted <- comparison.train[c(training.match, training.nonmatch.sample), ]

      adapted.glm.train <- glm(formula,
                               data = training.adapted,
                               family = binomial)
    }

  return(adapted.glm.train)
}


HclustGLM <- function(glm.train, comparison.test, fullTestRL, cut.threshold){

  test.preds <- predict(glm.train, comparison.test, type="response")
  test.dissims <- 1-test.preds

  glm.dist.mat <- matrix(NA, nrow = nrow(fullTestRL), ncol=nrow(fullTestRL))
  glm.dist.mat[lower.tri(glm.dist.mat)] <- test.dissims
  average.distance <- as.dist(glm.dist.mat, diag=FALSE, upper=FALSE)
  hclust.glm <- hclust(average.distance)
  cut.glm.hclust <- cutree(hclust.glm, h=cut.threshold)
  return(cut.glm.hclust)
}

# HclustAdaptedGLM <- function(comparison.train, comparison.test,  n=NULL, fullTestRL){
#
#   comparison.train <- as.data.frame(comparison.train)
#
#   if(is.null(n)){
#     training.adapted <- comparison.train
#   } else{
#     training.match <- which(comparison.train$True_Match == 1)
#     training.nonmatch <- which(comparison.train$True_Match != 1)
#
#     training.nonmatch.sample <- sample(training.nonmatch, (length(training.match)*n))
#     training.adapted <- comparison.train[c(training.match, training.nonmatch.sample), ]
#   }
#
#   glm.train <- glm(True_Match ~ fname_c1.jar + lname_c1.jar + by.Sta + bm.Sta + bd.Sta,
#                    data = training.adapted,
#                    family = binomial)
#
#   test.preds <- predict(glm.train, comparison.test, type="response")
#
#   average.dist.mat <- matrix(NA, nrow = nrow(fullTestRL), ncol=nrow(fullTestRL))
#   average.dist.mat[lower.tri(average.dist.mat)] <- test.preds
#   average.distance <- as.dist(average.dist.mat, diag=FALSE, upper=FALSE)
#   hclust.glm <- hclust(average.distance)
#   return(hclust.glm)
# }



##################
# By block
##################


CompleteRlByBlock <- function(Dsplit,
                              Idsplit,
                              seed=NULL,
                              prob.of.train,
                              variables.to.match=NULL,
                              string.comparators=NULL,
                              n=NULL,
                              formula,
                              cut.threshold){

  if(is.null(seed)){
    seed <- sample(1:100000, 1)
  } else{
    seed <- seed
  }

  set.seed(seed)
  train <- sample(c(TRUE,FALSE), nrow(Dsplit), rep=TRUE, prob = c(prob.of.train, 1-prob.of.train))
  test <- (! train)
  training.data <- Dsplit[train, ]
  testing.data  <- Dsplit[test, ]
  train.id <- Idsplit[train]
  test.id <- Idsplit[test]

  train.compare <- CompareInABlock(training.data,
                                   train.id,
                                   variables.to.match,
                                   string.comparators)$full.comparisons

  test.compare <- CompareInABlock(testing.data,
                                  test.id,
                                  variables.to.match,
                                  string.comparators)$full.comparisons

  creating.glm <- AdaptedGLM(formula=formula, comparison.train=train.compare, n=n)

  hclust.test <- HclustGLM(creating.glm, test.compare, testing.data, cut.threshold)

  results <- list(seed=seed,
                  hclust.results=hclust.test,
                  training.data=training.data,
                  testing.data=testing.data,
                  train.ids=train.id,
                  test.ids=test.id,
                  train.comparisons = train.compare,
                  test.comparisons = test.compare)
  return(results)

}

