


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



HclustGLM <- function(model, comparison.test, fullTestRL){

  test.preds <- predict(model, comparison.test, type="response")
  test.dissims <- 1-test.preds

  glm.dist.mat <- matrix(NA, nrow = nrow(fullTestRL), ncol=nrow(fullTestRL))
  glm.dist.mat[lower.tri(glm.dist.mat)] <- test.dissims
  average.distance <- as.dist(glm.dist.mat, diag=FALSE, upper=FALSE)
  hclust.glm <- hclust(average.distance)
  return(hclust.glm)
}




HclustCutGLM <- function(model, comparison.test, fullTestRL, cut.threshold){

  test.preds <- predict(model, comparison.test, type="response")
  test.dissims <- 1-test.preds

  glm.dist.mat <- matrix(NA, nrow = nrow(fullTestRL), ncol=nrow(fullTestRL))
  glm.dist.mat[lower.tri(glm.dist.mat)] <- test.dissims
  average.distance <- as.dist(glm.dist.mat, diag=FALSE, upper=FALSE)
  hclust.glm <- hclust(average.distance)
  cut.glm.hclust <- cutree(hclust.glm, h=cut.threshold)
  return(cut.glm.hclust)
}


#' @export
AllBlocksHclustCutGLM <- function(model, comparison.test.blocks, test.data.blocks, cut.threshold, current.record.names=NULL){

  if(is.null(current.record.names)){
    current.record.names <- c("CurrentRecord1", "CurrentRecord2")
  } else{
    current.record.names <- current.record.names
  }

  block.hclust.ids <- vector("list", length(comparison.test.blocks))

  for(i in seq_along(comparison.test.blocks)){
    print(i)

    comparison.test.blocks[[i]] <- as.data.frame(comparison.test.blocks[[i]])

    test.data.blocks[[i]] <- as.data.frame(test.data.blocks[[i]])

    hclust.uids <- HclustCutGLM(model,
                                comparison.test.blocks[[i]],
                                test.data.blocks[[i]],
                                cut.threshold)

    block.hclust.ids[[i]] <- paste("block",
                                   i,
                                   "uid",
                                   hclust.uids,
                                   sep = "")


    test.data.blocks[[i]]$HclustUID <- block.hclust.ids[[i]]


    pairwise.matches <- GetPairwiseMatchesFromIDs(as.matrix(comparison.test.blocks[[i]][, which(colnames(comparison.test.blocks[[i]]) %in% current.record.names)]), as.vector(block.hclust.ids[[i]]))

    comparison.test.blocks[[i]]$HclustMatch <- pairwise.matches




  }


  merged.block.data <- MergeAllBlocks(test.data.blocks)
  merged.comparison.data <- MergeAllBlocks(comparison.test.blocks)

  results <- list(block.hclust.ids=block.hclust.ids,
                  comparison.test.blocks=comparison.test.blocks,
                  test.data.blocks=test.data.blocks,
                  merged.comparison.data=merged.comparison.data,
                  merged.block.data=merged.block.data)

  return(results)


}




HclustCutGLM2 <- function(model, comparison.test, fullTestRL, cut.threshold){

  test.preds <- predict(model, comparison.test, type="response")
  test.dissims <- 1-test.preds

  glm.dist.mat <- matrix(NA, nrow = nrow(fullTestRL), ncol=nrow(fullTestRL))
  glm.dist.mat[lower.tri(glm.dist.mat)] <- test.dissims
  average.distance <- as.dist(glm.dist.mat, diag=FALSE, upper=FALSE)
  hclust.glm <- hclust(average.distance)
  cut.glm.hclust <- cutree(hclust.glm, h=cut.threshold)
  return(cut.glm.hclust)
}




#' @export
AllBlocksHclustCutGLM2 <- function(model, comparison.test.blocks, test.data.blocks, cut.threshold, current.record.names=NULL){

  if(is.null(current.record.names)){
    current.record.names <- c("CurrentRecord1", "CurrentRecord2")
  } else{
    current.record.names <- current.record.names
  }

  block.hclust.ids <- vector("list", length(comparison.test.blocks))

  for(i in seq_along(comparison.test.blocks)){

    comparison.test.blocks[[i]] <- as.data.frame(comparison.test.blocks[[i]])

    test.data.blocks[[i]] <- as.data.frame(test.data.blocks[[i]])

    hclust.uids <- HclustCutGLM2(model,
                                comparison.test.blocks[[i]],
                                test.data.blocks[[i]],
                                cut.threshold)

    block.hclust.ids[[i]] <- paste("block",
                                   i,
                                   "uid",
                                   hclust.uids,
                                   sep = "")


    test.data.blocks[[i]]$HclustUID <- block.hclust.ids[[i]]


    pairwise.matches <- GetPairwiseMatchesFromIDs(as.matrix(comparison.test.blocks[[i]][, which(colnames(comparison.test.blocks[[i]]) %in% current.record.names)]), as.vector(block.hclust.ids[[i]]))

    comparison.test.blocks[[i]]$HclustMatch <- pairwise.matches


  }


  merged.block.data <- MergeAllBlocks(test.data.blocks)
  merged.comparison.data <- MergeAllBlocks(comparison.test.blocks)

  results <- list(block.hclust.ids=block.hclust.ids,
                  comparison.test.blocks=comparison.test.blocks,
                  test.data.blocks=test.data.blocks,
                  merged.comparison.data=merged.comparison.data,
                  merged.block.data=merged.block.data)

  return(results)


}







