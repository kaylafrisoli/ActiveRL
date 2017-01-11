#' Block by variable
#'
#' Block a dataset by the factor levels of one variable
#'
#' @param records Data Frame containing records to be linked
#' @param var.name String of variable you want to block by
#' @return A vector of integers coresponding to the block assignment
#' @examples
#' BlockByVariable(iris, "Species")
BlockByVariable <- function(records, var.name) {
  # we could either make the user put quotes on "species" or fix it in the function using deparse(substitute(var.name)) or even do an if statement
  as.integer(as.factor(records[, var.name]))
}



#' Compare n records
#'
#' Number of comparisons made in n records
#'
#' @param n Number of records you want to compare
#' @return The total number of pair-comparisons in n records
#' @examples
#' Comparisons(dim(iris)[1])
Comparisons <- function(n){
  (n * (n-1)) / 2
}



#' Block by substrings
#'
#' Block by substrings of any length of multiple variables
#'
#' @param records Data Frame containing records to be linked
#' @param var.names Vector of strings of variables you want to block by
#' @param n.chars Vector of number of the characters you want to compare, respective to var.names
#' @return A list with the elements
#' \item{blocks}{vector of strings corresponding to the blocks}
#' \item{factors}{vector of integers corresponding to the blocks}
#' @examples
#' BlockBySubstr(iris, "Species") #identifies 2 blocks
#' BlockBySubstr(iris, "Species", 2) #identifies 3 blocks
#' BlockBySubstr(iris, c("Species", "Sepal.Length"), c(2,1)) #identifies 3 blocks
BlockBySubstr <- function(records, var.names, n.chars=NULL) {

  if(is.null(n.chars)){
    n.chars <- 1
  } else{
    n.chars <- n.chars
  }

  f1 <- function(x){substr(x, start=1, stop=n.chars)}
  new.mat <- t(apply(as.matrix(records[,var.names]), 1, f1))
  if (length(var.names) == 1){
    blocks <- as.factor(apply(new.mat, 2, paste, collapse=""))
  } else {
    blocks <- as.factor(apply(new.mat, 1, paste, collapse=""))
  }
  reduction.ratio <- 100 * (1 - (sum(Comparisons(as.numeric(table(blocks))))/ Comparisons(dim(records)[1])))
  results <- list(blocks=blocks, factors=as.integer(blocks), reduction.ratio=reduction.ratio)
  return(results)
}


library(RecordLinkage)
data("RLdata500")
data("RLdata10000")
head(RLdata500)
pass.structure <- list(matrix(c("fname_c1", "lname_c1", NA, NA), ncol = 2),
                       matrix(c("fname_c1", "by", NA, NA), ncol = 2),
                       matrix(c("fname_c1", "lname_c1", 3, 4), ncol = 2),
                       matrix(c("lname_c1", NA), ncol = 2))
records <- RLdata500
records <- RLdata10000
BlockInPasses <- function(records, pass.structure) {
  pairs.to.compare <- c()
  records$record.ids <- 1:nrow(records)
  for(i in 1:length(pass.structure)){
    # get substrings if necessary
    subs <- !is.na(pass.structure[[i]][, 2])
    if(any(subs)){
      f1 <- function(x){substr(x, start=1, stop=pass.structure[[i]][which(subs), 2])}
      new.mat <- t(apply(as.matrix(records[, pass.structure[[i]][which(subs), 1]]), 1, f1))
      new.mat <- cbind(new.mat, records[, pass.structure[[i]][which(!subs), 1]])
    } else{
      new.mat <- records[, pass.structure[[i]][, 1]]
    }
    # paste together strings to form blocks
    if (length(pass.structure[[i]][, 1]) == 1){
      blocks <- as.factor(new.mat)
    } else {
      blocks <- as.factor(apply(new.mat, 1, paste, collapse=""))
      names(blocks) <- NULL
    }
    # split the ids into blocks and get combinations
    orig.id.split <- split(records$record.ids, blocks)
    new.combs <- sapply(orig.id.split[as.numeric(which(sapply(orig.id.split, length) > 1))],
                        caTools::combs, k=2)
    new.combs <- as.data.frame(do.call(rbind, new.combs))
    colnames(new.combs)= c("min.id", "max.id")
    new.combs$blockid <- paste(apply(pass.structure[[i]], 1, paste, collapse=""),
                               collapse = "")
    # new.pairs <- do.call(rbind, new.combs)
    # sorted.new <- data.frame(apply(new.pairs, 1, min),
    #                          apply(new.pairs, 1, max))

    pairs.to.compare <- merge(pairs.to.compare, new.combs, )



    pairs.to.compare <- rbind(pairs.to.compare, new.combs)
    print(dim(pairs.to.compare))
    pairs.to.compare <- as.matrix(unique(data.frame(apply(pairs.to.compare, 1, min),
                                                    apply(pairs.to.compare, 1, max))))
    print(dim(pairs.to.compare))
    # colnames(pairs.to.compare) <- NULL
    # pairs.to.compare <- unique.pairs
  }
  return(pairs.to.compare)
}




#' Block a record linkage dataset
#'
#' Block a record linkage dataset by substrings of the variables in the dataset
#'
#' @param RLdata a data frame containing the records to be matched
#'
#' @param var.names a vector of strings containing the variable names you want to block by
#'
#' @param n.chars a vector of integers corresponding to the number of the characters you want to compare in each variable of var.names
#'
#' @param unique.ids a vector containing the true unique identifiers of the records in RLdata. It should be of length nrow(RLdata)
#'
#' @return A list containing blocking information and the blocked data and ids
#' \item{BlockInfo}{a list of blocking information: blocks, factors, reduction.ratio}
#' \item{DataSplit}{a list of datasets corresponding to each block}
#' \item{IdSplit}{a list of vectors containing the unique ids corresponding to each block}
#' @examples
#' BlockBySubstr(iris, "Species") #identifies 2 blocks
#' BlockBySubstr(iris, "Species", 2) #identifies 3 blocks
#' BlockBySubstr(iris, c("Species", "Sepal.Length"), c(2,1)) #identifies 3 blocks
#'
#' @export
BlockRlData <- function(RLdata,
                        var.names,
                        n.chars=NULL,
                        unique.ids=NULL,
                        pre.block.record=c(TRUE, FALSE)){


  options(expressions = 100000) # really should figure out what this means

  # full.comparisons <- matrix(NA, ncol = length(variables.to.match) + 3, nrow = choose(nrow(RLdata), 2))

  if(pre.block.record == TRUE){
    RLdata$PreBlockRecord <- 1:nrow(RLdata)
  }

  block.info <- BlockBySubstr(RLdata, var.names, n.chars)
  block.factors <- block.info$factors

  dsplit1 <- split(RLdata, block.factors)
  dsplit <- dsplit1[which(as.numeric(table(block.factors)) >= 2)]
  dsplit.singles <- MergeAllBlocks(dsplit1[which(as.numeric(table(block.factors)) < 2)])


  if(is.null(unique.ids)){
    unique.ids <- rep(NA, nrow(RLdata))
    id.split1 <- split(unique.ids, block.factors)
    id.split <- id.split1[which(as.numeric(table(block.factors)) >= 2)]
    id.split.singles <- unlist(id.split1[which(as.numeric(table(block.factors)) < 2)])
  } else{
    id.split1 <- split(unique.ids, block.factors)
    id.split <- id.split1[which(as.numeric(table(block.factors)) >= 2)]
    id.split.singles <- as.numeric(unlist(id.split1[which(as.numeric(table(block.factors)) < 2)]))
  }

  results <- list(BlockInfo = block.info,
                  DataSplit = dsplit,
                  IdSplit = id.split,
                  DataSplitSingles = dsplit.singles,
                  IdSplitSingles = id.split.singles)
  return(results)
}



#' Block a record linkage dataset adaptively by importance
#'
#' Block a record linkage dataset by substrings of the variables in the dataset
#'
#' @param RLdata a data frame containing the records to be matched
#'
#' @param var.names a vector of strings containing the variable names you want to block by
#'
#' @param n.chars a vector of integers corresponding to the number of the characters you want to compare in each variable of var.names
#'
#' @param unique.ids a vector containing the true unique identifiers of the records in RLdata. It should be of length nrow(RLdata)
#'
#' @return A list containing blocking information and the blocked data and ids
#' \item{BlockInfo}{a list of blocking information: blocks, factors, reduction.ratio}
#' \item{DataSplit}{a list of datasets corresponding to each block}
#' \item{IdSplit}{a list of vectors containing the unique ids corresponding to each block}
#' @examples
#' BlockBySubstr(iris, "Species") #identifies 2 blocks
#' BlockBySubstr(iris, "Species", 2) #identifies 3 blocks
#' BlockBySubstr(iris, c("Species", "Sepal.Length"), c(2,1)) #identifies 3 blocks
#'
#' @export
BlockRlDataAdapt <- function(RLdata,
                             var.names,
                             n.chars=NULL,
                             unique.ids=NULL,
                             max.size=NULL){

  if(is.null(max.size)){
    max.size <- 500
  } else{
    max.size <- max.size
  }

  RLdata$PreBlockRecord <- 1:nrow(RLdata)
  RLdata.loop <- RLdata
  id.loop <- unique.ids


  options(expressions = 100000) # really should figure out what this means

  blocks.total <- list()
  dsplit.total <- list()
  idsplit.total <- list()
  dsplit.single.total <- list()
  idsplit.single.total <- list()

  for(i in 1:length(var.names)){

    block.info <- BlockRlData(RLdata.loop,
                              var.names[1:i],
                              n.chars[1:i],
                              id.loop,
                              pre.block.record = FALSE)

    # block.too.big <- which(as.numeric(table(block.info$BlockInfo$blocks)) > max.size)
    block.just.right <- which(as.numeric(table(block.info$BlockInfo$blocks)) <= max.size)
    blocks.okayTF <- block.info$BlockInfo$blocks %in% names(table(block.info$BlockInfo$blocks)[block.just.right])
    blocks.total <- c(blocks.total, list(as.character(block.info$BlockInfo$blocks[blocks.okayTF])))

    dsplit.size <- sapply(block.info$DataSplit, nrow)

    too.big <- which(as.numeric(dsplit.size) > max.size)
    just.right <- which(as.numeric(dsplit.size) <= max.size)

    dsplit.total <- c(dsplit.total, block.info$DataSplit[just.right])
    idsplit.total <- c(idsplit.total, block.info$IdSplit[just.right])
    dsplit.single.total[[i]] <- block.info$DataSplitSingles
    idsplit.single.total[[i]] <- block.info$IdSplitSingles

    if(length(too.big) == 0) break

    RLdata.loop <- MergeAllBlocks(block.info$DataSplit[too.big])
    id.loop <- unlist(block.info$IdSplit[too.big])

  }

  total.blocks <- unlist(blocks.total)
  dsplit.singles <- MergeAllBlocks(dsplit.single.total)
  idsplit.singles <- unlist(idsplit.single.total)


  results <- list(BlockInfo = total.blocks,
                  DataSplit = dsplit.total,
                  IdSplit = idsplit.total,
                  DataSplitSingles = dsplit.singles,
                  IdSplitSingles = idsplit.singles)
  return(results)
}


