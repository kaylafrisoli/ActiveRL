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

#' Block a record linkage dataset in passes
#'
#' Block a record linkage dataset in passes of different blocking schemes
#'
#' @param records a data frame containing the records to be matched
#'
#' @param pass.structure a list containing a matrix for each pass where the first column of the matrix contains the variables to block on and the second column contains the number of characters to use (NA will use the entire variable)
#'
#' @return A data frame containing the ids of records we will compare and the blocking scheme used to choose them
#'
#' @examples
#' BlockInPasses(RLdata10000, list(matrix(c("fname_c1", "lname_c1", NA, NA), ncol = 2),
#'                                 matrix(c("fname_c1", "by", NA, NA), ncol = 2),
#'                                 matrix(c("fname_c1", "lname_c1", 3, 4), ncol = 2),
#'                                 matrix(c("lname_c1", NA), ncol = 2)))
#'
#' BlockInPasses(RLdata500, list(matrix(c("fname_c1", "lname_c1", "by", 1, 2, NA), ncol = 2)))
#'
#' @export
BlockInPasses <- function(records, pass.structure, verbose=FALSE) {
  pairs.to.compare <- c()
  records$record.ids <- 1:nrow(records)
  for(i in 1:length(pass.structure)){
    if(verbose) print(i)
    # get substrings if necessary
    start.time <- Sys.time()
    subs <- !is.na(pass.structure[[i]][, 2])
    # because of data frame size differences we have to split into
    # three cases even though we're doing the same thing
    # if we are substringing 2+ variables, substring them and add them to the
    # others (if there are any)
    if(sum(subs) > 1){
      f1 <- function(x){substr(x, start=1, stop=as.numeric(pass.structure[[i]][which(subs), 2]))}
      new.mat <- t(apply(as.matrix(records[pass.structure[[i]][which(subs), 1]]), 1, f1))
      new.mat <- cbind(new.mat, records[ pass.structure[[i]][which(!subs), 1]])
    # if there is only one to be substringed, we don't need apply
    }else if(sum(subs) == 1){
      new.mat <- substr(as.matrix(records[pass.structure[[i]][which(subs), 1]]),
                        start=1,
                        stop=as.numeric(pass.structure[[i]][which(subs), 2]))
      new.mat <- cbind(new.mat, records[ pass.structure[[i]][which(!subs), 1]])
    # if we aren't substringing then we just subset our data to the variables
    # we are blocking on
    } else{
      new.mat <- records[pass.structure[[i]][,1]]
    }
    end.time <- Sys.time()
    time.taken <- end.time - start.time
    if(verbose) print(paste0("substringing: ", time.taken))
    # paste together strings to form blocks

    start.time <- Sys.time()
    blocks <- as.factor(apply(new.mat, 1, paste, collapse=""))
    names(blocks) <- NULL
    end.time <- Sys.time()
    time.taken <- end.time - start.time
    if(verbose) print(paste0("creating blocks: ", time.taken))

    # split the ids into blocks and get combinations
    start.time <- Sys.time()
    orig.id.split <- split(records$record.ids, blocks)
    end.time <- Sys.time()
    time.taken <- end.time - start.time
    if(verbose) print(paste0("splitting ids: ", time.taken))

    start.time <- Sys.time()
    z <- sapply(orig.id.split[as.numeric(which(sapply(orig.id.split, length) > 1))],
                sort)
    x <- sapply(z, caTools::combs, k=2)
    end.time <- Sys.time()
    time.taken <- end.time - start.time
    if(verbose) print(paste0("getting combos: ", time.taken))

    for(j in y){
      our.combs <- caTools::combs(orig.id.split[j], k=2)
      new.combs <- data.frame(min.id=apply(our.combs, 1, min),
                              max.id=apply(our.combs, 1, max),
                              blockid=paste(apply(pass.structure[[i]], 1, paste, collapse=""),
                                            collapse = ""))
      pairs.to.compare <- rbind.fill(pairs.to.compare, new.combs)
      pairs.to.compare <- pairs.to.compare[!duplicated(pairs.to.compare[1:2]), ]

    }

    start.time <- Sys.time()

    new.combs <- plyr::rbind.fill.matrix(x)
    # new.combs <- as.data.frame(do.call(rbind,
    #                                    sapply(orig.id.split[as.numeric(which(sapply(orig.id.split,
    #                                                                                 length) > 1))],
    #                                           caTools::combs, k=2)))
    end.time <- Sys.time()
    time.taken <- end.time - start.time
    if(verbose) print(paste0("getting combos: ", time.taken))

    start.time <- Sys.time()

    new.combs <- data.frame(min.id=apply(new.combs, 1, min),
                            max.id=apply(new.combs, 1, max),
                            blockid=paste(apply(pass.structure[[i]], 1, paste, collapse=""),
                                          collapse = ""))
    pairs.to.compare <- rbind(pairs.to.compare, new.combs)
    if(verbose) print(dim(pairs.to.compare))
    end.time <- Sys.time()
    time.taken <- end.time - start.time
    if(verbose) print(paste0("order/combine: ", time.taken))

   start.time <- Sys.time()
    pairs.to.compare <- pairs.to.compare[!duplicated(pairs.to.compare[1:2]), ]
    end.time <- Sys.time()
    time.taken <- end.time - start.time
    if(verbose) print(paste0("deduping: ", time.taken))

    if(verbose)  print(dim(pairs.to.compare))
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


