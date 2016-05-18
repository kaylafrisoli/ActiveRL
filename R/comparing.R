

#' Get pairwise matches from IDs
#'
#' Take a vector of unique IDs from original data set and convert to binary matches for the comparisons of each entity in original dataset
#'
#' @param combinations.of.original.data The combinations of your original data that you want to extract pairwise matches for
#' @param ids.from.original.data The vector of unique IDs from original data containing match information
#' @return A vector of binary non-match/match values for each combination
#' @examples
#' # use packages RecordLinkage and gtools
#' # return vector of nrow(our.combinations)
#' our.combinations <- combinations(nrow(RLdata500), 2)
#' GetPairwiseMatchesFromIDs(our.combinations, identity.RLdata500)
GetPairwiseMatchesFromIDs <- function(combinations.of.original.data,
                                      ids.from.original.data){
  # MUST be a matrix with 2 columns or it won't work with 2 variables
  combinations.of.original.data <- matrix(combinations.of.original.data, ncol=2)
  ids.from.original.data <- as.vector(ids.from.original.data)
  1 * (ids.from.original.data[combinations.of.original.data[, 1]] ==
         ids.from.original.data[combinations.of.original.data[, 2]])
}


# assume no intransitive matches
# GetIDsFromPairwiseMatches <- function(unique.combinations, match1){
#   uniq.ids <- rep(NA, length(unique(c(unique.combinations[,1],
#                                       unique.combinations[,2]))))
#   for(i in 1:seq_along(match1)){
#     if(match1[i] == 1){
#       uniq.ids[unique.combinations[i, 1]] <- unique.combinations[i, 1]
#       uniq.ids[unique.combinations[i, 2]] <- unique.combinations[i, 1]
#     } else{
#       uniq.ids[unique.combinations[i, 2]] <- unique.combinations[i, 2]
#     }
#
# uniq.ids[i] <-
#
#   }
#
# }





#' Compare Unique Combinations of Records
#'
#' Match records from a dataset using any comparison function on any set of variables
#'
#' @param RLdata Dataset containing records to be matched
#'
#' @param unique.ids True unique identification vector of length nrow(RLdata)
#'
#' @param variables.to.match Vector of strings containing the variables of interest for this linkage. Default is all variables in RLdata. Can repeat variables to use different comparators on same variable.
#'
#' @param string.comparators Vector of strings containing the comparators wanted for each variable. Default is jarowinkler. Should be same length as variables.to.match.
#'
#' @return A matrix of comparator values of each combination of records for the variables of interest. Also includes binary matches and the combinations of records.
#'
#' @export
CompareUniqueCombinations <- function(RLdata,
                                      unique.ids=NULL,
                                      variables.to.match=NULL,
                                      string.comparators=NULL,
                                      record.ids.to.keep=NULL){

  # set the default variables.to.match to every column of the RLdata
  if(is.null(variables.to.match)){
    variables.to.match <- names(RLdata)
  } else{
    variables.to.match <- variables.to.match
  }

  rows      <- nrow(RLdata) # number of rows (people) in our dataset
  cols      <- length(variables.to.match) # number of cols (fields) we want to match
  comb.cols <- (cols + 2) : (cols + 3) # cols where we'll put our unique combinations


  # set the default string.comparator to jarowinkler for each column
  if(is.null(string.comparators)){
    string.comparators <- rep("jarowinkler", cols)
  } else{
    string.comparators <- string.comparators
  }

  options(expressions = 100000)

  n.extra.cols <- ifelse(is.null(record.ids.to.keep), 0, length(record.ids.to.keep)*2)

  # initialize the final matrix we will return
  final.matrix <- matrix(NA, ncol = cols + 3 + n.extra.cols, nrow = choose(rows, 2))
  # put the unique combinations into our final matrix for future use
  # when we use a function that is part of our dependencies we use package::fun()
  final.matrix[, comb.cols] <- gtools::combinations(rows, 2)

  for (i in 1:cols) {
    # extract only the unique values from field of interest
    unique.entities <- unique(RLdata[, variables.to.match[i]])

    # is there more than 1 unique value?
    if (length(unique.entities) > 1) {
      unique.combs <- gtools::combinations(length(unique.entities), 2)
      my.combs <- apply(unique.combs, 2, function(x) as.character(unique.entities[x]))
      # access my.combs as a matrix even if there are just two values in it
      my.combs <- matrix(my.combs, ncol=2)
      # use get
      unique.comparisons <- get(string.comparators[i])(my.combs[, 1], my.combs[, 2])

      # initialize our unique matrix
      unique.mat <- matrix(NA, length(unique.entities), length(unique.entities))
      # put our unique comparisons into the unique matrix
      unique.mat[lower.tri(unique.mat)] <- unique.comparisons
      # make diagonal value 1 for name v. name or NA for NA v. NA
      diag(unique.mat) <- ifelse (is.na(unique.entities), NA, 1)

      # transpose the matrix to fill in the bottom half of the symmetric matrix
      unique.mat <- t(unique.mat)
      unique.mat[lower.tri(unique.mat)] <- unique.comparisons

      # now we want to match our unique entities back to the original dataset
      mapping      <- match(RLdata[, variables.to.match[i]], unique.entities)
      # we want a matrix size nrow(final.matrix) x 2 filled with the indices
      # of the unique matrix that match up with the combination
      # if orig combination (400, 300) is kayla kyle , we want the indices
      # from the unique matrix that are for kayla kyle which may be (4, 5)
      matrix.index <- matrix(mapping[final.matrix[, comb.cols]], ncol = 2)


      # apply the comparison values from unique.mat to final matrix
      final.matrix[, i] <- apply(matrix.index, 1, function(x) unique.mat[x[1], x[2]])

      # only one unique value in field and it is not NA
    } else if (is.na(unique.entities[1]) == FALSE & length(unique.entities) == 1) {
      final.matrix[, i] <- 1

      # NA ----> NA
    } else {
      final.matrix[, i] <- NA
    }

  }

  # set the default ids to NA unless they are known
  if(is.null(unique.ids)){
    final.matrix[, cols + 1] <- NA
  } else{
    final.matrix[, cols + 1] <- GetPairwiseMatchesFromIDs(final.matrix[, comb.cols], unique.ids)
  }


  if(!is.null(record.ids.to.keep)){

    n.record.ids.to.keep <- length(record.ids.to.keep)
    current.cols <- cols + 3
    recs.to.keep.names <- c()

    for(i in 1:n.record.ids.to.keep){
      final.matrix[, current.cols + (2*i) - 1] <- RLdata[, which(colnames(RLdata) %in% record.ids.to.keep[i])][final.matrix[, cols+2]]
      final.matrix[, current.cols + (2*i)] <- RLdata[, which(colnames(RLdata) %in% record.ids.to.keep[i])][final.matrix[, cols+3]]

      recs.to.keep.names <- c(recs.to.keep.names, paste(record.ids.to.keep[i], 1, sep=""), paste(record.ids.to.keep[i], 2, sep=""))

    }

    colnames(final.matrix) <- c(paste(variables.to.match,
                                      substr(string.comparators, 1, 3),
                                      sep="."),
                                "True_Match",
                                "CurrentRecord1", "CurrentRecord2",
                                recs.to.keep.names)


  } else{
    colnames(final.matrix) <- c(paste(variables.to.match,
                                      substr(string.comparators, 1, 3),
                                      sep="."),
                                "True_Match",
                                "CurrentRecord1", "CurrentRecord2")
  }

  return(as.data.frame(final.matrix))
}









#' Block and then Compare Unique Combinations of Records
#'
#' Block by substrings of any length of multiple variables from a dataset and then compare using any comparison function on any set of variables
#'
#' @param RLdata Dataset containing records to be matched
#'
#' @param var.names Vector of strings of variables you want to block by
#'
#' @param n.chars Vector of number of the characters you want to compare, respective to var.names
#'
#' @param ids True unique identification vector of length nrow(RLdata)
#'
#' @param variables.to.match Vector of strings containing the variables of interest for this linkage. Default is all variables in RLdata. Can repeat variables to use different comparators on same variable.
#'
#' @param string.comparators Vector of strings containing the comparators wanted for each variable. Default is jarowinkler. Should be same length as variables.to.match.
#'
#' @return A list of data and comparisons for each block
#' \item{block.comparison.lists}{list of datasets corresponding to the linkage comparisons for each block of data}
#' \item{block.data}{list of blocked datasets}
BlockAndCompareCombinations <- function(RLdata,
                                        var.names,
                                        n.chars=NULL,
                                        ids=NULL,
                                        variables.to.match=NULL,
                                        string.comparators=NULL){


  options(expressions = 100000) # really should figure out what this means

  # full.comparisons <- matrix(NA, ncol = length(variables.to.match) + 3, nrow = choose(nrow(RLdata), 2))
  block.by.sub <- BlockBySubstr(RLdata, var.names, n.chars)
  init.blocks <- block.by.sub$factors

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

  block.comparison.lists <- vector("list", length(dsplit))

  for(i in seq_along(dsplit)){
    matrix.wo.orig.ids <- CompareUniqueCombinations(as.data.frame(dsplit[[i]])[, -which(names(RLdata) %in% "OriginalID")],
                                                    as.vector(id.split[[i]]),
                                                    variables.to.match,
                                                    string.comparators)
    mat.wo.ncol <- ncol(matrix.wo.orig.ids)
    colnames(matrix.wo.orig.ids)[(mat.wo.ncol-1) : mat.wo.ncol] <- c("Record1", "Record2")

    OP1 <- as.vector(orig.id.split[[i]])[matrix.wo.orig.ids[, mat.wo.ncol - 1]]
    OP2 <- as.vector(orig.id.split[[i]])[matrix.wo.orig.ids[, mat.wo.ncol]]
    matrix.wo.orig.ids <- cbind(matrix.wo.orig.ids, OriginalRecord1=OP1, OriginalRecord2=OP2)

    block.comparison.lists[[i]] <- as.data.frame(matrix.wo.orig.ids)
  }
  full.comparisons <- plyr::rbind.fill(block.comparison.lists, rbind)


  results <- list(block.comparison.lists = block.comparison.lists,
                  block.data=dsplit,
                  full.comparisons=full.comparisons,
                  orig.id.split = orig.id.split,
                  blocks=block.by.sub)
  return(results)

}





StandardizeBetween01 <- function(x, our.max, our.min=NULL){
  if(is.null(our.min)){
    our.min <- 0
  } else{
    our.min <- our.min
  }
  our.min <- as.numeric(our.min)
  our.max <- as.numeric(our.max)
  x <- as.vector(x)
  (x-our.max)/(our.min-our.max)
}



AbsoluteDistance <- function(vec1, vec2){
  vec1 <- as.numeric(vec1)
  vec2 <- as.numeric(vec2)
  max.val <- max(c(vec1, vec2))
  ab.dist <- abs(vec1 - vec2)
  min.dist <- pmin(ab.dist, max.val-ab.dist)
  return(min.dist)
}

StandardizedAbsoluteDistance <- function(vec1, vec2){
  vec1 <- as.numeric(vec1)
  vec2 <- as.numeric(vec2)
  max.val <- max(c(vec1, vec2))
  ab.dist <- abs(vec1 - vec2)
  min.dist <- pmin(ab.dist, max.val-ab.dist)

  # Standardize
  return(StandardizeBetween01(min.dist))
}



StandardizedAbsoluteDistanceM <- function(vec1, vec2){
  vec1 <- as.numeric(vec1)
  vec2 <- as.numeric(vec2)
  max.val <- max(c(vec1, vec2))
  ab.dist <- abs(vec1 - vec2)
  min.dist <- pmin(ab.dist, max.val-ab.dist)

  # Standardize
  return(StandardizeBetween01(min.dist, 12))
}


StandardizedAbsoluteDistanceD <- function(vec1, vec2){
  vec1 <- as.numeric(vec1)
  vec2 <- as.numeric(vec2)
  max.val <- max(c(vec1, vec2))
  ab.dist <- abs(vec1 - vec2)
  min.dist <- pmin(ab.dist, max.val-ab.dist)

  # Standardize
  return(StandardizeBetween01(min.dist, 31))
}



AbsoluteDifference <- function(vec1, vec2){
  ab.dif <- as.numeric(abs(as.numeric(vec1)-as.numeric(vec2)))
  return(ab.dif)
}

AbsoluteDifferenceDate <- function(vec1, vec2){
  ab.dif <- as.numeric(abs(difftime(vec1 , vec2)))
  return(ab.dif)
}



StandardizedAbsoluteDifference <- function(vec1, vec2){
  ab.dif <- abs(as.numeric(vec1)-as.numeric(vec2))
  return(StandardizeBetween01(ab.dif))
}

StandardizedAbsoluteDifferenceY <- function(vec1, vec2){
  ab.dif <- abs(as.numeric(vec1)-as.numeric(vec2))
  return(StandardizeBetween01(ab.dif, max(c(vec1, vec2, 65))))
}






#' Compare unique combinations of records in all blocks
#'
#' Compare unique combinations of records within every block of a dataset
#'
#' @param Dsplit a list of dataset containing records to be matched for each block
#'
#' @param Idsplit a list of vectors containing the unique ids corresponding to each block
#'
#' @param variables.to.match a vector of strings containing the variables of interest for this linkage. Default is all variables in RLdata. Can repeat variables to use different comparators on same variable.
#'
#' @param string.comparators a vector of strings containing the comparator to be used for each variable. Default is jarowinkler for all. Should be same length as variables.to.match.
#'
#' @param record.ids.to.keep a vector of strings containing the name of any record id that should be kept. e.g. PreSplitRecord or  PreBlockRecord
#'
#' @return A list of data frames containing the comparison matrix for each block
#' @export
CompareAllBlocksInLoop <- function(Dsplit,
                                   Idsplit=NULL,
                                   variables.to.match=NULL,
                                   string.comparators=NULL,
                                   record.ids.to.keep=NULL){

block.comparison.lists <- vector("list", length(Dsplit))

for(i in seq_along(Dsplit)){

#   ids.for.loop <- vector("list", length(Dsplit))
  if(is.null(Idsplit)){
    ids.for.loop <- NULL
  } else{
    ids.for.loop <- Idsplit[[i]]
  }

  comparison.in.block <- CompareUniqueCombinations(as.data.frame(Dsplit[[i]]),
                                                  as.vector(ids.for.loop),
                                                  variables.to.match = variables.to.match,
                                                  string.comparators = string.comparators,
                                                  record.ids.to.keep = record.ids.to.keep)

  block.comparison.lists[[i]] <- as.data.frame(comparison.in.block)
}

return(block.comparison.lists)

}




#' Compare unique combinations of records in all blocks using parallel structure
#'
#' Compare unique combinations of records within every block of a dataset using parallel structure
#'
#' @param Dsplit a list of dataset containing records to be matched for each block
#'
#' @param Idsplit a list of vectors containing the unique ids corresponding to each block
#'
#' @param variables.to.match a vector of strings containing the variables of interest for this linkage. Default is all variables in RLdata. Can repeat variables to use different comparators on same variable.
#'
#' @param string.comparators a vector of strings containing the comparator to be used for each variable. Default is jarowinkler for all. Should be same length as variables.to.match.
#'
#' @param record.ids.to.keep a vector of strings containing the name of any record id that should be kept. e.g. PreSplitRecord or  PreBlockRecord
#'
#' @return A list of data frames containing the comparison matrix for each block
#' @export
CompareAllBlocksInLoopPC <- function(Dsplit,
                                     Idsplit=NULL,
                                     variables.to.match=NULL,
                                     string.comparators=NULL,
                                     record.ids.to.keep=NULL,
                                     num.cores=NULL){

  if(is.null(num.cores)){
    num.cores <- 1
  } else{
    num.cores <- num.cores
  }


  block.comparison.lists <- vector("list", length(Dsplit))

  options(cores = num.cores)
  registerDoMC()

  block.comparison.lists <- doMC::foreach(i = seq_along(Dsplit)) %dopar% {

    #   ids.for.loop <- vector("list", length(Dsplit))
    if(is.null(Idsplit)){
      ids.for.loop <- NULL
    } else{
      ids.for.loop <- Idsplit[[i]]
    }

    comparison.in.block <- as.data.frame(CompareUniqueCombinations(as.data.frame(Dsplit[[i]]),
                                                     as.vector(ids.for.loop),
                                                     variables.to.match = variables.to.match,
                                                     string.comparators = string.comparators,
                                                     record.ids.to.keep = record.ids.to.keep))

  }

  return(block.comparison.lists)

}



#make sure all elements of block.comparison.list are a data.frame



#' Merge (rbind) all datasets in a list
#'
#' @param block.lists a list of datasets
#'
#' @return A data frame with all elements of block.lists merged together
#' @export
MergeAllBlocks <- function(block.lists){

  full.comparisons <- plyr::rbind.fill(block.lists, rbind)

  return(full.comparisons)

}






