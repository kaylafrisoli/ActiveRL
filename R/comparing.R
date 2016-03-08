

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
  1 * (ids.from.original.data[combinations.of.original.data[, 1]] ==
         ids.from.original.data[combinations.of.original.data[, 2]])
}









#' Compare Unique Combinations of Records
#'
#' Match records from a dataset using any comparison function on any set of variables
#'
#' @param RLdata Dataset containing records to be matched
#'
#' @param ids True unique identification vector of length nrow(RLdata)
#'
#' @param variables.to.match Vector of strings containing the variables of interest for this linkage. Default is all variables in RLdata. Can repeat variables to use different comparators on same variable.
#'
#' @param string.comparators Vector of strings containing the comparators wanted for each variable. Default is jarowinkler. Should be same length as variables.to.match.
#'
CompareUniqueCombinations <- function(RLdata,
                                      ids,
                                      variables.to.match=NULL,
                                      string.comparators=NULL){

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



  # initialize the final matrix we will return
  final.matrix <- matrix(NA, ncol = cols + 3, nrow = choose(rows, 2))
  # put the unique combinations into our final matrix for future use
  final.matrix[, comb.cols] <- combinations(rows, 2)

  for (i in 1:cols) {
    # extract only the unique values from field of interest
    unique.entities <- unique(RLdata[, variables.to.match[i]])

    # is there more than 1 unique value?
    if (length(unique.entities) > 1) {
      unique.combs <- combinations(length(unique.entities), 2)
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

  final.matrix[, cols + 1] <- GetPairwiseMatchesFromIDs(final.matrix[, comb.cols], ids)
  colnames(final.matrix) <- c(paste(variables.to.match,
                                    substr(string.comparators, 1, 3),
                                    sep="."),
                              "True Match",
                              "Person 1", "Person 2")
  return(final.matrix)
}
