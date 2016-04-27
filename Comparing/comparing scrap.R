?GetPairwiseMatchesFromIDs

library(RecordLinkage)
library("devtools")
devtools::load_all()
data("RLdata500")
library(roxygen2)
# write the documentation on top of function
devtools::document() # update documentation


hi <- combinations(nrow(RLdata500), 2)
hello <- GetPairwiseMatchesFromIDs(hi, identity.RLdata500)
hello2 <- GetPairwiseMatchesFromIDs(combinations(nrow(RLdata500), 2), identity.RLdata500) # doesn't work


?CompareUniqueCombinations

ids500 <- identity.RLdata500
jaro.matrixU <- CompareUniqueCombinations(RLdata500, ids500)

jaro.matrixNoID <- CompareUniqueCombinations(RLdata500)


combo.matrixU2 <- CompareUniqueCombinations(RLdata=RLdata500,
                                           ids=ids500,
                                           string.comparators = c("levenshteinSim", "jarowinkler",
                                                                  "jarowinkler", "jarowinkler",
                                                                  "levenshteinSim", "jarowinkler",
                                                                  "levenshteinSim"))






options(expressions = 100000)
setwd("~/Documents/CMU/Census/Package/BasicRL")
library("devtools")
devtools::load_all()
?BlockBySubstr

data("RLdata10000")


init.blocks <- BlockBySubstr(RLdata10000, "fname_c1", 3)$factors

dsplit <- split(RLdata10000, init.blocks)
dsplit <- dsplit[which(as.numeric(table(init.blocks)) >= 2)]

original.ids <- 1:nrow(RLdata10000)

orig.id.split <- split(peeps, init.blocks)
orig.id.split <- orig.id.split[which(as.numeric(table(init.blocks)) >= 2)]

RLdata10000$ids <- identity.RLdata10000
id.split <- split(RLdata10000$ids, init.blocks)
id.split <- id.split[which(as.numeric(table(init.blocks)) >= 2)]


jaro.list10000 <- vector("list", length(dsplit))

for(i in seq_along(dsplit)){
  jaro.list10000[[i]] <- CompareUniqueCombinations( RLdata=dsplit[[i]],
                                                    ids=id.split[[i]])
}




BlockAndCompareCombinations <- function(RLdata,
                                        var.names,
                                        n.chars=NULL,
                                        ids=NULL,
                                        variables.to.match=NULL,
                                        string.comparators=NULL){


  options(expressions = 100000) # really should figure out what this means

  init.blocks <- BlockBySubstr(RLdata, var.names, n.chars)$factors

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
    matrix.wo.orig.ids <- CompareUniqueCombinations(as.data.frame(dsplit[[i]]),
                                                    as.vector(id.split[[i]]),
                                                    variables.to.match,
                                                    string.comparators)
    mat.wo.ncol <- ncol(matrix.wo.orig.ids)

    OP1 <- as.vector(orig.id.split[[i]])[matrix.wo.orig.ids[, mat.wo.ncol - 1]]
    OP2 <- as.vector(orig.id.split[[i]])[matrix.wo.orig.ids[, mat.wo.ncol]]
    matrix.wo.orig.ids <- cbind(matrix.wo.orig.ids, OriginalPerson1=OP1, OriginalPerson2=OP2)

    block.comparison.lists[[i]] <- matrix.wo.orig.ids
  }
  results <- list(block.comparison.lists = block.comparison.lists, block.data=dsplit)
  return(results)

}




our.lists <- BlockAndCompareCombinations(RLdata10000,
                                         "fname_c1",
                                         3,
                                         identity.RLdata10000)

View(our.lists$block.data[[1]])
View(our.lists$block.comparison.lists[[1]])



?BlockAndCompareCombinations


