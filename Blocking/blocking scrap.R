
?BlockByVariable
BlockByVariable(iris, "Species")


?BlockBySubstr
BlockBySubstr(iris, "Species")
BlockBySubstr(iris, "Species", 1)
BlockBySubstr(iris, "Species", 2)
BlockBySubstr(iris, c("Species", "Sepal.Length"), c(2,1))


library(RecordLinkage)
data("RLdata500")
#View(RLdata500)

BlockBySubstr(RLdata500, c("fname_c1"), 2)
first.name <- BlockBySubstr(RLdata500, c("fname_c1"), 2)
first.name$blocks
first.name$factors
first.name$reduction.ratio
sort(table(first.name$factors))
sort(table(first.name$blocks))



BlockBySubstr(RLdata500, c("fname_c1", "lname_c1"), c(2, 3))
BlockBySubstr(RLdata500, c("fname_c1", "lname_c1", "by"), c(1, 1, 4))
fl.init.year <- BlockBySubstr(RLdata500, c("fname_c1", "lname_c1", "by"), c(1, 1, 4))
sort(table(fl.init.year$blocks))
sort(table(fl.init.year$factors))

fl.init.year <- BlockBySubstr(RLdata500, c("fname_c1", "lname_c1", "by"), c(1, 1, 2))
sort(table(fl.init.year$blocks))
sort(table(fl.init.year$factors))


## Reduction Ratio

## False negative error rate

## identity.RLdata500

unique(identity.RLdata500)

a <- c(1, 1, 2, 3, 4, 4, 4)

truly.match.pairs <- sum(Comparisons(as.numeric(table(identity.RLdata500))))




# #' Block by substring
# #'
# #' Block by substrings of up to 4 variables
# #'
# #' @param records Data Frame containing records to be linked
# #' @param var.name String of variable you want to block by
# #' @param n.chars Number of characters you want to compare, default is 1
# #' @return A printed vector of integers coresponding to the block
# #' @examples
# #' BlockBySubstr(iris, "Species") #identifies 2 blocks
# #' BlockBySubstr(iris, "Species", 2) #identifies 3 blocks
# BlockBySubstr <- function(records, var1.name, n1.chars=1, var2.name=NULL, n2.chars=1, var3.name=NULL, n3.chars=1, var4.name=NULL, n4.chars=1) {
#   as.integer(as.factor(paste(substr(records[, var1.name], 1, n1.chars),
#                              substr(records[, var2.name], 1, n2.chars),
#                              substr(records[, var3.name], 1, n3.chars),
#                              substr(records[, var4.name], 1, n4.chars), sep="")))
# }


# BlockBySubstrMV(RLdata500, "fname_c1", 2, "lname_c1", 2)
# BlockBySubstrMV(RLdata500, "fname_c1")
# BlockBySubstrMV(iris, "Species", 2)
#
# records <- RLdata500
# var.names <- c("fname_c1", "lname_c1")
# n.chars <- c(2, 3)
# records[1:50,var.names]
# # yo <- function(x){substr(x, start=1, stop=n.chars)}
# # t(apply(records[,var.names], 1, function(x){substr(x, start=1, stop=n.chars)}))
# #
# # apply(records[,var.names], 2, substr, start=1, stop=n.chars)[1
# #
#
# BlockBySubstrMV2(iris, "Species", 2)
# hi <- BlockBySubstrMV2(RLdata500, c("fname_c1", "lname_c1"), c(3,2))
# sort(table(hi))
# BlockBySubstrMV2(RLdata500, "fname_c1")
#
# f1 <- function(x){substr(x, start=1, stop=n.chars)}
#
#
# good <- t(apply(records[,var.names], 1, yo)[,1:50])
#
# BlockBySubstrMV2(iris, c("Species", "Sepal.Length"), c(2,1))
# sort(table(BlockBySubstrMV2(iris, c("Species", "Sepal.Length"), c(2,1))
# ))
