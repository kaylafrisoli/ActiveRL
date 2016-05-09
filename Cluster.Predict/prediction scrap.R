library(RecordLinkage)
library("devtools")
devtools::load_all()
data("RLdata500")
data("RLdata10000")


ids500 <- identity.RLdata500

absolute.difference <- function(vec1, vec2){
  abs(as.numeric(vec1)-as.numeric(vec2))
}


pred.matrix1 <- CompareUniqueCombinations(RLdata=RLdata500,
                                          ids=ids500,
                                          variables.to.match = c("fname_c1", "lname_c1", "by", "bm", "bd"),
                                          string.comparators = c("jarowinkler",
                                                                 "jarowinkler",
                                                                 "absolute.difference",
                                                                 "absolute.difference",
                                                                 "absolute.difference"))

pred.data1 <- as.data.frame(pred.matrix1)


rl500.glm <- glm(True_Match~fname_c1.jar + lname_c1.jar + by.abs + bm.abs + bd.abs,
                 data=pred.data1,
                 family="binomial")
summary(rl500.glm)

# Creating training and testing

set.seed(6)
train <- sample(c(TRUE,FALSE), nrow(pred.data1), rep=TRUE, prob = c(.75, .25))
test <- (! train)

# Training model - check how it does on its training data

rl500.glmTRAIN <- glm(True_Match ~ fname_c1.jar + lname_c1.jar + by.abs + bm.abs + bd.abs,
                      data=pred.data1[train,],
                      family=binomial)
summary(rl500.glmTRAIN)

preds <- round(predict(rl500.glmTRAIN, type="response"))
hist(preds)
train.v.pred <- as.data.frame(cbind(pred.data1$True_Match[train], preds))
colnames(train.v.pred) <- c("actual", "predicted")
table(train.v.pred)

# Test on testing data

test.preds <- round(predict(rl500.glmTRAIN, pred.data1[test,], type="response"))
hist(test.preds)
test.v.pred <- as.data.frame(cbind(pred.data1$True_Match[test], test.preds))
colnames(test.v.pred) <- c("actual", "predicted")
table(test.v.pred)





###########################
###########################
###########################
# Let's try our bigger data
###########################
###########################
###########################


data("RLdata10000")
ids10000 <- identity.RLdata10000
block.pred <- BlockAndCompareCombinations(RLdata=RLdata10000,
                                          var.names = c("fname_c1"),
                                          n.chars = 3,
                                          ids=ids10000,
                                          variables.to.match = c("fname_c1", "lname_c1", "by", "bm", "bd"),
                                          string.comparators = c("jarowinkler",
                                                                 "jarowinkler",
                                                                 "absolute.difference",
                                                                 "absolute.difference",
                                                                 "absolute.difference"))

###########################
# Different breakdown
###########################


data("RLdata10000")
ids10000 <- identity.RLdata10000
block.pred <- BlockAndCompareCombinations(RLdata=RLdata10000,
                                          var.names = c("fname_c1", "lname_c1"),
                                          n.chars = c(1, 1),
                                          ids=ids10000,
                                          variables.to.match = c("fname_c1", "lname_c1", "by", "bm", "bd"),
                                          string.comparators = c("jarowinkler",
                                                                 "jarowinkler",
                                                                 "absolute.difference",
                                                                 "absolute.difference",
                                                                 "absolute.difference"))


big.mat <- c()
for(i in seq_along(block.pred$block.comparison.lists)){
  big.mat <- rbind(big.mat, block.pred$block.comparison.lists[[i]])
}

# look at dplyr for combining data frames

# But here we are leaving out those groups with two people... 1 is okay because we will call that a unique entity

# Original model

big.rl.data <- as.data.frame(big.mat)

rl10000.glm <- glm(True_Match ~ fname_c1.jar + lname_c1.jar + by.abs + bm.abs + bd.abs, data=big.rl.data, response="binomial")
summary(rl10000.glm)

# Creating training and testing

set.seed(6)
train <- sample(c(TRUE,FALSE), nrow(big.rl.data), rep=TRUE, prob = c(.75, .25))
test <- (! train)


# Training model - check how it does on its training data

rl10000.glmTRAIN <- glm(True_Match ~ fname_c1.jar + lname_c1.jar + by.abs + bm.abs + bd.abs,
                      data=big.rl.data[train,],
                      family=binomial)
summary(rl10000.glmTRAIN)

preds <- round(predict(rl10000.glmTRAIN, type="response"))
hist(preds)
train.v.pred <- as.data.frame(cbind(big.rl.data$True_Match[train], preds))
colnames(train.v.pred) <- c("actual", "predicted")
table(train.v.pred)

# Not super great!

# Test on testing data

test.preds <- round(predict(rl10000.glmTRAIN, big.rl.data[test, ], type="response"))
#hist(test.preds)
test.v.pred <- as.data.frame(cbind(big.rl.data$True_Match[test], test.preds))
colnames(test.v.pred) <- c("actual", "predicted")
table(test.v.pred)






















##########################
## Actual Scrap Don't Need
##########################



# Trying to figure out 2 issue... FIXED IT !! WOO!!

RLdata=RLdata10000
var.names = c("fname_c1", "lname_c1")
n.chars = c(1, 1)
ids=ids10000
variables.to.match = c("fname_c1", "lname_c1", "by", "bm", "bd")
string.comparators = c("jarowinkler",
                       "jarowinkler",
                       "absolute.difference",
                       "absolute.difference",
                       "absolute.difference")

options(expressions = 100000) # really should figure out what this means

init.blocks <- BlockBySubstr(RLdata, var.names, n.chars)$factors

dsplit <- split(RLdata, init.blocks)
dsplit <- dsplit[which(as.numeric(table(init.blocks)) == 2)]

original.ids <- 1:nrow(RLdata)

orig.id.split <- split(original.ids, init.blocks)
orig.id.split <- orig.id.split[which(as.numeric(table(init.blocks)) == 2)]

if(is.null(ids)){
  ids <- rep(NA, nrow(RLdata))
  id.split <- split(ids, init.blocks)
  id.split <- id.split[which(as.numeric(table(init.blocks)) > 2)]
} else{
  id.split <- split(ids, init.blocks)
  id.split <- id.split[which(as.numeric(table(init.blocks)) == 2)]
}

block.comparison.lists <- vector("list", length(dsplit))

for(i in seq_along(dsplit)){

  i <- 1
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






# set the default variables.to.match to every column of the RLdata
if(is.null(variables.to.match)){
  variables.to.match <- names(RLdata)
} else{
  variables.to.match <- variables.to.match
}

RLdata <- as.data.frame(dsplit[[1]])

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
# when we use a function that is part of our dependencies we use package::fun()
final.matrix[, comb.cols] <- gtools::combinations(rows, 2)
ids <- as.vector(id.split[[1]])
final.matrix[, cols + 1] <- GetPairwiseMatchesFromIDs(final.matrix[, comb.cols], ids)

combinations.of.original.data <- as.matrix(final.matrix[, comb.cols], ncol=2)
ids.from.original.data <- as.vector(ids.from.original.data)
1 * (ids.from.original.data[combinations.of.original.data[, 1]] ==
       ids.from.original.data[combinations.of.original.data[, 2]])

