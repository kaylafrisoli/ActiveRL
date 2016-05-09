################
# PREP WORK ####
################

library(RecordLinkage)
library("devtools")
devtools::load_all()
data("RLdata500")
data("RLdata10000")




########################
# Split in to Train/Test
########################

fullRL <- rbind(RLdata500, RLdata10000)
fullID <- c(identity.RLdata500, identity.RLdata10000)

set.seed(2468)
train <- sample(c(TRUE,FALSE), nrow(fullRL), rep=TRUE, prob = c(.2, .8))
test <- (! train)
training.data <- fullRL[train, ]
testing.data  <- fullRL[test, ]
train.id <- fullID[train]
test.id <- fullID[test]


########################################
# Creating our comparison matrix - TRAIN
########################################


train.block.compare <- BlockAndCompareCombinations(RLdata=training.data,
                                                   var.names = c("fname_c1"),
                                                   n.chars = c(1),
                                                   ids=train.id,
                                                   variables.to.match = c("fname_c1",
                                                                          "lname_c1",
                                                                          "by",
                                                                          "bm",
                                                                          "bd"),
                                                   string.comparators = c("jarowinkler",
                                                                          "jarowinkler",
                                                                          "AbsoluteDifference",
                                                                          "AbsoluteDistance",
                                                                          "AbsoluteDistance"))


comparison.data.train <- train.block.compare$full.comparisons


########################################
# Creating our comparison matrix - TEST
########################################

test.block.compare <- BlockAndCompareCombinations(RLdata=testing.data,
                                                  var.names = c("fname_c1"),
                                                  n.chars = c(1),
                                                  ids=test.id,
                                                  variables.to.match = c("fname_c1",
                                                                         "lname_c1",
                                                                         "by",
                                                                         "bm",
                                                                         "bd"),
                                                  string.comparators = c("jarowinkler",
                                                                         "jarowinkler",
                                                                         "AbsoluteDifference",
                                                                         "AbsoluteDistance",
                                                                         "AbsoluteDistance"))
comparison.data.test <- test.block.compare$full.comparisons


#########################################################################

# What should be the ratio of non-matches to matches in our training data

#########################################################################


some.evals <- c("false.negative.error", "sensitivity", "false.discovery.rate", "precision")

max.n <- floor(length(which(comparison.data.train$True_Match == FALSE)) / length(which(comparison.data.train$True_Match == TRUE)))

n <- seq(1, max.n, by=80)

some.errors <- matrix(NA, nrow=length(n), ncol=length(some.evals) + 1)
some.errors[, 1] <- n

for(i in seq_along(n)){
  some.errors[i, 2:ncol(some.errors)] <- PickingN(comparison.data.train,
                                                  comparison.data.test,
                                                  n[i],
                                                  some.evals,
                                                  True_Match ~ fname_c1.jar + lname_c1.jar + by.Abs + bm.Abs + bd.Abs)
}


error.data <- as.data.frame(some.errors)
colnames(error.data) <- c("n", some.evals)

# write.csv(error.data)

write.csv(error.data, "/Users/kaylafrisoli/Documents/CMU/Census/Package/BasicRL/evaluationgraphs/f1data2080.csv")


########################
# Split in to Train/Test
########################

fullRL <- rbind(RLdata500, RLdata10000)
fullID <- c(identity.RLdata500, identity.RLdata10000)

set.seed(2468)
train <- sample(c(TRUE,FALSE), nrow(fullRL), rep=TRUE, prob = c(.5, .5))
test <- (! train)
training.data <- fullRL[train, ]
testing.data  <- fullRL[test, ]
train.id <- fullID[train]
test.id <- fullID[test]


########################################
# Creating our comparison matrix - TRAIN
########################################


train.block.compare <- BlockAndCompareCombinations(RLdata=training.data,
                                                   var.names = c("fname_c1"),
                                                   n.chars = c(1),
                                                   ids=train.id,
                                                   variables.to.match = c("fname_c1",
                                                                          "lname_c1",
                                                                          "by",
                                                                          "bm",
                                                                          "bd"),
                                                   string.comparators = c("jarowinkler",
                                                                          "jarowinkler",
                                                                          "AbsoluteDifference",
                                                                          "AbsoluteDistance",
                                                                          "AbsoluteDistance"))


comparison.data.train <- train.block.compare$full.comparisons


########################################
# Creating our comparison matrix - TEST
########################################

test.block.compare <- BlockAndCompareCombinations(RLdata=testing.data,
                                                  var.names = c("fname_c1"),
                                                  n.chars = c(1),
                                                  ids=test.id,
                                                  variables.to.match = c("fname_c1",
                                                                         "lname_c1",
                                                                         "by",
                                                                         "bm",
                                                                         "bd"),
                                                  string.comparators = c("jarowinkler",
                                                                         "jarowinkler",
                                                                         "AbsoluteDifference",
                                                                         "AbsoluteDistance",
                                                                         "AbsoluteDistance"))
comparison.data.test <- test.block.compare$full.comparisons


#########################################################################

# What should be the ratio of non-matches to matches in our training data

#########################################################################


some.evals <- c("false.negative.error", "sensitivity", "false.discovery.rate", "precision")

max.n <- floor(length(which(comparison.data.train$True_Match == FALSE)) / length(which(comparison.data.train$True_Match == TRUE)))

n <- seq(1, max.n, by=80)

some.errors <- matrix(NA, nrow=length(n), ncol=length(some.evals) + 1)
some.errors[, 1] <- n

for(i in seq_along(n)){
  some.errors[i, 2:ncol(some.errors)] <- PickingN(comparison.data.train,
                                                  comparison.data.test,
                                                  n[i],
                                                  some.evals,
                                                  True_Match ~ fname_c1.jar + lname_c1.jar + by.Abs + bm.Abs + bd.Abs)
}


error.data <- as.data.frame(some.errors)
colnames(error.data) <- c("n", some.evals)

# write.csv(error.data)

write.csv(error.data, "/Users/kaylafrisoli/Documents/CMU/Census/Package/BasicRL/evaluationgraphs/f1data5050.csv")





########################
# Split in to Train/Test
########################

fullRL <- rbind(RLdata500, RLdata10000)
fullID <- c(identity.RLdata500, identity.RLdata10000)

set.seed(2468)
train <- sample(c(TRUE,FALSE), nrow(fullRL), rep=TRUE, prob = c(.8, .2))
test <- (! train)
training.data <- fullRL[train, ]
testing.data  <- fullRL[test, ]
train.id <- fullID[train]
test.id <- fullID[test]


########################################
# Creating our comparison matrix - TRAIN
########################################


train.block.compare <- BlockAndCompareCombinations(RLdata=training.data,
                                                   var.names = c("fname_c1"),
                                                   n.chars = c(1),
                                                   ids=train.id,
                                                   variables.to.match = c("fname_c1",
                                                                          "lname_c1",
                                                                          "by",
                                                                          "bm",
                                                                          "bd"),
                                                   string.comparators = c("jarowinkler",
                                                                          "jarowinkler",
                                                                          "AbsoluteDifference",
                                                                          "AbsoluteDistance",
                                                                          "AbsoluteDistance"))


comparison.data.train <- train.block.compare$full.comparisons


########################################
# Creating our comparison matrix - TEST
########################################

test.block.compare <- BlockAndCompareCombinations(RLdata=testing.data,
                                                  var.names = c("fname_c1"),
                                                  n.chars = c(1),
                                                  ids=test.id,
                                                  variables.to.match = c("fname_c1",
                                                                         "lname_c1",
                                                                         "by",
                                                                         "bm",
                                                                         "bd"),
                                                  string.comparators = c("jarowinkler",
                                                                         "jarowinkler",
                                                                         "AbsoluteDifference",
                                                                         "AbsoluteDistance",
                                                                         "AbsoluteDistance"))
comparison.data.test <- test.block.compare$full.comparisons


#########################################################################

# What should be the ratio of non-matches to matches in our training data

#########################################################################


some.evals <- c("false.negative.error", "sensitivity", "false.discovery.rate", "precision")

max.n <- floor(length(which(comparison.data.train$True_Match == FALSE)) / length(which(comparison.data.train$True_Match == TRUE)))

n <- seq(1, max.n, by=80)

some.errors <- matrix(NA, nrow=length(n), ncol=length(some.evals) + 1)
some.errors[, 1] <- n

for(i in seq_along(n)){
  some.errors[i, 2:ncol(some.errors)] <- PickingN(comparison.data.train,
                                                  comparison.data.test,
                                                  n[i],
                                                  some.evals,
                                                  True_Match ~ fname_c1.jar + lname_c1.jar + by.Abs + bm.Abs + bd.Abs)
}


error.data <- as.data.frame(some.errors)
colnames(error.data) <- c("n", some.evals)

# write.csv(error.data)

write.csv(error.data, "/Users/kaylafrisoli/Documents/CMU/Census/Package/BasicRL/evaluationgraphs/f1data8020.csv")





########################
# Split in to Train/Test
########################

fullRL <- rbind(RLdata500, RLdata10000)
fullID <- c(identity.RLdata500, identity.RLdata10000)

set.seed(9)
train <- sample(c(TRUE,FALSE), nrow(fullRL), rep=TRUE, prob = c(.2, .8))
test <- (! train)
training.data <- fullRL[train, ]
testing.data  <- fullRL[test, ]
train.id <- fullID[train]
test.id <- fullID[test]


########################################
# Creating our comparison matrix - TRAIN
########################################


train.block.compare <- BlockAndCompareCombinations(RLdata=training.data,
                                                   var.names = c("fname_c1"),
                                                   n.chars = c(1),
                                                   ids=train.id,
                                                   variables.to.match = c("fname_c1",
                                                                          "lname_c1",
                                                                          "by",
                                                                          "bm",
                                                                          "bd"),
                                                   string.comparators = c("jarowinkler",
                                                                          "jarowinkler",
                                                                          "AbsoluteDifference",
                                                                          "AbsoluteDistance",
                                                                          "AbsoluteDistance"))


comparison.data.train <- train.block.compare$full.comparisons


########################################
# Creating our comparison matrix - TEST
########################################

test.block.compare <- BlockAndCompareCombinations(RLdata=testing.data,
                                                  var.names = c("fname_c1"),
                                                  n.chars = c(1),
                                                  ids=test.id,
                                                  variables.to.match = c("fname_c1",
                                                                         "lname_c1",
                                                                         "by",
                                                                         "bm",
                                                                         "bd"),
                                                  string.comparators = c("jarowinkler",
                                                                         "jarowinkler",
                                                                         "AbsoluteDifference",
                                                                         "AbsoluteDistance",
                                                                         "AbsoluteDistance"))
comparison.data.test <- test.block.compare$full.comparisons


#########################################################################

# What should be the ratio of non-matches to matches in our training data

#########################################################################


some.evals <- c("false.negative.error", "sensitivity", "false.discovery.rate", "precision")

max.n <- floor(length(which(comparison.data.train$True_Match == FALSE)) / length(which(comparison.data.train$True_Match == TRUE)))

n <- seq(1, max.n, by=80)

some.errors <- matrix(NA, nrow=length(n), ncol=length(some.evals) + 1)
some.errors[, 1] <- n

for(i in seq_along(n)){
  some.errors[i, 2:ncol(some.errors)] <- PickingN(comparison.data.train,
                                                  comparison.data.test,
                                                  n[i],
                                                  some.evals,
                                                  True_Match ~ fname_c1.jar + lname_c1.jar + by.Abs + bm.Abs + bd.Abs)
}


error.data <- as.data.frame(some.errors)
colnames(error.data) <- c("n", some.evals)

# write.csv(error.data)

write.csv(error.data, "/Users/kaylafrisoli/Documents/CMU/Census/Package/BasicRL/evaluationgraphs/f1data2080NewSeed.csv")




########################
# Split in to Train/Test
########################

fullRL <- rbind(RLdata500, RLdata10000)
fullID <- c(identity.RLdata500, identity.RLdata10000)

set.seed(9)
train <- sample(c(TRUE,FALSE), nrow(fullRL), rep=TRUE, prob = c(.5, .5))
test <- (! train)
training.data <- fullRL[train, ]
testing.data  <- fullRL[test, ]
train.id <- fullID[train]
test.id <- fullID[test]


########################################
# Creating our comparison matrix - TRAIN
########################################


train.block.compare <- BlockAndCompareCombinations(RLdata=training.data,
                                                   var.names = c("fname_c1"),
                                                   n.chars = c(1),
                                                   ids=train.id,
                                                   variables.to.match = c("fname_c1",
                                                                          "lname_c1",
                                                                          "by",
                                                                          "bm",
                                                                          "bd"),
                                                   string.comparators = c("jarowinkler",
                                                                          "jarowinkler",
                                                                          "AbsoluteDifference",
                                                                          "AbsoluteDistance",
                                                                          "AbsoluteDistance"))


comparison.data.train <- train.block.compare$full.comparisons


########################################
# Creating our comparison matrix - TEST
########################################

test.block.compare <- BlockAndCompareCombinations(RLdata=testing.data,
                                                  var.names = c("fname_c1"),
                                                  n.chars = c(1),
                                                  ids=test.id,
                                                  variables.to.match = c("fname_c1",
                                                                         "lname_c1",
                                                                         "by",
                                                                         "bm",
                                                                         "bd"),
                                                  string.comparators = c("jarowinkler",
                                                                         "jarowinkler",
                                                                         "AbsoluteDifference",
                                                                         "AbsoluteDistance",
                                                                         "AbsoluteDistance"))
comparison.data.test <- test.block.compare$full.comparisons


#########################################################################

# What should be the ratio of non-matches to matches in our training data

#########################################################################


some.evals <- c("false.negative.error", "sensitivity", "false.discovery.rate", "precision")

max.n <- floor(length(which(comparison.data.train$True_Match == FALSE)) / length(which(comparison.data.train$True_Match == TRUE)))

n <- seq(1, max.n, by=80)

some.errors <- matrix(NA, nrow=length(n), ncol=length(some.evals) + 1)
some.errors[, 1] <- n

for(i in seq_along(n)){
  some.errors[i, 2:ncol(some.errors)] <- PickingN(comparison.data.train,
                                                  comparison.data.test,
                                                  n[i],
                                                  some.evals,
                                                  True_Match ~ fname_c1.jar + lname_c1.jar + by.Abs + bm.Abs + bd.Abs)
}


error.data <- as.data.frame(some.errors)
colnames(error.data) <- c("n", some.evals)

# write.csv(error.data)

write.csv(error.data, "/Users/kaylafrisoli/Documents/CMU/Census/Package/BasicRL/evaluationgraphs/f1data5050NewSeed.csv")







########################
# Split in to Train/Test
########################

fullRL <- rbind(RLdata500, RLdata10000)
fullID <- c(identity.RLdata500, identity.RLdata10000)

set.seed(9)
train <- sample(c(TRUE,FALSE), nrow(fullRL), rep=TRUE, prob = c(.8, .2))
test <- (! train)
training.data <- fullRL[train, ]
testing.data  <- fullRL[test, ]
train.id <- fullID[train]
test.id <- fullID[test]


########################################
# Creating our comparison matrix - TRAIN
########################################


train.block.compare <- BlockAndCompareCombinations(RLdata=training.data,
                                                   var.names = c("fname_c1"),
                                                   n.chars = c(1),
                                                   ids=train.id,
                                                   variables.to.match = c("fname_c1",
                                                                          "lname_c1",
                                                                          "by",
                                                                          "bm",
                                                                          "bd"),
                                                   string.comparators = c("jarowinkler",
                                                                          "jarowinkler",
                                                                          "AbsoluteDifference",
                                                                          "AbsoluteDistance",
                                                                          "AbsoluteDistance"))


comparison.data.train <- train.block.compare$full.comparisons


########################################
# Creating our comparison matrix - TEST
########################################

test.block.compare <- BlockAndCompareCombinations(RLdata=testing.data,
                                                  var.names = c("fname_c1"),
                                                  n.chars = c(1),
                                                  ids=test.id,
                                                  variables.to.match = c("fname_c1",
                                                                         "lname_c1",
                                                                         "by",
                                                                         "bm",
                                                                         "bd"),
                                                  string.comparators = c("jarowinkler",
                                                                         "jarowinkler",
                                                                         "AbsoluteDifference",
                                                                         "AbsoluteDistance",
                                                                         "AbsoluteDistance"))
comparison.data.test <- test.block.compare$full.comparisons


#########################################################################

# What should be the ratio of non-matches to matches in our training data

#########################################################################


some.evals <- c("false.negative.error", "sensitivity", "false.discovery.rate", "precision")

max.n <- floor(length(which(comparison.data.train$True_Match == FALSE)) / length(which(comparison.data.train$True_Match == TRUE)))

n <- seq(1, max.n, by=80)

some.errors <- matrix(NA, nrow=length(n), ncol=length(some.evals) + 1)
some.errors[, 1] <- n

for(i in seq_along(n)){
  some.errors[i, 2:ncol(some.errors)] <- PickingN(comparison.data.train,
                                                  comparison.data.test,
                                                  n[i],
                                                  some.evals,
                                                  True_Match ~ fname_c1.jar + lname_c1.jar + by.Abs + bm.Abs + bd.Abs)
}


error.data <- as.data.frame(some.errors)
colnames(error.data) <- c("n", some.evals)

# write.csv(error.data)

write.csv(error.data, "/Users/kaylafrisoli/Documents/CMU/Census/Package/BasicRL/evaluationgraphs/f1data8020NewSeed.csv")









########################
# Split in to Train/Test
########################

fullRL <- rbind(RLdata500, RLdata10000)
fullID <- c(identity.RLdata500, identity.RLdata10000)

set.seed(2468)
train <- sample(c(TRUE,FALSE), nrow(fullRL), rep=TRUE, prob = c(.1, .9))
test <- (! train)
training.data <- fullRL[train, ]
testing.data  <- fullRL[test, ]
train.id <- fullID[train]
test.id <- fullID[test]


########################################
# Creating our comparison matrix - TRAIN
########################################


train.block.compare <- BlockAndCompareCombinations(RLdata=training.data,
                                                   var.names = c("bm"),
                                                   n.chars = 2,
                                                   ids=train.id,
                                                   variables.to.match = c("fname_c1",
                                                                          "lname_c1",
                                                                          "by",
                                                                          "bm",
                                                                          "bd"),
                                                   string.comparators = c("jarowinkler",
                                                                          "jarowinkler",
                                                                          "AbsoluteDifference",
                                                                          "AbsoluteDistance",
                                                                          "AbsoluteDistance"))


comparison.data.train <- train.block.compare$full.comparisons


########################################
# Creating our comparison matrix - TEST
########################################

test.block.compare <- BlockAndCompareCombinations(RLdata=testing.data,
                                                  var.names = c("bm"),
                                                  n.chars = 2,
                                                  ids=test.id,
                                                  variables.to.match = c("fname_c1",
                                                                         "lname_c1",
                                                                         "by",
                                                                         "bm",
                                                                         "bd"),
                                                  string.comparators = c("jarowinkler",
                                                                         "jarowinkler",
                                                                         "AbsoluteDifference",
                                                                         "AbsoluteDistance",
                                                                         "AbsoluteDistance"))
comparison.data.test <- test.block.compare$full.comparisons


#########################################################################

# What should be the ratio of non-matches to matches in our training data

#########################################################################


some.evals <- c("false.negative.error", "sensitivity", "false.discovery.rate", "precision")

max.n <- floor(length(which(comparison.data.train$True_Match == FALSE)) / length(which(comparison.data.train$True_Match == TRUE)))

n <- seq(1, max.n, by=120)

some.errors <- matrix(NA, nrow=length(n), ncol=length(some.evals) + 1)
some.errors[, 1] <- n

for(i in seq_along(n)){
  some.errors[i, 2:ncol(some.errors)] <- PickingN(comparison.data.train,
                                                  comparison.data.test,
                                                  n[i],
                                                  some.evals,
                                                  True_Match ~ fname_c1.jar + lname_c1.jar + by.Abs + bm.Abs + bd.Abs)
}


error.data <- as.data.frame(some.errors)
colnames(error.data) <- c("n", some.evals)

# write.csv(error.data)

write.csv(error.data, "/Users/kaylafrisoli/Documents/CMU/Census/Package/BasicRL/evaluationgraphs/bm2data1090.csv")




########################
# Split in to Train/Test
########################


fullRL <- rbind(RLdata500, RLdata10000)
fullID <- c(identity.RLdata500, identity.RLdata10000)

set.seed(2468)
train <- sample(c(TRUE,FALSE), nrow(fullRL), rep=TRUE, prob = c(.5, .5))
test <- (! train)
training.data <- fullRL[train, ]
testing.data  <- fullRL[test, ]
train.id <- fullID[train]
test.id <- fullID[test]


########################################
# Creating our comparison matrix - TRAIN
########################################


train.block.compare <- BlockAndCompareCombinations(RLdata=training.data,
                                                   var.names = c("bm"),
                                                   n.chars = 2,
                                                   ids=train.id,
                                                   variables.to.match = c("fname_c1",
                                                                          "lname_c1",
                                                                          "by",
                                                                          "bm",
                                                                          "bd"),
                                                   string.comparators = c("jarowinkler",
                                                                          "jarowinkler",
                                                                          "AbsoluteDifference",
                                                                          "AbsoluteDistance",
                                                                          "AbsoluteDistance"))


comparison.data.train <- train.block.compare$full.comparisons


########################################
# Creating our comparison matrix - TEST
########################################

test.block.compare <- BlockAndCompareCombinations(RLdata=testing.data,
                                                  var.names = c("bm"),
                                                  n.chars = 2,
                                                  ids=test.id,
                                                  variables.to.match = c("fname_c1",
                                                                         "lname_c1",
                                                                         "by",
                                                                         "bm",
                                                                         "bd"),
                                                  string.comparators = c("jarowinkler",
                                                                         "jarowinkler",
                                                                         "AbsoluteDifference",
                                                                         "AbsoluteDistance",
                                                                         "AbsoluteDistance"))
comparison.data.test <- test.block.compare$full.comparisons


#########################################################################

# What should be the ratio of non-matches to matches in our training data

#########################################################################


some.evals <- c("false.negative.error", "sensitivity", "false.discovery.rate", "precision")

max.n <- floor(length(which(comparison.data.train$True_Match == FALSE)) / length(which(comparison.data.train$True_Match == TRUE)))

n <- seq(1, max.n, by=120)

some.errors <- matrix(NA, nrow=length(n), ncol=length(some.evals) + 1)
some.errors[, 1] <- n

for(i in seq_along(n)){
  some.errors[i, 2:ncol(some.errors)] <- PickingN(comparison.data.train,
                                                  comparison.data.test,
                                                  n[i],
                                                  some.evals,
                                                  True_Match ~ fname_c1.jar + lname_c1.jar + by.Abs + bm.Abs + bd.Abs)
}


error.data <- as.data.frame(some.errors)
colnames(error.data) <- c("n", some.evals)

# write.csv(error.data)

write.csv(error.data, "/Users/kaylafrisoli/Documents/CMU/Census/Package/BasicRL/evaluationgraphs/bm2data5050.csv")






########################
# Split in to Train/Test
########################


fullRL <- rbind(RLdata500, RLdata10000)
fullID <- c(identity.RLdata500, identity.RLdata10000)

set.seed(2468)
train <- sample(c(TRUE,FALSE), nrow(fullRL), rep=TRUE, prob = c(.9, .1))
test <- (! train)
training.data <- fullRL[train, ]
testing.data  <- fullRL[test, ]
train.id <- fullID[train]
test.id <- fullID[test]


########################################
# Creating our comparison matrix - TRAIN
########################################


train.block.compare <- BlockAndCompareCombinations(RLdata=training.data,
                                                   var.names = c("bm"),
                                                   n.chars = 2,
                                                   ids=train.id,
                                                   variables.to.match = c("fname_c1",
                                                                          "lname_c1",
                                                                          "by",
                                                                          "bm",
                                                                          "bd"),
                                                   string.comparators = c("jarowinkler",
                                                                          "jarowinkler",
                                                                          "AbsoluteDifference",
                                                                          "AbsoluteDistance",
                                                                          "AbsoluteDistance"))


comparison.data.train <- train.block.compare$full.comparisons


########################################
# Creating our comparison matrix - TEST
########################################

test.block.compare <- BlockAndCompareCombinations(RLdata=testing.data,
                                                  var.names = c("bm"),
                                                  n.chars = 2,
                                                  ids=test.id,
                                                  variables.to.match = c("fname_c1",
                                                                         "lname_c1",
                                                                         "by",
                                                                         "bm",
                                                                         "bd"),
                                                  string.comparators = c("jarowinkler",
                                                                         "jarowinkler",
                                                                         "AbsoluteDifference",
                                                                         "AbsoluteDistance",
                                                                         "AbsoluteDistance"))
comparison.data.test <- test.block.compare$full.comparisons


#########################################################################

# What should be the ratio of non-matches to matches in our training data

#########################################################################


some.evals <- c("false.negative.error", "sensitivity", "false.discovery.rate", "precision")

max.n <- floor(length(which(comparison.data.train$True_Match == FALSE)) / length(which(comparison.data.train$True_Match == TRUE)))

n <- seq(1, max.n, by=120)

some.errors <- matrix(NA, nrow=length(n), ncol=length(some.evals) + 1)
some.errors[, 1] <- n

for(i in seq_along(n)){
  some.errors[i, 2:ncol(some.errors)] <- PickingN(comparison.data.train,
                                                  comparison.data.test,
                                                  n[i],
                                                  some.evals,
                                                  True_Match ~ fname_c1.jar + lname_c1.jar + by.Abs + bm.Abs + bd.Abs)
}


error.data <- as.data.frame(some.errors)
colnames(error.data) <- c("n", some.evals)

# write.csv(error.data)

write.csv(error.data, "/Users/kaylafrisoli/Documents/CMU/Census/Package/BasicRL/evaluationgraphs/bm2data9010.csv")


