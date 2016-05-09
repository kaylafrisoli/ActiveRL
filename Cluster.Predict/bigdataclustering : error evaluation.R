################
# PREP WORK ####
################


#  Function to check if packages are installed
# is.installed <- function(mypkg)  is.element(mypkg, installed.packages()[,1])
#
# #  Example usage
# if (!is.installed("RecordLinkage"))  install.packages("RecordLinkage", dependencies=TRUE, repos="http://cran.r-project.org")
# if (!is.installed("devtools"))  install.packages("devtools", dependencies=TRUE, repos="http://cran.r-project.org")

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

set.seed(1268)
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

#
# some.evals <- c("false.negative.error", "sensitivity", "false.discovery.rate", "precision")
#
# max.n <- floor(length(which(comparison.data.train$True_Match == FALSE)) / length(which(comparison.data.train$True_Match == TRUE)))
#
# n <- seq(1, max.n, by=20)
#
# some.errors <- matrix(NA, nrow=length(n), ncol=length(some.evals) + 1)
# some.errors[, 1] <- n
#
# for(i in seq_along(n)){
#   some.errors[i, 2:ncol(some.errors)] <- PickingN(comparison.data.train,
#                                                   comparison.data.test,
#                                                   n[i],
#                                                   some.evals,
#                                                   True_Match ~ fname_c1.jar + lname_c1.jar + by.Abs + bm.Abs + bd.Abs)
# }
#
#
# error.data <- as.data.frame(some.errors)
# colnames(error.data) <- c("n", some.evals)
#
# # write.csv(error.data)
#
# write.csv(error.data, "/Users/kaylafrisoli/Documents/CMU/Census/Package/BasicRL/errordata.csv")


library(ggplot2)
library(reshape2)

errordata <- read.csv("~/Documents/CMU/Census/Package/BasicRL/errordata.csv")
errordata <- errordata[, -1]

error.data.long <- melt(errordata, id="n")  # convert to long format
ggplot(data=error.data.long,
       aes(x=n, y=value, colour=variable)) +
  geom_line() +
  ggtitle("set.seed = 1268")





#########################################################################

# What should be the ratio of non-matches to matches in our training data - NEW SEED AND RATIO

#########################################################################

########################
# Split in to Train/Test
########################

fullRL <- rbind(RLdata500, RLdata10000)
fullID <- c(identity.RLdata500, identity.RLdata10000)

set.seed(8)
train <- sample(c(TRUE,FALSE), nrow(fullRL), rep=TRUE, prob = c(.4, .6))
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

#
#
# some.evals <- c("false.negative.error", "false.positive.error", "sensitivity", "false.discovery.rate", "precision", "accuracy")
#
# max.n <- floor(length(which(comparison.data.train$True_Match == FALSE)) / length(which(comparison.data.train$True_Match == TRUE)))
#
# n <- seq(1, max.n, by=30)
#
# some.errors <- matrix(NA, nrow=length(n), ncol=length(some.evals) + 1)
# some.errors[, 1] <- n
#
# for(i in seq_along(n)){
#   some.errors[i, 2:ncol(some.errors)] <- PickingN(comparison.data.train,
#                                                   comparison.data.test,
#                                                   n[i],
#                                                   some.evals,
#                                                   True_Match ~ fname_c1.jar + lname_c1.jar + by.Abs + bm.Abs + bd.Abs)
# }
#
#
# error.data2 <- as.data.frame(some.errors)
# colnames(error.data2) <- c("n", some.evals)
#
# # write.csv(error.data)
#
# write.csv(error.data2, "/Users/kaylafrisoli/Documents/CMU/Census/Package/BasicRL/errordata2.csv")


library(ggplot2)
library(reshape2)

errordata2 <- read.csv("~/Documents/CMU/Census/Package/BasicRL/errordata2.csv")
errordata2 <- errordata2[, -1]

error.data.long <- melt(errordata2, id="n")  # convert to long format
ggplot(data=error.data.long,
       aes(x=n, y=value, colour=variable)) +
  geom_line() +
  ggtitle("set.seed = 8")





######################
# Using GLM and HCLUST
######################


AdaptedGLM <- function(formula, comparison.train, n){

  comparison.train <- as.data.frame(comparison.train)
  training.match <- which(comparison.train$True_Match == 1)
  training.nonmatch <- which(comparison.train$True_Match != 1)

  training.nonmatch.sample <- sample(training.nonmatch, (length(training.match)*n))

  training.adapted <- comparison.train[c(training.match, training.nonmatch.sample), ]

  adapted.glm.train <- glm(formula,
                           data = training.adapted,
                           family = binomial)
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


adapted.glm <- glm(True_Match ~ fname_c1.jar + lname_c1.jar + by.Abs + bm.Abs + bd.Abs,
                  comparison.data.train,
                  family=binomial)

n.blocks <- length(test.block.compare$block.data)
glm.hclust.list <- vector("list", n.blocks)
for(i in 1:n.blocks){
  glm.hclust.list[[i]] <- HclustGLM(adapted.glm,
                                    test.block.compare$block.comparison.lists[[i]],
                                    test.block.compare$block.data[[i]],
                                    .5)

}

glm.hclust.list[[1]]
View(test.block.compare$block.data[[1]])

## Now for each block we want to put labels back on

final.predicted.ids <- rep(NA, nrow(testing.data))
for(i in 1:n.blocks){
  final.predicted.ids[test.block.compare$block.data[[i]]$OriginalID] <- paste("block",
                                                                              i,
                                                                              "uid",
                                                                              glm.hclust.list[[i]],
                                                                              sep = "")
}

testing.data$TrueUniqueID <- test.id
testing.data$PredUniqueID <- final.predicted.ids

# I'm kinda confused because we already have everything in pairwise form so it seems we're doing extra work here when we don't need to
library(gtools)
# our.combos <- combinations(nrow(testing.data), 2) ## doesn't work let's try other things


## imma do it another way

n.blocks <- length(test.block.compare$block.data)
glm.hclust.list <- vector("list", n.blocks)
predicted.match.list <- vector("list", n.blocks)
full.test.comparisons <- test.block.compare$full.comparisons
for(i in 1:n.blocks){
  glm.hclust.list[[i]] <- HclustGLM(adapted.glm,
                                    test.block.compare$block.comparison.lists[[i]],
                                    test.block.compare$block.data[[i]],
                                    .5)

  predicted.match.list[[i]] <- GetPairwiseMatchesFromIDs(as.matrix(test.block.compare$block.comparison.lists[[i]][, which(names(test.block.compare$block.comparison.lists[[i]]) %in% c("Record1", "Record2"))]), as.vector(glm.hclust.list[[i]]))

}


Predicted_Match <- as.vector(unlist(predicted.match.list))

full.test.comparisons$Predicted_Match <- Predicted_Match

evaluation(full.test.comparisons$True_Match, full.test.comparisons$Predicted_Match)


