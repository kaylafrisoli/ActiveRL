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

set.seed(8684)
train <- sample(c(TRUE,FALSE), nrow(fullRL), rep=TRUE, prob = c(.5, .5))
test <- (! train)
training.data <- fullRL[train, ]
testing.data  <- fullRL[test, ]
train.id <- fullID[train]
test.id <- fullID[test]

################################
# Creating our comparison matrix
################################


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
                                                                 "StandardizedAbsoluteDifferenceY",
                                                                 "StandardizedAbsoluteDistanceM",
                                                                 "StandardizedAbsoluteDistanceD"))

#View(train.block.compare$block.comparison.lists[[1]])
#View(train.block.compare$block.data[[1]])
#train.block.compare$orig.id.split[[1]]

comparison.data.train <- train.block.compare$full.comparisons


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
                                                                          "StandardizedAbsoluteDifferenceY",
                                                                          "StandardizedAbsoluteDistanceM",
                                                                          "StandardizedAbsoluteDistanceD"))
comparison.data.test <- test.block.compare$full.comparisons

glm.train <- glm(True_Match ~ fname_c1.jar + lname_c1.jar + by.Sta + bm.Sta + bd.Sta,
                 data = comparison.data.train,
                 family = binomial)

test.preds <- round(predict(glm.train, comparison.data.test, type="response"))

evaluation(comparison.data.test$True_Match, test.preds)


PickingN(comparison.data.train, comparison.data.test, 2, c("false.negative.error", "false.positive.error"))


#
# some.evals <- c("false.negative.error", "sensitivity", "false.discovery.rate")
# n <- 1:150
# some.errors <- matrix(NA, nrow=150, ncol=length(some.evals) + 1)
# some.errors[, 1] <- n
#
# for(i in n){
#   some.errors[i, 2:4] <- PickingN(comparison.data.train, comparison.data.test, i, some.evals )
#   print(i)
# }
#
# plot(n, false.neg.errors)
#
# error.data <- as.data.frame(some.errors)
# colnames(error.data) <- c("n", some.evals)

#write.csv(error.data, "/Users/kaylafrisoli/Documents/CMU/Census/Package/BasicRL/errordata.csv")

library(ggplot2)
library(reshape2)

errordata <- read.csv("~/Documents/CMU/Census/Package/BasicRL/errordata.csv")
errordata <- errordata[, -1]

error.data.long <- melt(errordata, id="n")  # convert to long format
ggplot(data=error.data.long,
       aes(x=n, y=value, colour=variable)) +
  geom_line()



# some.evals <- c("false.negative.error", "sensitivity", "false.discovery.rate")
# n <- seq(1, 10, by=.2)
# some.errors <- matrix(NA, nrow=length(n), ncol=length(some.evals) + 1)
# some.errors[, 1] <- n
#
# for(i in 1:length(n)){
#   some.errors[i, 2:4] <- PickingN(comparison.data.train, comparison.data.test, n[i], some.evals )
#   print(i)
# }

#
#
# error.data2 <- as.data.frame(some.errors)
# colnames(error.data2) <- c("n", some.evals)

#write.csv(error.data2, "/Users/kaylafrisoli/Documents/CMU/Census/Package/BasicRL/errordata2.csv")

errordata2 <- read.csv("~/Documents/CMU/Census/Package/BasicRL/errordata2.csv")
errordata2 <- errordata2[, -1]

error.data.long <- melt(errordata2, id="n")  # convert to long format
ggplot(data=error.data.long,
       aes(x=n, y=value, colour=variable)) +
  geom_line()



####################
# Comparing methods
####################

#
# average.dist <- apply(comparison.data.test[, 1:5], 1, mean)
# average.dist.mat <- matrix(NA, nrow = nrow(testing.data), ncol=nrow(testing.data))
# average.dist.mat[lower.tri(average.dist.mat)] <- average.dist
# sqrt(length(average.dist))
# average.dist <- as.dist(average.dist.mat, diag=FALSE, upper=FALSE)
#
# hclust.avg.dist <- hclust(average.dist)


# test.block.compare$block.comparison.lists



############
# Average
############

HclustAvg <- function(fullRL, comparisons, columns.we.want){
  average.dist <- apply(comparisons[, columns.we.want], 1, mean, na.rm=TRUE)
  average.dist.mat <- matrix(NA, nrow = nrow(fullRL), ncol=nrow(fullRL))
  average.dist.mat[lower.tri(average.dist.mat)] <- average.dist
  average.distance <- as.dist(average.dist.mat, diag=FALSE, upper=FALSE)
  hclust.avg.dist <- hclust(average.distance)
  return(hclust.avg.dist)
}

n.blocks <- length(test.block.compare$block.data)
avg.hclust.list <- vector("list", n.blocks)
for(i in 1:n.blocks){
  avg.hclust.list[[i]] <- HclustAvg(test.block.compare$block.data[[i]], test.block.compare$block.comparison.lists[[i]], 1:5)

}

avg.hclust.list[[23]]


##############
# Using GLM
##############

HclustAdaptedGLM <- function(comparison.train, comparison.test,  n, fullTestRL){

  comparison.train <- as.data.frame(comparison.train)
  training.match <- which(comparison.train$True_Match == 1)
  training.nonmatch <- which(comparison.train$True_Match != 1)

  training.nonmatch.sample <- sample(training.nonmatch, (length(training.match)*n))

  training.adapted <- comparison.train[c(training.match, training.nonmatch.sample), ]

  glm.train <- glm(True_Match ~ fname_c1.jar + lname_c1.jar + by.Sta + bm.Sta + bd.Sta,
                   data = training.adapted,
                   family = binomial)

  test.preds <- predict(glm.train, comparison.test, type="response")

  average.dist.mat <- matrix(NA, nrow = nrow(fullTestRL), ncol=nrow(fullTestRL))
  average.dist.mat[lower.tri(average.dist.mat)] <- test.preds
  average.distance <- as.dist(average.dist.mat, diag=FALSE, upper=FALSE)
  hclust.glm <- hclust(average.distance)
  return(hclust.glm)
}


n.blocks <- length(test.block.compare$block.data)
glm.hclust.list <- vector("list", n.blocks)
for(i in 1:n.blocks){
  glm.hclust.list[[i]] <- HclustAdaptedGLM(train.block.compare$full.comparisons,
                                           test.block.compare$block.comparison.lists[[i]],
                                           3,
                                           test.block.compare$block.data[[i]])

}




glm.hclust.list[[23]]
