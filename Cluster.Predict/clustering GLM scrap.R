
################
# PREP WORK ####
################

library(RecordLinkage)
library("devtools")
devtools::load_all()
data("RLdata500")
ids500 <- identity.RLdata500


###########
# Functions
###########
#
# standardize.between.01 <- function(x){
#   (x-max(x))/(min(x)-max(x))
# }
#
# absolute.distance <- function(vec1, vec2){
#   vec1 <- as.numeric(vec1)
#   vec2 <- as.numeric(vec2)
#   max.val <- max(c(vec1, vec2))
#   ab.dist <- abs(vec1 - vec2)
#   min.dist <- pmin(ab.dist, max.val-ab.dist)
#   return(min.dist)
# }
#
# standardized.absolute.distance <- function(vec1, vec2){
#   vec1 <- as.numeric(vec1)
#   vec2 <- as.numeric(vec2)
#   max.val <- max(c(vec1, vec2))
#   ab.dist <- abs(vec1 - vec2)
#   min.dist <- pmin(ab.dist, max.val-ab.dist)
#   # Standardize
#   return(standardize.between.01(min.dist))
# }
#
# absolute.difference <- function(vec1, vec2){
#   ab.dif <- abs(as.numeric(vec1)-as.numeric(vec2))
#   return(ab.dif)
# }
#
# standardized.absolute.difference <- function(vec1, vec2){
#   ab.dif <- abs(as.numeric(vec1)-as.numeric(vec2))
#   return(standardize.between.01(ab.dif))
# }
#

###############################################
# A better distance between months, days, years
###############################################


hi <- c(1, 2, 3, 4)
yo <- c(12, 2, 2, 9)

AbsoluteDifference(hi, yo)
AbsoluteDistance(hi, yo)
StandardizedAbsoluteDistance(hi, yo)



############
# Evaluation
############

# evaluation <- function(true.matches, predicted.matches){
#   test.v.pred <- as.data.frame(cbind(true.matches, predicted.matches))
#   colnames(test.v.pred) <- c("truth", "predicted")
#   contingency.table <- table(test.v.pred)
#
#   true.positive <- contingency.table["1", "1"]
#   true.negative <- contingency.table["0", "0"]
#   false.positive <- contingency.table["0", "1"]
#   false.negative <- contingency.table["1", "0"]
#
#   false.positive.error <- false.positive/(false.positive + true.negative)
#   false.negative.error <- false.negative/(false.negative + true.positive)
#   false.discovery.rate <- false.positive/(false.positive + true.positive)
#   sensitivity <- true.positive/ (true.positive + false.negative)
#   specificity <- true.negative/ (true.negative + false.positive)
#   precision <- true.positive/ (true.positive + false.positive)
#   negative.predictive.value <- true.negative/ (true.negative + false.negative)
#   accuracy <- (true.positive + true.negative)/(true.positive + true.negative +
#                                                  false.positive + false.negative)
#
#   return(list(contingency.table = contingency.table,
#               false.positive.error = false.positive.error,
#               false.negative.error = false.negative.error,
#               false.discovery.rate = false.discovery.rate,
#               sensitivity = sensitivity,
#               specificity = specificity,
#               precision = precision,
#               negative.predictive.value = negative.predictive.value,
#               accuracy = accuracy))
#
# }



################################
# Creating our comparison matrix
################################


pred.matrix1 <- CompareUniqueCombinations(RLdata=RLdata500,
                                          ids=ids500,
                                          variables.to.match = c("fname_c1", "lname_c1", "by", "bm", "bd"),
                                          string.comparators = c("jarowinkler",
                                                                 "jarowinkler",
                                                                 "StandardizedAbsoluteDifference",
                                                                 "StandardizedAbsoluteDistance",
                                                                 "StandardizedAbsoluteDistance"))

pred.data1 <- as.data.frame(pred.matrix1)


###############
# Train
###############



set.seed(6899)
train <- sample(c(TRUE,FALSE), nrow(pred.data1), rep=TRUE, prob = c(.75, .25))
test <- (! train)
training.data <- pred.data1[train,]

rl500.glmTRAIN <- glm(True_Match ~ fname_c1.jar + lname_c1.jar + by.Sta + bm.Sta + bd.Sta,
                      data=training.data,
                      family=binomial)
#summary(rl500.glmTRAIN)

preds <- round(predict(rl500.glmTRAIN, type="response"))
evaluation(training.data$True_Match, preds)



##################
# Adapted training
##################

training.match <- which(pred.data1$True_Match[train] == 1)
training.nonmatch <- which(pred.data1$True_Match[train] != 1)

training.nonmatch.sample <- sample(training.nonmatch, (length(training.match)*2))

training.data.adapt <- training.data[c(training.match, training.nonmatch.sample),]


# Training model - check how it does on its training data

glm.train.adapt <- glm(True_Match ~ fname_c1.jar + lname_c1.jar + by.Sta + bm.Sta + bd.Sta,
                      data = training.data.adapt,
                      family = binomial)
summary(glm.train.adapt)

preds <- round(predict(glm.train.adapt, type="response"))

evaluation(training.data.adapt$True_Match, preds)



###############
# Testing
###############

test.preds <- round(predict(rl500.glmTRAIN, pred.data1[test, ], type="response"))
evaluation(pred.data1$True_Match[test], test.preds)

##################
# Adapted Testing
#################


test.preds.adapt <- round(predict(glm.train.adapt, pred.data1[test, ], type="response"))
evaluation(pred.data1$True_Match[test], test.preds.adapt)

# False positive rate is super high but false nevative rate is 0


##########
# Total
##########

table(pred.data1$True_Match)




##############################
# Trying some other partitions
##############################


###############
# Train
###############



set.seed(8324908)
train <- sample(c(TRUE,FALSE), nrow(pred.data1), rep=TRUE, prob = c(.8, .2))
test <- (! train)
training.data <- pred.data1[train,]

rl500.glmTRAIN <- glm(True_Match ~ fname_c1.jar + lname_c1.jar + by.Sta + bm.Sta + bd.Sta,
                      data=training.data,
                      family=binomial)
#summary(rl500.glmTRAIN)

preds <- round(predict(rl500.glmTRAIN, type="response"))
evaluation(training.data$True_Match, preds)



##################
# Adapted training
##################

training.match <- which(pred.data1$True_Match[train] == 1)
training.nonmatch <- which(pred.data1$True_Match[train] != 1)

training.nonmatch.sample <- sample(training.nonmatch, (length(training.match)*4))

training.data.adapt <- training.data[c(training.match, training.nonmatch.sample),]


# Training model - check how it does on its training data

glm.train.adapt <- glm(True_Match ~ fname_c1.jar + lname_c1.jar + by.Sta + bm.Sta + bd.Sta,
                       data = training.data.adapt,
                       family = binomial)
summary(glm.train.adapt)

preds <- round(predict(glm.train.adapt, type="response"))

evaluation(training.data.adapt$True_Match, preds)



###############
# Testing
###############

test.preds <- round(predict(rl500.glmTRAIN, pred.data1[test, ], type="response"))
evaluation(pred.data1$True_Match[test], test.preds)

##################
# Adapted Testing
#################


test.preds.adapt <- round(predict(glm.train.adapt, pred.data1[test, ], type="response"))
evaluation(pred.data1$True_Match[test], test.preds.adapt)

# False positive rate is super high but false negative rate is 0





#############################
# Calculating Distance Matrix
#############################


## In our situation we shouldn't be using the dist function
## because we already calculated the distance by our comparisons

average.dist <- apply(pred.matrix1[, 1:5], 1, mean)
average.dist.mat <- matrix(NA, nrow = nrow(RLdata500), ncol=nrow(RLdata500))
average.dist.mat[lower.tri(average.dist.mat)] <- average.dist

average.dist <- as.dist(average.dist.mat, diag=FALSE, upper=FALSE)

hclust.avg.dist <- hclust(average.dist)


# Realistically

d <- cut(as.dendrogram(hclust.avg.dist), h=.2) # 443 brances

par(mfrow=c(3, 2))
plot(d$lower[[1]])
plot(d$lower[[2]])
plot(d$lower[[3]])
plot(d$lower[[4]])
plot(d$lower[[5]])
plot(d$lower[[6]])
par(mfrow=c(1, 1))

# Better for visualiztion

d <- cut(as.dendrogram(hclust.avg.dist), h=.8) # 443 brances

par(mfrow=c(3, 2))
plot(d$lower[[1]])
plot(d$lower[[2]])
plot(d$lower[[3]])
plot(d$lower[[4]])
plot(d$lower[[5]])
plot(d$lower[[6]])
par(mfrow=c(1, 1))

