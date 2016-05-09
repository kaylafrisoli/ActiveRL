
################
# PREP WORK ####
################

library(RecordLinkage)
library("devtools")
devtools::load_all()
data("RLdata500")
ids500 <- identity.RLdata500


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


set.seed(986899)
train <- sample(c(TRUE,FALSE), nrow(pred.data1), rep=TRUE, prob = c(.75, .25))
test <- (! train)
training.data <- pred.data1[train,]


library(tree)
tree.train <- tree(True_Match ~ fname_c1.jar + lname_c1.jar + by.Sta + bm.Sta + bd.Sta,
                   data=training.data)

summary(tree.train)
plot(tree.train)
text(tree.train)

preds <- predict(tree.train)

round.preds <- 1*(preds > .5)
table(preds)
table(round.preds)
evaluation(training.data$True_Match, round.preds)

# instead of .5 let's try .9  cutoff
round.preds <- 1*(preds > .9)
table(round.preds)
evaluation(training.data$True_Match, round.preds)



##################
# Adapted training
##################


training.match <- which(training.data$True_Match == 1)
training.nonmatch <- which(training.data$True_Match != 1)

# We could do *2, *4 etc..... what's the perfect ratio?
training.nonmatch.sample <- sample(training.nonmatch, (length(training.match)*4))

training.data.adapt <- training.data[c(training.match, training.nonmatch.sample),]


tree.train.adapt <- tree(True_Match ~ fname_c1.jar + lname_c1.jar + by.Sta + bm.Sta + bd.Sta,
                   data=training.data.adapt)

summary(tree.train.adapt)
plot(tree.train.adapt)
text(tree.train.adapt)

preds <- predict(tree.train.adapt)

round.preds <- 1*(preds > .5)
table(preds)
table(round.preds)
evaluation(training.data.adapt$True_Match, round.preds)





###############
# Testing
###############

test.preds <- predict(tree.train, pred.data1[test, ])
table(test.preds)
test.preds <- 1*(test.preds > .5)
evaluation(pred.data1$True_Match[test], test.preds)


##################
# Adapted Testing
#################


test.preds.adapt <- predict(tree.train.adapt, pred.data1[test, ])
table(test.preds.adapt)
evaluation(pred.data1$True_Match[test], test.preds.adapt)


