
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


library(e1071)
svm.train <- svm(True_Match ~ fname_c1.jar + lname_c1.jar + by.Sta + bm.Sta + bd.Sta,
                   data=training.data)
summary(svm.train)
preds <- predict(svm.train)
round.preds <- 1*(preds > .5)
table(round.preds)

evaluation(training.data$True_Match, round.preds)
# Error in contingency.table["1", "1"] : subscript out of bounds
# Fixed it... like a boss



##################
# Adapted training
##################


training.match <- which(training.data$True_Match == 1)
training.nonmatch <- which(training.data$True_Match != 1)

# We could do *2, *4 etc..... what's the perfect ratio?
training.nonmatch.sample <- sample(training.nonmatch, (length(training.match)*3))

training.data.adapt <- training.data[c(training.match, training.nonmatch.sample),]


svm.train.adapt <- svm(True_Match ~ fname_c1.jar + lname_c1.jar + by.Sta + bm.Sta + bd.Sta,
                       data=training.data.adapt)

summary(svm.train.adapt)
preds <- predict(svm.train.adapt)

round.preds <- 1*(preds > .5)
table(preds)
table(round.preds)
evaluation(training.data.adapt$True_Match, round.preds)



###############
# Testing
###############

test.preds <- predict(svm.train, pred.data1[test, ])
# table(test.preds)
test.preds <- 1*(test.preds > .5)
evaluation(pred.data1$True_Match[test], test.preds)


##################
# Adapted Testing
#################

# We got all of the true/trues which is good because our seed accidently didn't have many matches in it


test.preds.adapt <- predict(svm.train.adapt, pred.data1[test, ])
test.preds.adapt <- 1*(test.preds.adapt > .5)
evaluation(pred.data1$True_Match[test], test.preds.adapt)









