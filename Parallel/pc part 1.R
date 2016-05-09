

library(RecordLinkage)
library("devtools")
devtools::load_all()
data("RLdata500")
data("RLdata10000")

fullRL <- rbind(RLdata500, RLdata10000)
fullID <- c(identity.RLdata500, identity.RLdata10000)
splitRL <- SplitIntoTrainTest(fullRL, fullID, seed=16, prob.of.train = .5)


blockTrain <- BlockRlData(splitRL$training.data,
                          var.names = "bm",
                          n.chars = 2,
                          splitRL$train.id)


blockTest <- BlockRlData(splitRL$testing.data,
                         var.names = "bm",
                         n.chars = 2,
                         splitRL$test.id)



time.start=proc.time()[[3]]

block.compare.test <- CompareAllBlocksInLoop(blockTest$DataSplit,
                                               blockTest$IdSplit,
                                               variables.to.match = c("fname_c1",
                                                                      "lname_c1",
                                                                      "by",
                                                                      "bm",
                                                                      "bd"),
                                               string.comparators = c("jarowinkler",
                                                                      "jarowinkler",
                                                                      "AbsoluteDifference",
                                                                      "AbsoluteDistance",
                                                                      "AbsoluteDistance"),
                                               record.ids.to.keep=c("PreSplitRecord", "PreBlockRecord"))


time.end=proc.time()[[3]]
dif <- time.end-time.start
dif





time.start=proc.time()[[3]]

block.compare.train <- CompareAllBlocksInLoop(blockTrain$DataSplit,
                                             blockTrain$IdSplit,
                                             variables.to.match = c("fname_c1",
                                                                    "lname_c1",
                                                                    "by",
                                                                    "bm",
                                                                    "bd"),
                                             string.comparators = c("jarowinkler",
                                                                    "jarowinkler",
                                                                    "AbsoluteDifference",
                                                                    "AbsoluteDistance",
                                                                    "AbsoluteDistance"),
                                             record.ids.to.keep=c("PreSplitRecord", "PreBlockRecord"))


time.end=proc.time()[[3]]
dif <- time.end-time.start
dif




library(doMC)
options(cores = 8)
registerDoMC()



time.start=proc.time()[[3]]
block.compare.test <- CompareAllBlocksInLoopPC(blockTest$DataSplit,
                                             blockTest$IdSplit,
                                             variables.to.match = c("fname_c1",
                                                                    "lname_c1",
                                                                    "by",
                                                                    "bm",
                                                                    "bd"),
                                             string.comparators = c("jarowinkler",
                                                                    "jarowinkler",
                                                                    "AbsoluteDifference",
                                                                    "AbsoluteDistance",
                                                                    "AbsoluteDistance"),
                                             record.ids.to.keep=c("PreSplitRecord", "PreBlockRecord"))
time.end=proc.time()[[3]]
dif <- time.end-time.start
dif





time.start=proc.time()[[3]]

block.compare.train <- CompareAllBlocksInLoopPC(blockTrain$DataSplit,
                                              blockTrain$IdSplit,
                                              variables.to.match = c("fname_c1",
                                                                     "lname_c1",
                                                                     "by",
                                                                     "bm",
                                                                     "bd"),
                                              string.comparators = c("jarowinkler",
                                                                     "jarowinkler",
                                                                     "AbsoluteDifference",
                                                                     "AbsoluteDistance",
                                                                     "AbsoluteDistance"),
                                              record.ids.to.keep=c("PreSplitRecord", "PreBlockRecord"))


time.end=proc.time()[[3]]
dif <- time.end-time.start
dif

