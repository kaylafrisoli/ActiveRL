
library(RecordLinkage)
library("devtools")
devtools::load_all()
data("RLdata500")
data("RLdata10000")

fullRL <- rbind(RLdata500, RLdata10000)
fullID <- c(identity.RLdata500, identity.RLdata10000)

####################################
# Train/ Test
####################################

splitRL <- SplitIntoTrainTest(fullRL, fullID, seed=16, prob.of.train = .5)

head(splitRL$training.data)

head(splitRL$testing.data)



####################################
# Block Train
####################################

blockTrain <- BlockRlData(splitRL$training.data,
                          var.names = "bm",
                          n.chars = 2,
                          splitRL$train.id)

blockTrain$BlockInfo$reduction.ratio
head(blockTrain$DataSplit[[1]])
head(blockTrain$IdSplit[[1]])
blockTrain$DataSplitSingles
blockTrain$IdSplitSingles
table(blockTrain$BlockInfo$blocks)



####################################
# Block Test
####################################

blockTest <- BlockRlData(splitRL$testing.data,
                          var.names = "bm",
                          n.chars = 2,
                          splitRL$test.id)

blockTest$DataSplitSingles

####################################
# Compare within train
####################################

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


head(block.compare.train[[1]])

####################################
# Compare within test
####################################

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



#####################
# Merge training comparisons
#####################

training.merged <- MergeAllBlocks(block.compare.train)


####################################
# Build an adapted or non-adapted model
####################################

colnames(training.merged)

model <- glm(True_Match ~ fname_c1.jar + lname_c1.jar + by.Abs + bm.Abs + bd.Abs,
             data=training.merged,
             family = binomial)



####################################
# Test on each block of testing
####################################


get.test.ids.by.block <- AllBlocksHclustCutGLM(model, block.compare.test, blockTest$DataSplit, .5)

get.test.ids.by.block$block.hclust.ids[[1]]
get.test.ids.by.block$comparison.test.blocks[[1]]
get.test.ids.by.block$test.data.blocks[[1]]

head(get.test.ids.by.block$merged.comparison.data)
head(get.test.ids.by.block$merged.block.data)

table(get.test.ids.by.block$merged.comparison.data$True_Match, get.test.ids.by.block$merged.comparison.data$HclustMatch)
evaluation(get.test.ids.by.block$merged.comparison.data$True_Match, get.test.ids.by.block$merged.comparison.data$HclustMatch)

# blockTest$DataSplitSingles






