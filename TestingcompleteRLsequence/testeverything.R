
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

splitRL <- SplitIntoTrainTest(fullRL, fullID, seed=6, prob.of.train = .5)

head(splitRL$training.data)
head(splitRL$testing.data)




####################################
# Block
####################################

blockTrain <- BlockRlData(splitRL$training.data,
                          var.names = "bm",
                          n.chars = 2,
                          splitRL$train.id)

head(blockTrain$DataSplit[[1]])
head(blockTrain$DataSplit[[2]])


blockTest <- BlockRlData(splitRL$testing.data,
                          var.names = "bm",
                          n.chars = 2,
                          splitRL$test.id)


####################################
# Compare
####################################


compareTrain <- CompareUniqueCombinations(blockTrain$DataSplit[[1]],
                                      unique.ids=blockTrain$IdSplit[[1]],
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
                                      record.ids.to.keep=NULL)

head(compareTrain)

compareTrain2 <- CompareUniqueCombinations(blockTrain$DataSplit[[1]],
                                          unique.ids=blockTrain$IdSplit[[1]],
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
                                          record.ids.to.keep="PreSplitRecord")
head(blockTrain$DataSplit[[1]])
head(compareTrain2)


compareTrain3 <- CompareUniqueCombinations(blockTrain$DataSplit[[1]],
                                           unique.ids=blockTrain$IdSplit[[1]],
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
head(blockTrain$DataSplit[[1]])
head(compareTrain3)


####################################
# Compare all at once
####################################



block.comparisons <- vector("list", length(blockTrain$DataSplit))

for(i in seq_along(blockTrain$DataSplit)){
  block.comparisons[[i]] <- CompareUniqueCombinations(blockTrain$DataSplit[[i]],
                                                      unique.ids=blockTrain$IdSplit[[i]],
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
}


head(block.comparisons[[1]])
length(block.comparisons)


#####
# OR
#####

block.comparisons2 <- CompareAllBlocksInLoop(blockTrain$DataSplit,
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


head(block.comparisons2[[1]])
length(block.comparisons2)

block.comparisonsNULL <- CompareAllBlocksInLoop(blockTrain$DataSplit,
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

head(block.comparisonsNULL[[1]])
length(block.comparisonsNULL)



#####################
# Merge Comparisons
#####################

training.merged <- MergeAllBlocks(block.comparisons2)




####
# Compare test
####

block.comparisonsTest <- CompareAllBlocksInLoop(blockTest$DataSplit,
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


head(block.comparisonsTest[[1]])
length(block.comparisonsTest)

testing.merged <- MergeAllBlocks(block.comparisonsTest)


###################
# Create a model
###################

model <- glm(True_Match ~ fname_c1.jar + lname_c1.jar + by.Abs + bm.Abs + bd.Abs,
             data=training.merged,
             family = binomial)


##################
# Get unique ids
###################


get.test.ids <- HclustCutGLM(model, testing.merged, splitRL$testing.data, .5)

get.test.ids[1:20]


get.test.ids.by.block <- AllBlocksHclustCutGLM(model, block.comparisonsTest, blockTest$DataSplit, .5)

get.test.ids.by.block$block.hclust.ids[[1]]
get.test.ids.by.block$comparison.test.blocks[[1]]
get.test.ids.by.block$test.data.blocks[[1]]

head(get.test.ids.by.block$merged.comparison.data)
head(get.test.ids.by.block$merged.block.data)

table(get.test.ids.by.block$merged.comparison.data$True_Match, get.test.ids.by.block$merged.comparison.data$HclustMatch)



