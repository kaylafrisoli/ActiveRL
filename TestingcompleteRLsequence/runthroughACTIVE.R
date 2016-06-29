
##################################
# Without blocking
##################################

library(RecordLinkage)
library("devtools")
devtools::load_all()
data("RLdata500")
data("RLdata10000")

fullRL <- rbind(RLdata500, RLdata10000)


####################################
# Let's subset our data for ease
####################################


set.seed(89)
smaller.subset <- sample(c(TRUE,FALSE), nrow(fullRL), rep=TRUE, prob = c(.05, .95))

table(smaller.subset)

subsetRLdata <- fullRL[smaller.subset, ]


createTraining <- BuildATrainingDataset(subsetRLdata,
                                        n.pairs.to.test=30,
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
                                        standardized.variables=c("fname_c1",
                                                                 "lname_c1"))


createTraining$tested.comparisons
createTraining$tested.data
head(createTraining$comparisons)

#write.csv(createTraining$comparisons, "/Users/kaylafrisoli/Documents/CMU/Census/Package/BasicRL/TestingcompleteRLsequence/totalcomparisons.csv")
#write.csv(createTraining$tested.data, "/Users/kaylafrisoli/Documents/CMU/Census/Package/BasicRL/TestingcompleteRLsequence/testeddata.csv")
#write.csv(createTraining$tested.comparisons, "/Users/kaylafrisoli/Documents/CMU/Census/Package/BasicRL/TestingcompleteRLsequence/testedcomparisons.csv")

head(read.csv('/Users/kaylafrisoli/Documents/CMU/Census/Package/BasicRL/TestingcompleteRLsequence/totalcomparisons.csv'))
head(read.csv('/Users/kaylafrisoli/Documents/CMU/Census/Package/BasicRL/TestingcompleteRLsequence/testeddata.csv'))
head(read.csv('/Users/kaylafrisoli/Documents/CMU/Census/Package/BasicRL/TestingcompleteRLsequence/testedcomparisons.csv'))


