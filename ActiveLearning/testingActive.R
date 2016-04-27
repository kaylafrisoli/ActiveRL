
library(RecordLinkage)
library("devtools")
devtools::load_all()
data("RLdata500")
data("RLdata10000")

ProbItsAMatch(RLdata500[1,], RLdata500[2,])

hi <- IsItAMatch(RLdata500[1,], RLdata500[2,])

comp <- CompareUniqueCombinations(RLdata500,
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

smaller <- comp[1:50,]

variables.to.match = c("fname_c1",
                       "lname_c1")

standardized.variables <- c("fname_c1",
                            "lname_c1")
cls2 <- which(variables.to.match %in% standardized.variables)
cls2

#cls <- which(colnames(RLdata500) %in% hi)
apply(smaller[,cls2], 1, mean, na.rm=T)


standardized.variables = which( variables.to.match %in% names(which(sapply(RLdata500[, unique(variables.to.match)], is.factor) == TRUE)))

as.numeric()

#which(variables.to.match %in%  sapply(RLdata500[, variables.to.match], is.factor) == TRUE))

factor.vars <- names(which(sapply(RLdata500[, unique(variables.to.match)], is.factor) == TRUE))

factor.vars <- variables.to.match %>%
                unique() %>%
                  RLdata500[, .] %>%
                    sapply(., is.factor) %>%
                      which(. == TRUE) %>%
                        names()

average.similarity <- apply(comp[1:50, cls2], 1, mean, na.rm=T)


avg.sims <- data.frame(n=1:length(average.similarity), avg.sim=average.similarity)

avg.sims <- avg.sims[order(avg.sims$avg.sim),]

avg.sims[1:5,]


BuildATrainingDataset(RLdata500[1:15,],
                      n.pairs.to.test=5,
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




yeah <- BuildATrainingDataset(RLdata500[1:100,],
                                  n.pairs.to.test=15,
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












