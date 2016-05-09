

library(RecordLinkage)
library("devtools")
devtools::load_all()
data("RLdata500")
data("RLdata10000")


########################
# Split in to Train/Test
########################
dsets <- list(RLdata10000, RLdata500)

sup <- rbind(dsets)

str(dsets)

sup2 <- plyr::rbind.fill(dsets, rbind)

fullRL <- rbind(RLdata500, RLdata10000)
fullID <- c(identity.RLdata500, identity.RLdata10000)

fullid2 <- plyr::rbind.fill(fullID, rbind)


block.by.month <- BlockAndPrepareforComparison(RLdata=fullRL,
                                               var.names = c("bm"),
                                               n.chars = c(2),
                                               ids=fullID)

table(block.by.month$Blocks) # how do we want to train/test on blocks containing only 5 people

block.info <- BlockBySubstr(RLdata500, "fname_c1", 1)
block.factors <- block.info$factors


dsplit <- split(RLdata500, block.factors)
dsplit <- dsplit[which(as.numeric(table(block.factors)) >= 2)]
dsplit[[1]]

dsplit1 <- split(RLdata500, block.factors)
dsplit2 <- block.factors %>%
            table() %>%
              as.numeric() %>%
                which(. >= 2) %>%
                  dsplit1[.]






form <- True_Match ~ fname_c1.jar + lname_c1.jar + by.Abs + bm.Abs + bd.Abs

TryBlock1 <- CompleteRlByBlock(block.by.month$DataSplit[[1]],
                               block.by.month$IdSplit[[1]],
                               seed=6,
                               prob.of.train=.5,
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
                               formula=form,
                               cut.threshold=.5)



TryBlock1$seed
TryBlock1$hclust.results
TryBlock1$training.data
TryBlock1$train.comparisons
TryBlock1$testing.data



TryBlock2 <- CompleteRlByBlock(block.by.month$DataSplit[[2]],
                               block.by.month$IdSplit[[2]],
                               seed=6,
                               prob.of.train=.5,
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
                               formula=form,
                               cut.threshold=.5)


TryBlock2$seed
TryBlock2$hclust.results
TryBlock2$training.data[1:10,]
TryBlock2$testing.data[1:10,]
TryBlock2$train.comparisons[1:10,]









set.seed(16)
train <- sample(c(TRUE,FALSE), nrow(fullRL), rep=TRUE, prob = c(.5, .5))
test <- (! train)
training.data <- fullRL[train, ]
testing.data  <- fullRL[test, ]
train.id <- fullID[train]
test.id <- fullID[test]


train.block.prepareF1 <- BlockAndPrepareforComparison(RLdata=training.data,
                                                      var.names = c("fname_c1"),
                                                      n.chars = c(1),
                                                      ids=train.id)

table(train.block.prepareF1$Blocks)

train.block.prepareL1 <- BlockAndPrepareforComparison(RLdata=training.data,
                                                      var.names = c("lname_c1"),
                                                      n.chars = c(3),
                                                      ids=train.id)

sort(table(train.block.prepareL1$Blocks))

train.block.prepareBM <- BlockAndPrepareforComparison(RLdata=training.data,
                                                      var.names = c("bm"),
                                                      n.chars = c(2),
                                                      ids=train.id)

table(train.block.prepareBM$Blocks)
table(training.data$bm)


train.compareALL <- StickInABlock(train.block.prepareF1$DataSplit,
                                  train.block.prepareF1$IdSplit,
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
train.compareALL$block.data[[23]]
train.compareALL$block.comparison.lists[[23]]
train.compareALL$full.comparisons[1:10,]

train.compareB1 <- StickInABlock(train.block.prepareF1$DataSplit[[1]],
                                 train.block.prepareF1$IdSplit[[1]],
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

View(train.compareB1$block.data)
View(train.compareB1$block.comparison.lists)
