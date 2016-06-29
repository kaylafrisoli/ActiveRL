
library(devtools)
install_github('kaylafrisoli/BasicRL')



library(BasicRL)
library(RecordLinkage)
data("RLdata500")
ids <- identity.RLdata500

splitRL <- SplitIntoTrainTest(RLdata500, ids, seed=16, prob.of.train = .5)


blockTrain <- BlockRlData(splitRL$training.data,
                          var.names = "bm",
                          n.chars = 2,
                          splitRL$train.id)



blockTest <- BlockRlData(splitRL$testing.data,
                         var.names = "bm",
                         n.chars = 2,
                         splitRL$test.id)




library(doMC)
options(cores = 8)
registerDoMC()

compare.train <- CompareAllBlocksInLoopPC(blockTrain$DataSplit,
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
                                          record.ids.to.keep=c("PreSplitRecord", "PreBlockRecord"),
                                          num.cores=6)
















#### Make comparisons



compare.train <- CompareAllBlocksInLoop(blockTrain$DataSplit,
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


compare.test <- CompareAllBlocksInLoop(blockTest$DataSplit,
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


  return(block.comparison.lists)



CompareAllBlocksInLoopPC <- function(Dsplit,
                                       Idsplit=NULL,
                                       variables.to.match=NULL,
                                       string.comparators=NULL,
                                       record.ids.to.keep=NULL,
                                       num.cores=NULL){

    if(is.null(num.cores)){
      num.cores <- 1
    } else{
      num.cores <- num.cores
    }

    print(num.cores)

    block.comparison.lists <- vector("list", length(Dsplit))

    options(cores = num.cores)
    registerDoMC()

    foreach(i = seq_along(Dsplit), .packages = "BasicRL:::CompareUniqueCombinations") %dopar% {

      #   ids.for.loop <- vector("list", length(Dsplit))
      if(is.null(Idsplit)){
        ids.for.loop <- NULL
      } else{
        ids.for.loop <- Idsplit[[i]]
      }

      comparison.in.block <- BasicRL:::CompareUniqueCombinations(as.data.frame(Dsplit[[i]]),
                                                       as.vector(ids.for.loop),
                                                       variables.to.match = variables.to.match,
                                                       string.comparators = string.comparators,
                                                       record.ids.to.keep = record.ids.to.keep)

      print(head(comparison.in.block))

      block.comparison.lists[[i]] <- as.data.frame(comparison.in.block)
    }

    return(block.comparison.lists)

  }



compare.train <- CompareAllBlocksInLoopPC(blockTrain$DataSplit,
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
                                          record.ids.to.keep=c("PreSplitRecord", "PreBlockRecord"),
                                          num.cores = 6)


options(cores = 8)
registerDoMC()

compare.test <- CompareAllBlocksInLoopPC(blockTest$DataSplit,
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



#### Model


training.merged <- MergeAllBlocks(compare.train$)


model <- glm(True_Match ~ fname_c1.jar + lname_c1.jar + by.Abs + bm.Abs + bd.Abs,
             data=training.merged,
             family = binomial)


#### Predict, cluster, assign unique ids


We can then calculate the probability that records in our testing data match and hierarchically cluster based on these probabilities. We end up with unique ids for our testing data.

```R

get.test.ids.by.block <- AllBlocksHclustCutGLM(model, block.compare.test, blockTest$DataSplit, .5)

```

#### Assess error

We may want to check how well we did.

```R

true.match <- get.test.ids.by.block$merged.comparison.data$True_Match
predicted.match <- get.test.ids.by.block$merged.comparison.data$HclustMatch

> evaluation(true.match, predicted.match)
$contingency.table
predicted
truth    0    1
0 2596    0
1    2    8

$false.positive.error
[1] 0

$false.negative.error
[1] 0.2

$false.discovery.rate
[1] 0

$sensitivity
[1] 0.8

$specificity
[1] 1

$precision
[1] 1

$negative.predictive.value
[1] 0.9992302

$accuracy
[1] 0.9992325


```





