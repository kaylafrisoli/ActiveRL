# BasicRL

# BasicRL - R package for record linkage 



## Installation

Installation of BasicRL is done through the `devtools` package in R. You will need to download the package if you have never done so before using the command `install.packages("devtools")`.

```R
library(devtools)
install_github('kaylafrisoli/BasicRL')
```

## Description

BasicRL provides the tools to perform record linkage on a data set of records. If you have training data you can utilize current supervised learning methods to create unique ids for your testing data. If not, you can create your own training data and then utilize the same methods to create unique ids. 


## Record linkage example

We will walk through an example from a dataset built in to the `RecordLinkage` package in R. You will need to install this package if you do not already have it.

```R
library(BasicRL)
library(RecordLinkage)
data("RLdata500")
ids <- identity.RLdata500

```

First we will evenly split our data into testing and training data.

```R
splitRL <- SplitIntoTrainTest(RLdata500, ids, seed=16, prob.of.train = .5)
```

Then we will block both of our training and testing data by birth month.


```R

blockTrain <- BlockRlData(splitRL$training.data,
                          var.names = "bm",
                          n.chars = 2,
                          splitRL$train.id)
                          
   

blockTest <- BlockRlData(splitRL$testing.data,
                          var.names = "bm",
                          n.chars = 2,
                          splitRL$test.id)

```


We then need to make comparisons between records, within each block. We will do this all at once but we could parallelize this process across blocks, since they are independent. We are going to compare each field in `variables.to.match` using the functions in `string.comparators`. These should match up respectively. 

```R
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

```


We then merge or training comparisons and build a model on the compaison data. 

```R

training.merged <- MergeAllBlocks(block.compare.train)

model <- glm(True_Match ~ fname_c1.jar + lname_c1.jar + by.Abs + bm.Abs + bd.Abs,
             data=training.merged,
             family = binomial)

```


We can then calculate the probability that records in our testing data match and hierarchically cluster based on these probabilities. We end up with unique ids for our testing data.

```R

get.test.ids.by.block <- AllBlocksHclustCutGLM(model, block.compare.test, blockTest$DataSplit, .5)

```

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





