# ActiveRL

# ActiveRL - R package for record linkage 



## Installation

Installation of ActiveRL is done through the `devtools` package in R. You will need to download the package if you have never done so before using the command `install.packages("devtools")`.

```R
library(devtools)
install_github('kaylafrisoli/ActiveRL')
```

## Description

Record linkage is the process of identifying records corresponding to unique entities across multiple data sets. This is necessary because records within a data set may not share a unique identifier, such as social security number or licence plate number. Record linkage is a common problem in many fields. For example, if two airlines merge they may want to link their flight records to gain information on their customers. They would need to decide if Sam Smith with birth date 05/15/86 is the same person as Samuel Smyth with birth date 05/16/86. Record linkage is especially important for government organizations like the U.S. Census Bureau, who require accurate linkages of information across their many data sources.

ActiveRL provides the tools to perform record linkage on a data set of records. If you have training data you can utilize current supervised learning methods to create unique ids for your testing data. If not, you can create your own training data and then utilize the same methods to create unique ids. 

We build our own training data using an active learning approach that allows users to create their own optimized training datasets for specific record linkage problems. Our algorithm prompts users to label record-pairs for which its predictions are most uncertain, increasing the resulting classifier’s predictive power. There is much work to be done in this section and we are continually learning more about active learning, and how to handle situations wtih imbalanced classes (more record non-matches than record matches).


## Record linkage example

We will walk through an example from a data set built in to the `RecordLinkage` package in R. You will need to install this package if you do not already have it.

```R
library(ActiveRL)
library(RecordLinkage)
data("RLdata500")
ids <- identity.RLdata500

```

#### Split into training and testing 

First we will evenly split our data into testing and training data.

```R
splitRL <- SplitIntoTrainTest(RLdata500, ids, seed=16, prob.of.train = .5)
?SplitIntoTrainTest

```

#### Block

Then we will block both our training and testing data by birth month. In this example we create 12 different data sets, where each one corresponding to a month. This reduces our comparison space and makes computations more feasabile. Each block is independent of one another and therefore we perform record linkage only within each block.


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

#### Make comparisons

We then need to make comparisons between records, within each block. We will do this all at once at first and then we will parallelize this process across blocks, because they are independent. We are going to compare each field in `variables.to.match` using the functions in `string.comparators`. These should match up respectively. 

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

#### Make comparisons in parallel 

Running the loop across cores:

```R

library(doMC)
options(cores = 6)
registerDoMC()


Dsplit  <- blockTrain$DataSplit
Idsplit <- blockTrain$IdSplit
variables.to.match <- c("fname_c1",
                        "lname_c1",
                        "by",
                        "bm",
                        "bd")
string.comparators <- c("jarowinkler",
                        "jarowinkler",
                        "AbsoluteDifference",
                        "AbsoluteDistance",
                        "AbsoluteDistance")
record.ids.to.keep <- c("PreSplitRecord", "PreBlockRecord")

block.comparison.lists <- vector("list", length(Dsplit))


block.comparison.lists <- foreach(i = seq_along(Dsplit)) %dopar% {

    #   ids.for.loop <- vector("list", length(Dsplit))
    if(is.null(Idsplit)){
      ids.for.loop <- NULL
    } else{
      ids.for.loop <- Idsplit[[i]]
    }

    comparison.in.block <- as.data.frame(CompareUniqueCombinations(as.data.frame(Dsplit[[i]]),
                                                     as.vector(ids.for.loop),
                                                     variables.to.match = variables.to.match,
                                                     string.comparators = string.comparators,
                                                     record.ids.to.keep = record.ids.to.keep))

}
  
compare.train <- block.comparison.lists

                                              
options(cores = 6)
registerDoMC()

Dsplit  <- blockTest$DataSplit
Idsplit <- blockTest$IdSplit

block.comparison.lists <- vector("list", length(Dsplit))

block.comparison.lists <- foreach(i = seq_along(Dsplit)) %dopar% {

    #   ids.for.loop <- vector("list", length(Dsplit))
    if(is.null(Idsplit)){
      ids.for.loop <- NULL
    } else{
      ids.for.loop <- Idsplit[[i]]
    }

    comparison.in.block <- as.data.frame(CompareUniqueCombinations(as.data.frame(Dsplit[[i]]),
                                                     as.vector(ids.for.loop),
                                                     variables.to.match = variables.to.match,
                                                     string.comparators = string.comparators,
                                                     record.ids.to.keep = record.ids.to.keep))

  }
  
compare.test <- block.comparison.lists

   

```

#### Model

We then merge or training comparisons and build a model on the comparison data. 

```R

training.merged <- MergeAllBlocks(compare.train)

model <- glm(True_Match ~ fname_c1.jar + lname_c1.jar + by.Abs + bm.Abs + bd.Abs,
             data=training.merged,
             family = binomial)

```

#### Predict, cluster, assign unique ids


We can then calculate the probability that records in our testing data match and hierarchically cluster based on these probabilities. We end up with unique ids for our testing data.

```R

get.test.ids.by.block <- AllBlocksHclustCutGLM(model, compare.test, blockTest$DataSplit, .5)

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


#### Create your own data

```R

createTraining <- BuildATrainingDataset(RLdata500,
                                        n.pairs.to.test=10,
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


> createTraining$tested.comparisons
       fname_c1.jar lname_c1.jar by.Abs bm.Abs bd.Abs True_Match CurrentRecord1 CurrentRecord2 Active_Match
29        0.0000000    0.0000000     45      6      1         NA              1             30            0
125       0.0000000    0.0000000     22      2      1         NA              1            126            0
121282    1.0000000    1.0000000     32      2     11         NA            417            435            0
122238    1.0000000    1.0000000     16      1      8         NA            429            473            0
124569    1.0000000    1.0000000      9      1      1         NA            481            490            1
57032     0.5555556    0.5873016      7      5     15         NA            132            310            0
85128     0.5972222    0.7703704     39      2     15         NA            218            499            0
103440    0.4179894    0.6583333     19      2      4         NA            294            305            0
41309     0.5396825    0.5619048     42      1     12         NA             91            495            0
66563     0.5000000    0.0000000     38      5     15         NA            159            283            0


```





# ActiveRL
