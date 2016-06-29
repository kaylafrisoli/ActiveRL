
library(RecordLinkage)
library("devtools")
devtools::load_all()
data("RLdata10000")

var.names <- c("fname_c1", "bm")
n.chars <- c(1, 2)


BlockRlDataAdapt <- function(RLdata,
                             var.names,
                             n.chars=NULL,
                             unique.ids=NULL,
                             max.size=NULL){

  if(is.null(max.size)){
    max.size <- 500
  } else{
    max.size <- max.size
  }

  RLdata.loop <- RLdata
  id.loop <- identity.RLdata10000


  options(expressions = 100000) # really should figure out what this means

  blocks.total <- list()
  dsplit.total <- list()
  idsplit.total <- list()
  dsplit.single.total <- list()
  idsplit.single.total <- list()

  for(i in 1:length(var.names)){

    block.info <- BlockRlData(RLdata.loop,
                              var.names[1:i],
                              n.chars[1:i],
                              id.loop)

    # block.too.big <- which(as.numeric(table(block.info$BlockInfo$blocks)) > max.size)
    block.just.right <- which(as.numeric(table(block.info$BlockInfo$blocks)) <= max.size)
    blocks.okayTF <- block.info$BlockInfo$blocks %in% names(table(block.info$BlockInfo$blocks)[block.just.right])
    blocks.total <- c(blocks.total, list(block.info$BlockInfo$blocks[blocks.okayTF]))

    dsplit.size <- sapply(block.info$DataSplit, nrow)

    too.big <- which(as.numeric(dsplit.size) > max.size)
    just.right <- which(as.numeric(dsplit.size) <= max.size)

    dsplit.total <- c(dsplit.total, block.info$DataSplit[just.right])
    idsplit.total <- c(idsplit.total, block.info$IdSplit[just.right])
    dsplit.single.total[[i]] <- block.info$DataSplitSingles
    idsplit.single.total[[i]] <- block.info$IdSplitSingles

    if(length(too.big) == 0) break

    RLdata.loop <- MergeAllBlocks(block.info$DataSplit[too.big])
    id.loop <- unlist(block.info$IdSplit[too.big])

  }

  total.blocks <- unlist(blocks.total)
  dsplit.singles <- MergeAllBlocks(dsplit.single.total)
  idsplit.singles <- unlist(idsplit.single.total)


  results <- list(BlockInfo = total.blocks,
                  DataSplit = dsplit.total,
                  IdSplit = idsplit.total,
                  DataSplitSingles = dsplit.singles,
                  IdSplitSingles = idsplit.singles)
  return(results)
}



trial1 <- BlockRlDataAdapt(RLdata10000,
                            var.names <- c("fname_c1", "bm"),
                            n.chars <- c(1, 2),
                           unique.ids = identity.RLdata10000,
                           max.size = 500)

table(trial1$BlockInfo)
trial1$DataSplitSingles
trial1$IdSplitSingles

trial1$DataSplit[[1]]
trial1$IdSplit[[1]]

trial1$DataSplit[[20]]
trial1$IdSplit[[20]]


trial2 <- BlockRlDataAdapt(RLdata10000,
                           var.names <- c("fname_c1", "lname_c1"),
                           n.chars <- c(1, 1),
                           unique.ids = identity.RLdata10000,
                           max.size = 500)

table(trial2$BlockInfo)
trial2$DataSplitSingles
trial2$IdSplitSingles

trial1$DataSplit[[20]]
trial1$IdSplit[[20]]
