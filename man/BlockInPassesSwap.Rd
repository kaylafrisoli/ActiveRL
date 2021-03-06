% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/blocking.R
\name{BlockInPassesSwap}
\alias{BlockInPassesSwap}
\title{Block a record linkage dataset in passes}
\usage{
BlockInPassesSwap(records, pass.structure, swap.structure = NULL,
  verbose = FALSE)
}
\arguments{
\item{records}{a data frame containing the records to be matched}

\item{pass.structure}{a list containing a matrix for each pass where the first column of the matrix contains the variables to block on and the second column contains the number of characters to use (NA will use the entire variable)}

\item{swap.structure}{a list containing a vector for each pass where the elements are a 0 if that variable should not be swapped and a 1 if it should. The vector should have the same number of elements as variables in the pass block and can add to either 2 or 0 (either 2 variables will be swapped or none)}
}
\value{
A data frame containing the ids of records we will compare and the blocking scheme used to choose them
}
\description{
Block a record linkage dataset in passes of different blocking schemes
}
\examples{
records <- data.frame(fname_c1=c("1949", "CARSTEN", "KAYLA", "KAYLA", "FRISOLI"),
                      lname_c1=c("MEIER", "MEIER", "FRISOLI","FRISOLI", "KAYLA"),
                      by= c("CARSTEN", "1949", "1993", "1993", "1993"),
                      bm=c("7", "7", "6", "6", "3"),
                      bd=c("22", "22", "12", "12", "10"))
pass.structure <- list(matrix(c("fname_c1", "by", NA, NA), ncol = 2),
                       matrix(c("fname_c1", "lname_c1", 3, 3), ncol = 2),
                       matrix(c("lname_c1", NA), ncol = 2))
swap.structure <- list(c(1, 1), c(1, 1), c(0))
BlockInPassesSwap(records, pass.structure, swap.structure)

}

