% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/blocking.R
\name{Comparisons}
\alias{Comparisons}
\title{Compare n records}
\usage{
Comparisons(n)
}
\arguments{
\item{n}{Number of records you want to compare}
}
\value{
The total number of pair-comparisons in n records
}
\description{
Number of comparisons made in n records
}
\examples{
Comparisons(dim(iris)[1])
}

