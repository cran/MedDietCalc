\name{periodicity}
\encoding{UTF-8}
\alias{periodicity}
\title{
Transforms data codified as daily, weekly or monthly, to any other of them.
}

\description{
Diferent scores of Mediterranean Diet set cutoffs of daily, weekly or monthly consumption. Additionally, a dataset can be stored as diferent frequency of consumption.
This function has been created to be called by others, it just multiplies or divides by the suitable numbre (for instance, from 'daily' to 'weekly' it just multiplies by 7)
}

\usage{
periodicity(x, OriginalFreq, TargetFreq)
}

\arguments{
  \item{x}{numeric variable or a list of numeric variables, which want to be converted}
  \item{OriginalFreq}{character string. The frequency in which information was captured (should be provided by user). Allowed values are 'daily', 'weekly' or 'monthly'}
  \item{TargetFreq}{character string. The frequency in which information has to be transformed. Allowed values are 'daily', 'weekly' or 'monthly'. Usually it will be provided by another formula, depending in its scoring scheme}
}
\value{A numeric vector, or a list of numeric vectors.}

\author{Miguel Menendez}

\examples{
foodA <- c(1,2,3)
foodB <- c(3,2,1)
L <- list(foodA = foodA, foodB= foodB)

# Use with a numeric variable
periodicity(foodA, "daily", "weekly")

#Use with a list
periodicity(L, "daily", "weekly")
}
