\name{yearBirths}
\alias{yearBirths}
\title{Count Birth Totals}
\usage{
  yearBirths(data, bounds = NULL)
}
\arguments{
  \item{data}{A data frames with columns for Name, M, F,
  and Year e.g. one returned by \code{\link{usnames}}}

  \item{range}{A numeric vector of length 2 with the start
  and end years inclusive}
}
\value{
  A single data frame with columns for Births, Gender and
  Year in "long" format a la \code{\link{reshape}}
}
\description{
  Given a data frame and an optional range of years, return
  a data frame of birth totals for those years
}

