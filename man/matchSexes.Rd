\name{matchSexes}
\alias{matchSexes}
\title{Match names across gender count}
\usage{
  matchSexes(x)
}
\arguments{
  \item{x}{A data frames with columns for Name, Sex, Count
  and Year}

  \item{by}{string denoting common value for data frame
  (e.g. year or state)}
}
\value{
  A single data frame with columns for Name, F, M, and Year
}
\description{
  Construct a pivot table for imput data frames which are
  in long format
}

