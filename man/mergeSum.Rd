\name{mergeSum}
\alias{mergeSum}
\title{Recursively merge name datasets by summing comparable name counts}
\usage{
  mergeSum(dataframes)
}
\arguments{
  \item{dataframes}{A list of data frames with columns for
  Name, F, M, and Year}
}
\value{
  A single data frame with columns for Name, F, M, and Year
}
\description{
  Function to merge name data from difference countryies,
  matching by name and summing counts.
}
\keyword{merge}

