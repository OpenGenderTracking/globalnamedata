\name{nameMetric}
\alias{nameMetric}
\title{Compute gender balance}
\usage{
  nameMetric(data, names, bounds = NULL, metric)
}
\arguments{
  \item{data}{A data frames with columns for Name, M, F,
  and Year e.g. one returned by \code{\link{usnames}}}

  \item{names}{A character vector of names (potentially of
  length 1)}

  \item{range}{An (optional) numeric vector of length 2
  with the start and end years inclusive}

  \item{metric}{A character vector of length 1. "Male",
  "Female" or "Neutral"}
}
\value{
  A single data frame with columns for the metric, number
  of births the years and the name.
}
\description{
  Determine the per-year gender breakdown for names
  anchored to male, female or neutral
}

