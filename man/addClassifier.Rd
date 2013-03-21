\name{addClassifier}
\alias{addClassifier}
\title{Read name data formatted as gender-year combinations and
  accumulates total counts per name}
\usage{
  addClassifier(data, method = "ac", lower.t = 0.5,
    unknown = TRUE, ...)
}
\arguments{
  \item{data}{data frame with columns for Name,
  years.appearing, counts.female and counts.male}

  \item{method}{string passed to binom.confint}

  \item{lower.t}{numeric indicating the minimum lower bound
  of the confidence interval acceptable for classification}

  \item{unknown}{Logical indicating if you want the
  classifier to return "unknown" for names with low
  confidence}

  \item{...}{Additional arguments to be passed to
  \code{\link{binom.confint}}}
}
\value{
  A data frame with the same structure but appended columns
  for prob.gender (a factor) and upper and lower bounds for
  confidence intervals
}
\description{
  Designed to be fast and flexible, allowing for rapid
  testing
}

