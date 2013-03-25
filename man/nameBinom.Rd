\name{nameBinom}
\alias{nameBinom}
\title{Read name data formatted as gender-year combinations and
  accumulates total counts per name}
\usage{
  nameBinom(data, method = "ac", threshold = 0.9, ...)
}
\arguments{
  \item{data}{data frame with columns for Name,
  years.appearing, counts.female and counts.male}

  \item{method}{string passed to binom.confint}

  \item{threshold}{numeric between 0.5 and 1 indicating the
  minimum proportion of male names required to classify a
  name as likely male. If threshold is less than 0.5 it
  will be used as minimum proportion of female names}

  \item{...}{Additional arguments to be passed to
  \code{\link{binom.confint}}}
}
\value{
  A data frame with the same structure but appended columns
  for prob.gender (a factor), estimated proportion and
  upper and lower bounds for confidence intervals
}
\description{
  Designed to be fast and flexible, allowing for rapid
  testing
}

