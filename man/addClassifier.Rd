\name{addClassifier}
\alias{addClassifier}
\title{Read name data formatted as gender-year combinations and
  accumulates total counts per name}
\usage{
  addClassifier(data, method = "wilson", upper.t = 0.7,
    lower.t = 0.05)
}
\arguments{
  \item{data}{data frame with columns for Name,
  YearsAppearing, CountsFemale and CountsMale}

  \item{method}{string passed to binom.confint}

  \item{upper.t}{numeric indicating the minimum upper bound
  of the confidence interval which allows us to classify a
  name/gender}

  \item{lower.t}{numeric indicating the maximum lower bound
  of the confidence interval acceptable for classification}
}
\value{
  A data frame with the same structure but appended columns
  for ProbGender (a factor) and Upper and Lower bounds for
  confidence intervals
}
\description{
  Designed to be fast and flexible, allowing for rapid
  testing
}

