\name{docsFromIndex}
\alias{docsFromIndex}
\title{Return Locations from Parsed Index}
\usage{
  docsFromIndex(url, xpath)
}
\arguments{
  \item{url}{A character vector of length 1 specifying the
  index url}

  \item{xpath}{A character vector of length 1 specifying
  the xpath query}
}
\value{
  A character vector of matched locations
}
\description{
  Wrapper for xpathSApply and htmlParse
}

