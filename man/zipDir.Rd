\name{zipDir}
\alias{zipDir}
\title{Download zipped folder and extract to temp directory}
\usage{
  zipDir(url, pattern = "ssa")
}
\arguments{
  \item{url}{string Location for zip file}

  \item{pattern}{string File name prefix}
}
\value{
  A string containing the path to the directory
}
\description{
  SSA releases names as text files in a directory.
  Downloading these files can be a bit laborious. See
  http://www.ssa.gov/oact/babynames/limits.html for details
}

