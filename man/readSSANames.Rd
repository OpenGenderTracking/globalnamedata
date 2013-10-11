\name{readSSANames}
\alias{readSSANames}
\title{Read and return United States name data from the
  Social Security Administration}
\usage{
  readSSANames(type)
}
\arguments{
  \item{type}{string Specifying national or state level
  data}
}
\value{
  Data frame with columns for Name, Year, and counts for
  gender incidence
}
\description{
  Download data from the SSA website and convert into a
  single data frame Downloading and converting data will
  take some time.
}
\seealso{
  \code{\link{readONSNames}}, \code{\link{readNISRANames}},
  \code{\link{readGRONames}}
}
\keyword{america}

