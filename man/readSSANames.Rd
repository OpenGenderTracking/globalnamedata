\name{readSSANames}
\alias{readSSANames}
\title{Read and return United States name data from the
  Social Security Administration}
\usage{
  readSSANames()
}
\value{
  Data frame with columns for Name, Year, and counts for
  gender incidence
}
\description{
  Download data from the SSA website and convert into a
  single data frame Downloading and converting data will
  take some time. The resultant dataset is provided as
  \code{\link{usnames}}
}
\seealso{
  \code{\link{readONSNames}}, \code{\link{readNISRANames}},
  \code{\link{readScotlandNames}}
}
\keyword{america}

