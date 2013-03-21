\name{readONSNames}
\alias{readONSNames}
\title{Read and return name data for England and Wales from the
  Office of National Statistics}
\usage{
  readONSNames()
}
\value{
  Data frame with columns for Name, Year, and counts for
  gender incidence
}
\description{
  Download data from the ONS website and convert into a
  single data frame Downloading and converting data will
  take some time. The resultant dataset is provided as
  \code{\link{ewnames}}
}
\seealso{
  \code{\link{readScotlandNames}},
  \code{\link{readNISRANames}}, \code{\link{readSSANames}}
}
\keyword{england}

