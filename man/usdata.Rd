\docType{data}
\name{usdata}
\alias{usdata}
\title{Name and Gender Data for the United States}
\format{A dataframe with 1571650 observations on the following 4 variables,

\describe{

\item{Name}{String: Birth name as recorded by the SSA, may be changed from uppercase.}

\item{F}{Numeric: Counts for births with that name recorded as female in a Year}

\item{M}{Numeric: Counts for births with that name recorded as male in a Year}

\item{Year}{Numeric: Years associated with given counts for a name}

}}
\source{
  http://www.ons.gov.uk/ons/rel/vsob1/baby-names--england-and-wales/index.html
}
\description{
  The Social Security Administration provides gender data
  for names with greater than 5 births per year. Full name
  data is provided between 1880 and 2011. Births prior to
  1937 are generated from social security applications and
  may not be completely accurate.
}
\references{
  Office for National Statistics, Statistical bulletin:
  Baby Names - England and Wales, 2011 [computer file].
  Vital Statistics Outputs Branch
}
\keyword{datasets}

