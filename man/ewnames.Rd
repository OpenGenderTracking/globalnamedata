\docType{data}
\name{ewnames}
\alias{ewnames}
\title{Name and Gender Data for England and Wales}
\format{A dataframe with 165457 observations on the following 4 variables,

\describe{

\item{Name}{String: Birth name as recorded by the ONS, may be changed from uppercase.}

\item{F}{Numeric: Counts for births with that name recorded as female in a Year}

\item{M}{Numeric: Counts for births with that name recorded as male in a Year}

\item{Year}{Numeric: Years associated with given counts for a name}

}}
\source{
  http://www.ons.gov.uk/ons/rel/vsob1/baby-names--england-and-wales/index.html
}
\description{
  The Office of National Statistics provides gender data
  for names with greater than 3 births per year. Full name
  data is provided between 1996 and 2011. The ONS offers
  historical summary data for 1904-1994 but these are
  restricted to the most popular names per year and so are
  not of much analytical value and not provided here.
}
\references{
  Office for National Statistics, Statistical bulletin:
  Baby Names - England and Wales, 2011 [computer file].
  Vital Statistics Outputs Branch
}
\keyword{datasets}

