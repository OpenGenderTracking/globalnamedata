\docType{data}
\name{usprocessed}
\alias{usprocessed}
\title{Processed name data for the United States}
\format{A dataframe with 89925 observations on the following 7 variables,

\describe{

\item{Name}{String: Birth name as recorded by agencies, may be changed from uppercase}

\item{years.appearing}{Numeric: Number of years where the name appears}

\item{counts.female}{Numeric: Counts for births with that name recorded as female}

\item{counts.male}{Numeric: Counts for births with that name recorded as male}

\item{prob.gender}{Factor: Likely gender, outputted from the default classifier}

\item{emp.male}{Numeric: Empirical proportion of male recorded as gender for the given name}

\item{est.male}{Numeric: Estimated proportion of male names based on classifier}

\item{upper}{Numeric: Upper bound for confidence interval}

\item{lower}{Numeric: Lower bound for confidence interval}

}}
\description{
  Processed data from the United States
}
\keyword{datasets}

