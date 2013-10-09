#' Name and Gender Data for England and Wales
#' 
#' 
#' The Office of National Statistics provides gender data for names with 
#' greater than 3 births per year. Full name data is provided between 1996 and 
#' 2012. The ONS offers historical summary data for 1904-1994 but these are 
#' restricted to the most popular names per year and so are not of much analytical
#' value and not provided here.
#' 
#' @name ewnames
#' @docType data
#' @format A dataframe with 178811 observations on the following 4 variables,
#'
#' \describe{
#' 
#' \item{Name}{String: Birth name as recorded by the ONS, may be changed from uppercase.}
#' 
#' \item{F}{Numeric: Counts for births with that name recorded as female in a Year}
#' 
#' \item{M}{Numeric: Counts for births with that name recorded as male in a Year}
#'
#' \item{Year}{Numeric: Years associated with given counts for a name}
#'
#' }
#' @references Office for National Statistics, Statistical bulletin: 
#' Baby Names - England and Wales, 2011 [computer file]. Vital Statistics Outputs Branch
#' @source http://www.ons.gov.uk/ons/rel/vsob1/baby-names--england-and-wales/index.html
#' @keywords datasets
NULL

#' Name and Gender Data for Northern Ireland
#' 
#' 
#' The Northern Ireland Statistics and Research Administration provides gender 
#' data for names with greater than 3 births per year. 
#' Full name data is provided between 1997 and 2012. 
#' 
#' @name ninames
#' @docType data
#' @format A dataframe with 13857 observations on the following 4 variables,
#'
#' \describe{
#' 
#' \item{Name}{String: Birth name as recorded by NISRA, may be changed from uppercase.}
#' 
#' \item{F}{Numeric: Counts for births with that name recorded as female in a Year}
#' 
#' \item{M}{Numeric: Counts for births with that name recorded as male in a Year}
#'
#' \item{Year}{Numeric: Years associated with given counts for a name}
#'
#' }
#' @references Northern Ireland Statistics and Research Administration, Baby 
#' Names Statistical Bulletin [computer file]. Demography Branch
#' @source http://www.nisra.gov.uk/demography/default.asp28.htm
#' @keywords datasets
NULL

#' Name and Gender Data for Scotland
#' 
#' Scotland's General Register Office provides gender 
#' data for names with greater than 3 births per year. 
#' Full name data is provided for 2009 to 2012. 
#'
#' @name scotnames
#' @docType data
#' @format A dataframe with 24414 observations on the following 4 variables,
#'
#' \describe{
#' 
#' \item{Name}{String: Birth name as recorded by the GRO, may be changed from uppercase.}
#' 
#' \item{F}{Numeric: Counts for births with that name recorded as female in a Year}
#' 
#' \item{M}{Numeric: Counts for births with that name recorded as male in a Year}
#'
#' \item{Year}{Numeric: Years associated with given counts for a name}
#'
#' }
#' @references General Register Office, Baby Names Statistical Bulletin [computer file].
#' National Records of Scotland
#' @source http://www.gro-scotland.gov.uk/statistics/theme/vital-events/births/bckgr-info.html
#' @keywords datasets
NULL

#' Name and Gender Data for the United States
#' 
#' 
#' The Social Security Administration provides gender data for names with 
#' greater than 5 births per year. Full name data is provided between 1880 and 
#' 2011. Births prior to 1937 are generated from social security applications
#' and may not be completely accurate.
#' 
#' @name usnames
#' @docType data
#' @format A dataframe with 1571650 observations on the following 4 variables,
#'
#' \describe{
#' 
#' \item{Name}{String: Birth name as recorded by the SSA, may be changed from uppercase.}
#' 
#' \item{F}{Numeric: Counts for births with that name recorded as female in a Year}
#' 
#' \item{M}{Numeric: Counts for births with that name recorded as male in a Year}
#'
#' \item{Year}{Numeric: Years associated with given counts for a name}
#'
#' }
#' @references Social Security Administration, Baby Names. Office Of The Chief Actuary
#' @source http://www.ssa.gov/oact/babynames/index.html
#' @keywords datasets
NULL

#' Name and Gender Data for the United Kingdom
#' 
#'
#' Merged data from England and Wales, Northern Ireland and Scotland
#' 
#' @name uknames
#' @docType data
#' @format A dataframe with 189264 observations on the following 4 variables,
#'
#' \describe{
#' 
#' \item{Name}{String: Birth name as recorded by agencies, may be changed from uppercase.}
#' 
#' \item{F}{Numeric: Counts for births with that name recorded as female in a Year}
#' 
#' \item{M}{Numeric: Counts for births with that name recorded as male in a Year}
#'
#' \item{Year}{Numeric: Years associated with given counts for a name}
#'
#' }
#'
#' @keywords datasets
NULL

#' Processed name data for the United Kingdom
#' 
#'
#' Processed data from England and Wales, Northern Ireland and Scotland
#' 
#' @name ukprocessed
#' @docType data
#' @format A dataframe with 34347 observations on the following 7 variables,
#'
#' \describe{
#' 
#' \item{Name}{String: Birth name as recorded by agencies, may be changed from uppercase}
#' 
#' \item{years.appearing}{Numeric: Number of years where the name appears}
#' 
#' \item{counts.female}{Numeric: Counts for births with that name recorded as female}
#' 
#' \item{counts.male}{Numeric: Counts for births with that name recorded as male}
#' 
#' \item{prob.gender}{Factor: Likely gender, outputted from the default classifier}
#'
#' \item{emp.male}{Numeric: Empirical proportion of male recorded as gender for the given name}
#' 
#' \item{est.male}{Numeric: Estimated proportion of male names based on classifier}
#'
#' \item{upper}{Numeric: Upper bound for confidence interval}
#' 
#' \item{lower}{Numeric: Lower bound for confidence interval}
#'
#' }
#'
#' @keywords datasets
NULL

#' Processed name data for the United States
#' 
#'
#' Processed data from the United States
#' 
#' @name usprocessed
#' @docType data
#' @format A dataframe with 89925 observations on the following 7 variables,
#'
#' \describe{
#' 
#' \item{Name}{String: Birth name as recorded by agencies, may be changed from uppercase}
#' 
#' \item{years.appearing}{Numeric: Number of years where the name appears}
#' 
#' \item{counts.female}{Numeric: Counts for births with that name recorded as female}
#' 
#' \item{counts.male}{Numeric: Counts for births with that name recorded as male}
#' 
#' \item{prob.gender}{Factor: Likely gender, outputted from the default classifier}
#'
#' \item{emp.male}{Numeric: Empirical proportion of male recorded as gender for the given name}
#' 
#' \item{est.male}{Numeric: Estimated proportion of male names based on classifier}
#'
#' \item{upper}{Numeric: Upper bound for confidence interval}
#' 
#' \item{lower}{Numeric: Lower bound for confidence interval}
#'
#' }
#'
#' @keywords datasets
NULL
