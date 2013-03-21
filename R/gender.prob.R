
#' Read name data formatted as gender-year combinations and 
#'   accumulates total counts per name
#'
#' Return total counts and years appearing to offer multiple ways to gauge
#' likelihood of a match.
#'
#' @param data A data frame with columns for Name, F, M, and Year
#' @return Data frame with columns for Name, counts for 
#'   gender incidence and year counts
#' @export
byNameCount <- function(data) {
  # input structure should look like this:
  #     Name  F   M Year
  # 1  Aaron  0 102 1880
  # 2     Ab  0   5 1880
  # 3  Abbie 71   0 1880
  # 4 Abbott  0   5 1880
  # 5   Abby  6   0 1880
  # 6    Abe  0  50 1880
  # count occurences of name by gender-year and by year only
  countYears <- function(data, sex) {
    # restrict to sex passed by argument
    years <- count(data[data[, sex] > 0, ], vars = c("Name", sex))
    # count gender-year
    name.counts <- with(years, rowsum(freq * get(sex), Name, reorder = FALSE))
    # year only
    year.counts <- with(years, rowsum(freq, Name, reorder = FALSE))
    year.df <- data.frame(Name = rownames(year.counts), 
                          Counts = name.counts,
                          Appearances = year.counts,
                          stringsAsFactors = FALSE)
    names(year.df) <- c("Name", 
                        paste0("Count", substitute(sex)),
                        paste0("Sumyears", substitute(sex)))
    
    return(unrowname(year.df))
  }

  # populate name column quickly 
  data.out <- data.frame(Name = sort(unique(data[, "Name"])),
                         freq = count(data, "Name")[, 2], 
                         stringsAsFactors = FALSE)

  data.out <- cbind(data.out,  merge(countYears(data, "M"), 
                                    countYears(data, "F"), 
                                    by = "Name", all = TRUE)[, -1])

  
  # cleanup NAs generated from merging countYears()
  data.out[is.na(data.out)] <- 0

  # drops intermediate columns and rename
  data.out <- data.out[, c("Name", "freq", 
                           "CountF", "CountM")]
  names(data.out) <- c("Name", "years.appearing", 
                       "counts.female", "counts.male")
#    Name years.appearing counts.female counts.male
#       A              6            0         32
#   A-jay             10            0         52
#    A.j.              2            0          6
#  A'isha             10           52          0
# A'ishah             13           67          0
# Aa'isha              1            3          0
  return(data.out)
}


#' Read name data formatted as gender-year combinations and 
#'   accumulates total counts per name
#'
#' Designed to be fast and flexible, allowing for rapid testing
#'
#' @param data data frame with columns for Name, years.appearing,
#'   counts.female and counts.male
#' @param method string passed to binom.confint
#' @param lower.t numeric indicating the minimum lower bound of 
#'   the confidence interval acceptable for classification
#' @param unknown Logical indicating if you want the classifier to 
#'   return "unknown" for names with low confidence
#' @param ... Additional arguments to be passed to \code{\link{binom.confint}}
#'
#' @return A data frame with the same structure but appended columns for
#'   prob.gender (a factor) and upper and lower bounds for confidence 
#'   intervals
#'
#' @export
#' @importFrom binom binom.confint
addClassifier <- function(data, method = "ac", 
                          lower.t = 0.5,
                          unknown = TRUE, 
                          ...) {
  data <- data[, c("Name", "years.appearing",
                   "counts.female", "counts.male")]
  binom.out <- with(data, binom.confint(counts.male,
                                        counts.male + counts.female,
                                        method = method,
                                        ...))
  prop <- with(data, counts.male /(counts.male + counts.female))

  # assign as male if we meet the thresholds
  # Because the ratios are essentially symmetric we can assign
  # confidence intervals for both in one pass
  gender.prediction <- with(binom.out, ifelse(mean > 0.5,
                                              "Male", "Female"))
  gender.upper <- with(binom.out, ifelse(mean > 0.5, upper, 1 - lower))
  gender.lower <- with(binom.out, ifelse(mean > 0.5, lower, 1 - upper))

  gender.prediction <- ifelse(abs(prop - 0.5) + gender.lower > lower.t + 0.05 |
                              gender.lower > lower.t,
                              gender.prediction, "Unknown")
  
  data.pred <- data.frame(prob.gender = gender.prediction,
                          emp.male = prop,
                          est.male = binom.out[, "mean"],
                          upper = gender.upper,
                          lower = gender.lower)

# Output structure will look like:
#   prob.gender emp.male est.male    upper     lower
# 1        Male        1        1 1.020139 0.8726819
# 2        Male        1        1 1.013403 0.9178043
# 3        Male        1        1 1.052446 0.5572192
# 4      Female        0        0 1.013403 0.9178043
# 5      Female        0        0 1.010707 0.9350666
# 6     Unknown        0        0 1.055975 0.3825284
  return(cbind(data, data.pred))
}
