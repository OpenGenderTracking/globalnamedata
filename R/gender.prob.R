#####
###
### functions for gender computation
###
#####

#' Read name data formatted as gender-year combinations and 
#'   accumulates total counts per name
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
  require(plyr)
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
  names(data.out) <- c("Name", "YearsAppearing", 
                       "CountsFemale", "CountsMale")
#    Name YearsAppearing CountsFemale CountsMale
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
#' @param data data frame with columns for Name, YearsAppearing,
#'   CountsFemale and CountsMale
#' @param method string passed to binom.confint
#' @param upper.t numeric indicating the minimum upper bound of 
#'   the confidence interval which allows us to classify a name/gender
#' @param lower.t numeric indicating the maximum lower bound of 
#'   the confidence interval acceptable for classification
#' @return A data frame with the same structure but appended columns for
#'   ProbGender (a factor) and Upper and Lower bounds for confidence 
#'   intervals
#'
#' @export
addClassifier <- function(data, method = "wilson", 
                          upper.t = 0.7, lower.t = 0.05) {
  require(binom)
  binom.out <- with(data, binom.confint(CountsMale,
                                        CountsMale + CountsFemale,
                                        method = method))
  # assign as male if we meet the thresholds
  # Because the ratios are essentially symmetric we can assign
  # confidence intervals for both in one pass
  data.pred <- data.frame(ProbGender = with(binom.out,
                                           ifelse(upper > upper.t &
                                                  lower > lower.t,
                                                  "Male", "Female")),
                         Upper = with(binom.out, 
                                      ifelse(mean > 0.5, upper, 1 - lower)),
                         Lower = with(binom.out, 
                                      ifelse(mean > 0.5, lower, 1 - upper)))
# Output structure will look like:
#   ProbGender Upper     Lower
# 1       Male     1 0.8897446
# 2     Female     1 0.6456696
# 3       Male     1 0.5655175
# 4     Female     1 0.5655175
# 5       Male     1 0.9750298
# 6       Male     1 0.9541819
  return(cbind(data, data.pred))
}
