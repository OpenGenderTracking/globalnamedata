
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
  prop.male <- with(data.out, counts.male /(counts.male + counts.female))
  # return the proportion here 
  data.out[, "prop.male"] <- prop.male
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
#' @param threshold numeric between 0.5 and 1 indicating the minimum proportion
#'   of male names required to classify a name as likely male. If threshold is 
#'   less than 0.5 it will be used as minimum proportion of female names
#' @param ... Additional arguments to be passed to \code{\link{binom.confint}}
#'
#' @return A data frame with the same structure but appended columns for
#'   prob.gender (a factor), estimated proportion and upper and lower bounds 
#'   for confidence intervals
#'
#' @export
#' @importFrom binom binom.confint
nameBinom <- function(data, method = "ac", 
                      threshold = 0.9, ...) {
  # drop columns from other analyses
  input.cols <- !names(data) %in% c("prob.gender", "upper", "lower", "est.male")
  data <- data[, input.cols]

  pred <- with(data, binom.confint(counts.male, counts.male + counts.female,
                                   method = method, ...))

  # assign as male/female if we meet the thresholds
  procThreshold <- function(input, threshold) {
    if (threshold < 0.5) {
      treshold <- 1 - threshold
    }
    out.val <- rep("Unknown", times = length(input))
    out.val <- ifelse(input > threshold, 
                      "Male", ifelse(input < 1 - threshold,
                                     "Female", out.val))
    return(out.val)
  }

  gender.prediction <- procThreshold(pred[, "mean"], threshold)
  # Because the ratios are essentially symmetric we can assign
  # confidence intervals for both in one pass
  gender.upper <- with(pred, ifelse(mean > 0.5, upper, 1 - lower))
  gender.lower <- with(pred, ifelse(mean > 0.5, lower, 1 - upper))

  data.pred <- data.frame(prob.gender = gender.prediction,
                          est.male = pred[, "mean"],
                          upper = gender.upper,
                          lower = gender.lower)
  return(cbind(data, data.pred))
}
