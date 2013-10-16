#####
###
### Gender likelihood for names
###
#####

#' Read name data formatted as gender-year combinations and 
#'   accumulates total count per name
#'
#' Designed to be fast and flexible, allowing for rapid testing
#'
#' @param data data frame with columns for Name, years.appearing,
#'   count.female and count.male
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
  base.cols <- c("Name", "years.appearing","count.female", "count.male")
  # drop columns from other analyses
  data <- data[, names(data) %in% base.cols]

  pred <- with(data, binom.confint(count.male, count.male + count.female,
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
  gender.observed <- with(data, (count.male / (count.male + count.female)))
  # Because the ratios are essentially symmetric we can assign
  # confidence intervals for both in one pass
  gender.upper <- with(pred, ifelse(mean > 0.5, upper, 1 - lower))
  gender.lower <- with(pred, ifelse(mean > 0.5, lower, 1 - upper))

  data.pred <- data.frame(prob.gender = gender.prediction,
                          obs.male = pred[, "mean"],
                          est.male = gender.observed,
                          upper = gender.upper,
                          lower = gender.lower)
  return(cbind(data, data.pred))
}
