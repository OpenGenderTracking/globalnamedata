#####
###
### External utility functions for handling name data
###
#####

#' Read name data formatted as gender-year combinations and 
#'   accumulates total counts per name
#'
#' Return total counts and years appearing to offer multiple ways to gauge
#' likelihood of a match.
#'
#' @param data A data frame with columns for Name, F, M, and Year
#' @return Data frame with columns for Name, count for 
#'   gender incidence and year count
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

  # Cache factor of names
  name.factor <- as.factor(data[, "Name"])

  # Group by year/name combination, return the unique year rows
  # prevents double counting for years for M and F in the same year
  years.appearing <- tapply(data[, "Year"], name.factor,
    function(x) {
      length(unique(x))
    }
  )

  data.out <- data.frame(
    Name = rownames(years.appearing),
    years.appearing = as.numeric(years.appearing),
    # Sum the counts for each level in the name factor (every name)
    count.male = as.numeric(rowsum(data[, "M"], group = name.factor)),
    count.female = as.numeric(rowsum(data[, "F"], group = name.factor)),
    stringsAsFactors = FALSE
  )

  return(data.out)
}

#' Recursively merge name datasets by summing comparable name counts
#'
#' Function to merge name data from difference countryies, matching by name
#' and summing counts.
#'
#' @param dataframes A list of data frames with columns for Name, F, M, and Year
#' @return A single data frame with columns for Name, F, M, and Year
#' @export

mergeSum <- function(dataframes) {
  mergeSumSingle <- function(dfx, dfy) {
    m.out <- ddply(
      merge(dfx, dfy, all = TRUE), 
      c("Name", "Year"), 
        function(x) c(F = sum(x[, "F"]), M = sum(x[, "M"])
      )
    )
    return(m.out)
  }
  return(Reduce(f = mergeSumSingle, x = dataframes))
}

#' Compute gender balance
#'
#' Determine the per-year gender breakdown for names anchored to 
#' male, female or neutral
#'
#' @param data A data frames with columns for Name, M, F, and Year
#' e.g. one returned by \code{\link{usnames}}
#' @param names A character vector of names (potentially of length 1)
#' @param bounds An (optional) numeric vector of length 2 
#' with the start and end years inclusive
#' @param metric A character vector of length 1. "Male", "Female" or "Neutral"
#' @return A single data frame with columns for the metric, number of births
#' the years and the name. 
#' @export
nameMetric <- function(data, names, bounds = NULL, metric) {
  matches <- data[, "Name"] %in% names
  if(all(matches == 0)) {
    stop("Names not found")
  }
  data <- data[matches, ]

  metric <- match.arg(metric, choices = c("Male", "Female", "Neutral"))


  nameTotal <- function(name.single) {
    singleton <- data[data[, "Name"] == name.single, ]
    tot <- with(singleton,
      rowsum((M + F), group = Year)
    )
    metFun <- switch(metric,
       Male = singleton[, "M"] / tot,
       Female = singleton[, "F"] / tot,
       Neutral = 1 - abs(0.5 - singleton[, "M"] / tot) * 2
    )
    single.df <- data.frame(
      Name = name.single,
      Births = tot,
      Year = as.numeric(rownames(tot)),
      Metric = metric,
      Proportion = metFun
    )
    return(single.df)
  }
  names.list <- lapply(names, nameTotal)
  data.out <- do.call(rbind, names.list)

  if (length(bounds) == 2) {
    data.out <- subset(data.out, Year %in% do.call(seq, as.list(bounds)))
  }
  return(unrowname(data.out))
}