#####
###
### External Utility functions for handling name data
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
  # count occurences of name by gender-year and by year only
  countYears <- function(data, gender) {
    # restrict to gender passed by argument
    years <- count(data[data[, gender] > 0, ], vars = c("Name", gender))
    # count gender-year
    name.count <- with(years, rowsum(freq * get(gender), Name, reorder = FALSE))
    # year only
    year.count <- with(years, rowsum(freq, Name, reorder = FALSE))
    year.df <- data.frame(Name = rownames(year.count), 
                          count = name.count,
                          Appearances = year.count,
                          stringsAsFactors = FALSE)
    names(year.df) <- c("Name", 
                        paste0("Count", substitute(gender)),
                        paste0("Sumyears", substitute(gender)))
    
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
                       "count.female", "count.male")
  prop.male <- with(data.out, count.male / (count.male + count.female))
  # return the proportion here 
  data.out[, "prop.male"] <- prop.male
  return(data.out)
}

#' Count Birth Totals
#'
#' Given a data frame and an optional range of years, return 
#' a data frame of birth totals for those years
#'
#' @param data A data frames with columns for Name, M, F, and Year
#' e.g. one returned by \code{\link{usnames}}
#' @param range A numeric vector of length 2 with the start and end years
#' inclusive
#' @return A single data frame with columns for Births, Gender and Year
#' in "long" format a la \code{\link{reshape}}
#' @export 

yearBirths <- function(data, bounds = NULL) {
  countBy <- function(x = c("Male", "Female")) {
                      # Contingency table for births
                      births <- with(data, rowsum(get(x), group = Year))
                      out <- data.frame(Year = as.numeric(rownames(births)),
                                        Births = unname(births),
                                        Gender = match.arg(x))
                      return(out)
                    }
  data.out <- rbind(countBy("M"), countBy("F"))
  # bounds checking if provided
  if (length(bounds) == 2) {
    data.out <- subset(data.out, Year %in% do.call(seq, as.list(bounds)))
  }
  return(data.out)
}



#' Compute gender balance
#'
#' Determine the per-year gender breakdown for names anchored to 
#' male, female or neutral
#'
#' @param data A data frames with columns for Name, M, F, and Year
#' e.g. one returned by \code{\link{usnames}}
#' @param names A character vector of names (potentially of length 1)
#' @param range An (optional) numeric vector of length 2 
#' with the start and end years inclusive
#' @param metric Male, Female or Neutral
#' @return A single data frame with columns for the metric, number of births
#' the years and the name. 
#' @export
nameMetric <- function(data, names, bounds = NULL, metric) {
  data <- subset(data, Name %in% names)
  nameTotal <- function(name.single) {
    singleton <- subset(data, Name == name.single)
    tot <- with(singleton, rowsum((M + F), group = Year))
    metFun <- switch(metric,
                     Male = singleton[, "M"] / tot,
                     Female = singleton[, "F"] / tot,
                     Neutral = 1 - abs(0.5 - singleton[, "M"] / tot) * 2
                    )
    single.df <- data.frame(Proportion = metFun,
                            Metric = metric,
                            Births = tot,
                            Year = as.numeric(rownames(tot)),
                            Name = name.single)
    return(single.df)
  }
  if (length(names) > 1) {
    names.list <- lapply(names, nameTotal)
    data.out <- do.call(rbind, names.list)
  } else {
    data.out <- nameTotal(names)
  }
  if (length(bounds) == 2) {
    data.out <- subset(data.out, Year %in% do.call(seq, as.list(bounds)))
  }
  return(unrowname(data.out))
}