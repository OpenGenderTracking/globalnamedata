#####
###
### functions for gender computation
###
#####

# turn counts into imputed probability of a name being male
# or female. Very basic at the moment

probProcess <- function(data) {
  require(plyr)
  # structure will look like this:
  
  #     Name  F   M Year
  # 1  Aaron  0 102 1880
  # 2     Ab  0   5 1880
  # 3  Abbie 71   0 1880
  # 4 Abbott  0   5 1880
  # 5   Abby  6   0 1880
  # 6    Abe  0  50 1880
  
  # Handling years now, so we convert it to numeric 
  data[, "Year"] <- as.numeric(data[, "Year"])
  
  # Compute the bare proportion of female names
  # ddply not necessary, since we're not changing the mapping
  data[, "PropFemale"] <- with(data, F/(F + M))
  # Male is just 1 - PropFemale
  data[, "PropMale"] <- 1 - data[, "PropFemale"]
  
  ## in practical terms very few names in a given year are ambiguous
  ## roughly 10% are not 1 or 0 and > 40% of those are 0-0.1 or 0.8-1
  ## There is still some value to retaining this information
  
  ## Sums up the proportion of female (male) names 
  
  # count the number of total occurances per gender as well
  countYears <- function(data, sex) {
    years <- count(data[data[, sex] > 0, ], vars = c("Name", sex))
    name.counts <- with(years, rowsum(freq * get(sex), Name, reorder = FALSE))
    year.counts <- with(years, rowsum(freq, Name, reorder = FALSE))
    year.df <- data.frame(Name = rownames(year.counts), 
                          Counts = name.counts,
                          Appearances = year.counts,
                          stringsAsFactors = FALSE)
    names(year.df) <- c("Name", 
                        paste0("Count", substitute(sex)),
                        paste0("Sumyears", substitute(sex)))
    rownames(year.df) <- as.character(1:nrow(year.df))
    return(year.df)
  }

  # populate name column quickly 
  data.out <- data.frame(Name = sort(unique(data[, "Name"])),
                         freq = count(data, "Name")[, 2], 
                         stringsAsFactors = FALSE)
  
  data.out <- cbind(data.out,  merge(countYears(data, "M"), 
                                    countYears(data, "F"), 
                                    by = "Name", all = TRUE)[, -1])

  # Counts appearances by years (used to normalize the result)
  # This is a bit of spaghetti code, but it's our normalization  
  data.out[, "PropFemale"] <- data.out[, "SumyearsF"] / data.out[, "freq"]
  data.out[, "PropMale"] <- data.out[, "SumyearsM"] / data.out[, "freq"]

  # cleanup the final df
  data.out[is.na(data.out)] <- 0
  # drops intermediate columns and rename
  data.out <- data.out[, c("Name", "freq", 
                           "PropFemale", "CountF",
                           "PropMale", "CountM")]
  names(data.out) <- c("Name", "YearsAppearing", 
                       "PropFemale", "CountsFemale",
                       "PropMale", "CountsMale")
  data.out[, "Name"] <- as.character(data.out[, "Name"])
  return(data.out)
}
