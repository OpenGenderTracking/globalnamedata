########
###
### Packages and utility functions for downloading name data
###
########

##
## General dependencies
## Dependencies which apply to only one source will be loaded
## in that source script
##


# Plyr and reshape needed to produce pivot tables and combine
# data frames

library(plyr)
library(reshape2)


# RCurl necessary because read.xls will fail gracelessly 
# on malformed downloads, so we use writeBin

library(RCurl)

##
## Utility functions
##

# Condense names to single row
# Many sources include male and female names 
# in different locations

matchSexes <- function(x) {
  # melt and cast are two broad data handling patterns
  # think of them as the two steps in constructing a
  # pivot table:
  
  # x is of the following form:
  #        Name Sex Count Year
  # 1      Mary   F  7065 1880
  # 2      Anna   F  2604 1880
  # 3      Emma   F  2003 1880
  # 4 Elizabeth   F  1939 1880
  # 5    Minnie   F  1746 1880
  # 6  Margaret   F  1578 1880
  
  x.out <- dcast(x[, c("Name", "Sex", "Count")], Name ~ Sex, sum, value.var = "Count")
  ## x.out structure
  #     Name  F   M
  # 1  Aaron  0 102
  # 2     Ab  0   5
  # 3  Abbie 71   0
  # 4 Abbott  0   5
  # 5   Abby  6   0
  # 6    Abe  0  50

  # Add a year column. Year is the same for all rows since
  # names are grouped per file per year.
  # Faster/safer than unique(x[, "Year"])
  # no easy way to extract from the passed argument
  x.out[, "Year"] <- x[, "Year"][1]
  
  return(x.out)
}

## Download Excel files with RCurl
## return a character vector for the temp file created
##

downloadXLS <- function(url, pattern) {
  
  # Download files as binary and push to a temp directory
  # because read.xls has problems w/ web downloads
  temp.file <- tempfile(pattern = pattern, fileext = ".xls")
  f <- getBinaryURL(url)
  writeBin(f, temp.file)
  return(temp.file)
}

# Common cleanup functions

cleanupNC <- function(data) {
  ## Count
  data[, "Count"] <- gsub(",|\\.+|;|\\s+", "", data[, "Count"])
  # remove rows
  data <- data[grepl("^[0-9]+$", data[, "Count"]), ]
  data[, "Count"] <- as.numeric(data[, "Count"])

  ## Name
  # NISRA and Scotland return some multi-byte characters
  if (any(is.na(nchar(data[, "Name"], allowNA = TRUE)))) {
    data[, "Name"] <- iconv(data[, "Name"],
                            from = "latin1",
                            to = "UTF-8")
  }
  data[, "Name"] <- gsub("^\\s+|\\s+$", "", data[, "Name"])
  data <- data[nchar(data[, "Name"]) > 0, ]
  rownames(data) <- as.character(1:nrow(data))
  return(data)
}


# turn counts into imputed probability of a name being male
# or female. Very basic at the moment

probProcess <- function(data) {
  # structure will look like this:
  
  #     Name  F   M Year
  # 1  Aaron  0 102 1880
  # 2     Ab  0   5 1880
  # 3  Abbie 71   0 1880
  # 4 Abbott  0   5 1880
  # 5   Abby  6   0 1880
  # 6    Abe  0  50 1880
  
  # Each name is now associated w/ a *single* row
  # so we only need to look up one "key" as it were
  
  
  # Handling years now, so we convert it to numeric 
  data[, "Year"] <- as.numeric(data[, "Year"])
  
  # Nate's year logic from https://gist.github.com/natematias/4743564
  # used for consistency
  
  nateMod <- function(x, penalty = 100) {
    out <- x
    # hahahaha That's awesome. This is
    # actually handy http://rmazing.wordpress.com/2013/01/30/the-magic-empty-bracket/
    out[] <- 1
    # unless I'm reading it wrong, there's a 
    # bug in the ruby script. Should be 1 + ...
    # 1 - ... weights old/new years more than 1960-1980
    
    # Post 1980 names shouldn't be penalized b/c we want to
    # capture m/f dynamics in new 1st gen american names
    
    # out[x > 1980] <- 1 + (1960 - x[x > 1980])/penalty
    out[x < 1960] <- 1 + (x[x < 1960] - 1980)/penalty
    return(out)
  }
  
  data[, "YearModifier"] <- nateMod(data[, "Year"])
  
  # Compute the bare proportion of female names
  # ddply not necessary, since we're not changing the mapping
  
  data[, "PropF"] <- with(data, F/(F + M))
  
  # Male is just 1 - PropF
  
  data[, "PropM"] <- 1 - data[, "PropF"]
  
  ## in practical terms very few names in a given year are ambiguous
  ## roughly 10% are not 1 or 0 and > 40% of those are 0-0.1 or 0.8-1
  ## There is still some value to retaining this information
  
  ## Sums up the proportion of female (male) names 
  
  # populate name column quickly 
  data.out <- data.frame(Name = sort(unique(data[, "Name"])), stringsAsFactors = FALSE)
  
  # MUCH faster than ddply or tapply
  data.out[, "SumPropF"] <- with(data, rowsum(PropF*YearModifier, Name))
  data.out[, "SumPropM"] <- with(data, rowsum(PropM*YearModifier, Name))
  
  # Counts appearances by years (used to normalize the result)
  data.out <- merge(data.out, count(data, "Name"), by = "Name")
  
  # This is a bit of spaghetti code, but it's our normalization  
  data.out[, "ImputedProbF"] <- data.out[, "SumPropF"] / data.out[, "freq"]
  data.out[, "ImputedProbM"] <- data.out[, "SumPropM"] / data.out[, "freq"]
  
  # cleanup the final df
  data.out <- data.out[, c("Name", "freq", 
                           "ImputedProbF",
                           "ImputedProbM")]
  
  names(data.out)[2] <- "YearsAppearing"
  
  # Output structure looks like:
  
  #        Name YearsAppearing AnyFemale ImputedProbF AnyMale ImputedProbM
  # 1     Aaban              4     FALSE            0    TRUE            1
  # 2     Aabha              1      TRUE            1   FALSE            0
  # 3     Aabid              1     FALSE            0    TRUE            1
  # 4 Aabriella              1      TRUE            1   FALSE            0
  # 5     Aadam             20     FALSE            0    TRUE            1
  # 6     Aadan              6     FALSE            0    TRUE            1
  return(data.out)
}





