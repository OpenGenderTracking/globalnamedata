#####
###
### Internal utility functions for downloading name data
###
#####

#' Return Locations from Parsed Index
#'
#' Wrapper for xpathSApply and htmlParse
#'
#' @param url A character vector of length 1 specifying the index url
#' @param xpath A character vector of length 1 specifying the xpath query
#'
#' @return A character vector of matched locations
#' @importFrom XML htmlParse xpathSApply xmlAttrs

docsFromIndex <- function(url, xpath) {
  index.doc <- htmlParse(url)
  locations <- xpathSApply(index.doc, xpath, xmlAttrs)
  return(unname(locations))
}

# Condense names to single row. Accepts a single argument, x with the form
#        Name Sex Count Year
# 1      Mary   F  7065 1880
# 2      Anna   F  2604 1880
# 3      Emma   F  2003 1880
# 4 Elizabeth   F  1939 1880
# 5    Minnie   F  1746 1880
# 6  Margaret   F  1578 1880

#' Match names across gender count
#'
#' Construct a pivot table for imput data frames which are in long format
#'
#' @param x A data frames with columns for Name, Sex, Count
#' and Year
#' @param by string denoting common value for data frame (e.g. year or state)
#'
#' @return A single data frame with columns for Name, F, M, and Year
#' @importFrom reshape2 dcast
matchSexes <- function(x) {
  # melt and cast are two broad data handling patterns
  # think of them as the two steps in constructing a
  # pivot table
  
  x.out <- dcast(x[, c("Name", "Sex", "Count")], 
                 Name ~ Sex, sum, value.var = "Count")
  ## x.out structure
  #     Name  F   M
  # 1  Aaron  0 102
  # 2     Ab  0   5
  # 3  Abbie 71   0
  # 4 Abbott  0   5
  # 5   Abby  6   0
  # 6    Abe  0  50

  return(x.out)
}

# Common cleanup functions. Accepts a data frame as an argument with
# columns for Count and Name

cleanupNC <- function(data) {
  capOne <- function(x) {
    paste0(toupper(substring(x, 1, 1)), 
           tolower(substring(x, 2)))
  }
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
  data <- data[grepl("[A-Za-z]+", data[, "Name"]), ]
  data <- data[nchar(data[, "Name"]) > 0, ]
  # Possibly controversial, but datasets mix case and 
  # we want to merge
  data[, "Name"] <- capOne(data[, "Name"])
  rownames(data) <- as.character(1:nrow(data))
  return(data)
}

#' Recursively merge name datasets by summing comparable name counts
#'
#' Function to merge name data from difference countryies, matching by name
#' and summing counts.
#'
#' @param dataframes A list of data frames with columns for Name, F, M, and Year
#' @return A single data frame with columns for Name, F, M, and Year
mergeSum <- function(dataframes) {
  mergeSumSingle <- function(dfx, dfy) {
    m.out <- ddply(merge(dfx, dfy, all = TRUE), 
                   c("Name", "Year"), 
                   function(x) c(F = sum(x[, "F"]), M = sum(x[, "M"])))
    return(m.out)
  }
  return(Reduce(f = mergeSumSingle, x = dataframes))
}

#' Generate uuids for files (which aren'd generated in as tempfiles)
#'
#' Basenames for source urls are not unique enough to avoid collisions
#'
#' @return A string representing a unique uuid

uuid <- function() {
  # From http://stackoverflow.com/a/10493590/1188479
  baseuuid <- paste(sample(c(letters[1:6],0:9),30,replace=TRUE),collapse="")
  uuid <- paste0(substr(baseuuid,1,8), "-",
    substr(baseuuid,9,12), "-", "4",
    substr(baseuuid,13,15), "-",
    sample(c("8","9","a","b"),1),
    substr(baseuuid,16,18), "-",
    substr(baseuuid,19,30),
    collapse=""
  )
  return(uuid)
}


#' Download zipped folder and extract to temp directory
#'
#' SSA releases names as text files in a directory. Downloading these files 
#' can be a bit laborious. See http://www.ssa.gov/oact/babynames/limits.html
#' for details
#'
#' @param url string Location for zip file
#' @param pattern string File name prefix
#' @return A string containing the path to the directory

zipDir <- function(url, pattern = "ssa") {
  # Specify temp directory
  # some environments may pollute the temp directory, so create a new folder
  assets.path <- file.path(tempdir(), "zipout")
  # # Unweildy to stream from url to directory while unzipping
  temp <- tempfile(pattern = pattern, fileext = ".zip")
  # # download and unzip (both invoked for side effects)
  download.file(url, temp)
  dir.create(assets.path, recursive = TRUE)
  unzip(temp, exdir = assets.path)
  # Cleanup temp file and remove explanatory pdf from temp directory
  unlink(c(temp, file.path(assets.path, "*.pdf")))
  return(assets.path)
}



