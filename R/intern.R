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
  ## Condense names to single row. Accepts a single argument, x with the form
  #        Name Sex Count Year
  # 1      Mary   F  7065 1880
  # 2      Anna   F  2604 1880
  # 3      Emma   F  2003 1880
  # 4 Elizabeth   F  1939 1880
  
  ## dcast helps build a pivot table based on names
  # for each name/sex combo, we compute the sum of births and accumulate
  x.out <- dcast(
    x[, c("Name", "Sex", "Count")], 
    Name ~ Sex, sum,
    value.var = "Count"
  )

  ## x.out structure
  #     Name  F   M
  # 1  Aaron  0 102
  # 2     Ab  0   5
  # 3  Abbie 71   0
  # 4 Abbott  0   5

  return(x.out)
}

# Common cleanup functions. Accepts a data frame as an argument with
# columns for Count and Name

cleanupNC <- function(data) {
  capOne <- function(x) {
    paste0(
      toupper(substring(x, 1, 1)), 
      tolower(substring(x, 2))
    )
  }
  ## Count
  data[, "Count"] <- gsub(",|\\.+|;|\\s+", "", data[, "Count"])
  # remove rows without all numeric values
  data <- data[grepl("^[0-9]+$", data[, "Count"]), ]
  data[, "Count"] <- as.numeric(data[, "Count"])

  ## Name
  # NISRA and Scotland return some multi-byte characters
  if (any(is.na(nchar(data[, "Name"], allowNA = TRUE)))) {
    data[, "Name"] <- iconv(
      data[, "Name"],
      from = "latin1",
      to = "UTF-8"
    )
  }
  # trim trailing and leading space
  data[, "Name"] <- gsub("^\\s+|\\s+$", "", data[, "Name"])
  # remove rows without letter characters
  data <- data[grepl("[A-Za-z]+", data[, "Name"]), ]
  # Remove 0 length names
  data <- data[nchar(data[, "Name"]) > 0, ]
  # Possibly controversial, but datasets mix case and 
  # we want to merge
  data[, "Name"] <- capOne(data[, "Name"])
  rownames(data) <- NULL
  return(data)
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
  
  # download and unzip (both invoked for side effects)
  download.file(url, temp)
  dir.create(assets.path, recursive = TRUE)
  unzip(temp, exdir = assets.path)

  # Cleanup temp file and remove explanatory pdf from temp directory
  unlink(c(temp, file.path(assets.path, "*.pdf")))
  return(assets.path)
}



