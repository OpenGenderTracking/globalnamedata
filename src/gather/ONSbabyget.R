library(XML)

# necessary because ONS keeps records as .xls (multi-sheet)
library(gdata)

library(RCurl)

# UK

installXLSXsupport()

ons.base.url <- "http://www.ons.gov.uk"

# index page for data 

index.doc <- htmlParse(file.path(ons.base.url, "rel/vsob1/baby-names--england-and-wales/index.html"))

# somewhat fragile path to individual data pages

year.pages <- paste(ons.base.url, 
                    xpathSApply(index.doc, "//div[@class = 'previous-releases-results']//a", 
                                xmlAttrs), sep="")

# drop summary page for now
year.pages <- year.pages[!grepl("1904-1994", year.pages)]

tableGet <- function(url) {
  tables <- xpathSApply(htmlParse(url), "//div[@class = 'srp-pubs-list']//a", xmlAttrs)
  tables <- tables[grepl("reference", tables)]
  excel.out <- xpathSApply(htmlParse(paste(ons.base.url, tables, sep="")),
                           "//div[@class = 'download-options']//a[@class  = 'xls']",
                           xmlAttrs)
  # the class and the href are both attributes
  return(excel.out[1, ])
}

year.tables <- sapply(year.pages, tableGet)

year.tables <- paste(ons.base.url, year.tables, sep="")

# drop columns which are auto-named

wrapXLS <- function(url, sheet = 7) {
  # wrap read.xls with a separate download call
  # because read.xls has problems w/ web downloads
  temp.dir <- tempdir()
  temp.file <- paste(tempfile(), "xls", sep=".")
  
  f <- getBinaryURL(url)
  writeBin(f, temp.file)
  
  con <- xls2csv(temp.file)
  sheet.names <- sheetNames(temp.file)
  
  # find which sheet we need to look at as well as
  # which gender are we looking at.
  
  pattern.boys.names <- 'Boys names - E&W'
  pattern.girls.names <- 'Girls names - E&W'

  gender <- 'U'
  sheet.number <- grep(pattern.boys.names, sheet.names)
  if (!is.null(sheet.number)) {
    gender <- 'M'
  } else {
    sheet.number <- grep(pattern.girls.names, sheet.names)
    if (!is.null(sheet.number)) {
      gender <- 'F'
    }
  }
  
  # read the sheet in question
  xls.df <- read.xls(temp.file, sheet=sheet.number, method="csv", skip=2, stringsAsFactors = FALSE)
  
  # drop columns with no content
  xls.df <- xls.df[, names(xls.df)[!grepl("X(\\.?[0-9]*)?", names(xls.df))]]
  
  # add a year based on the file name
  xls.df[, "Year"] <- basename(sub("([0-9]{4})/[^/]*$", "\\1", url))
  
  # set the gender
  xls.df[, "Sex"] <- gender
  
  unlink(temp.dir)
  
  return(xls.df)
}

alluk.df <- do.call(rbind, lapply(year.tables, wrapXLS))

matchSexes <- function(x) {
  # melt and cast are two broad data handling patterns
  # think of them as the two steps in constructing a
  # pivot table:
  #        Name Sex variable value
  # 1      Mary   F    Count  7065
  # 2      Anna   F    Count  2604
  # 3      Emma   F    Count  2003
  # 4 Elizabeth   F    Count  1939
  # 5    Minnie   F    Count  1746
  # 6  Margaret   F    Count  1578
  x.melt <- melt(x, id.vars = c("Name", "Sex"), measure.vars = "Count")
  
  ##
  #     Name  F   M
  # 1  Aaron  0 102
  # 2     Ab  0   5
  # 3  Abbie 71   0
  # 4 Abbott  0   5
  # 5   Abby  6   0
  # 6    Abe  0  50
  x.out <- dcast(x.melt, Name ~ Sex, sum)
  
  x.out[, "Name"] <- as.character(x.out[, "Name"])
  
  # Add a year column. Year is the same for all rows since
  # names are grouped per file per year.
  # Faster/safer than unique(x[, "Year"])
  # no easy way to extract from the passed argument
  x.out[, "Year"] <- x[, "Year"][1]
  
  return(x.out)
}



# sheet names!

# TODO use wget for download method 

# sheetNames("test2.xls")
## Scotland

# http://www.gro-scotland.gov.uk/statistics/theme/vital-events/births/popular-names/archive/2009/detailed-tables.html

# http://www.gro-scotland.gov.uk/statistics/theme/vital-events/births/popular-names/archive/2010/detailed-tables.html
