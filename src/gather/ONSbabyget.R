# scraping ONS index pages
library(XML)

# necessary because ONS keeps records as .xls (multi-sheet)
# reading XLS files with gdata requires a perl installation
library(gdata)
# RCurl necessary because read.xls will fail gracelessly 
# on malformed downloads, so we use writeBin
library(RCurl)
# if needed, the path to perl can be set as an argument here
installXLSXsupport()






# index page for data 
ons.base.url <- "http://www.ons.gov.uk"
ons.index <- "rel/vsob1/baby-names--england-and-wales/index.html"
index.doc <- htmlParse(file.path(ons.base.url, ons.index))

# somewhat fragile path to individual data pages

year.pages <- xpathSApply(index.doc, 
                          "//div[@class = 'previous-releases-results']//a", 
                          xmlAttrs)

year.pages <- paste(ons.base.url,
                    year.pages,
                    sep="")

# drop summary page for now
year.pages <- year.pages[!grepl("1904-1994", year.pages)]

tableGet <- function(url) {
  tables <- xpathSApply(htmlParse(url), 
                        "//div[@class = 'srp-pubs-list']//a", 
                        xmlAttrs)
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

wrapXLS <- function(url) {
  # wrap read.xls with a separate download call
  # because read.xls has problems w/ web downloads
  temp.file <- paste(tempfile(), "xls", sep=".")
  
  f <- getBinaryURL(url)
  writeBin(f, temp.file)
  
  sheet.names <- sheetNames(temp.file)
  
  # find which sheet we need to look at as well as
  # which gender are we looking at.
  sheet.loc <- grepl("(Boy|Girl)s names", sheet.names)
  if (any(sheet.loc)) {
    # only will happen if ONS changes structure of xls files
    if (sum(sheet.loc) > 1) {
      stop("too many sheets found")
    }
    sheet.number <- which(sheet.loc)
    gender <- ifelse(grep("Boy", sheet.names[sheet.loc]), "M", "F")
    xls.df <- read.xls(temp.file, sheet=sheet.number, method="csv",
                       skip=2, stringsAsFactors = FALSE)
    xls.df <- xls.df[, names(xls.df)[!grepl("X(\\.?[0-9]*)?", names(xls.df))]]
    xls.df[, "Sex"] <- gender
    xls.df[, "Year"] <- basename(sub("([0-9]{4})/[^/]*$", "\\1", url))
  } else {
    stop("no full sheet found")
  }
  unlink(temp.file)
  closeAllConnections()
  
  return(xls.df)
}

alluk.df <- do.call(rbind, lapply(year.tables, wrapXLS))

# cleanup df

alluk.df[, "Count"] <- as.numeric(gsub(",|\\.|;", "", alluk.df[, "Count"]))
alluk.df <- alluk.df[, c("Name", "Count", "Sex", "Year")]
alluk.df <- alluk.df[complete.cases(alluk.df[, "Count"]), ]

## should use matchsexes from SSAbabyget
## really, those should be loaded first.

alluk.df <- ddply(alluk.df, "Year", function(x) matchSexes(x))

## Scotland

# http://www.gro-scotland.gov.uk/statistics/theme/vital-events/births/popular-names/archive/2009/detailed-tables.html

# http://www.gro-scotland.gov.uk/statistics/theme/vital-events/births/popular-names/archive/2010/detailed-tables.html
