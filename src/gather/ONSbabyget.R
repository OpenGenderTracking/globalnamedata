library(XML)

# necessary because ONS keeps records as .xls (multi-sheet)
library(gdata)
# UK

ons.base.url <- "http://www.ons.gov.uk"

# index page for data 
paste0(ons.base.url, "/ons/rel/vsob1/baby-names--england-and-wales/index.html")

index.doc <- htmlParse(file.path(ons.base.url, "rel/vsob1/baby-names--england-and-wales/index.html"))


# somewhat fragile path to individual data pages
xpathSApply(index.doc, "//div[@class = 'previous-releases-results']//a", xmlAttrs)

year.pages <- paste0(ons.base.url, xpathSApply(index.doc, "//div[@class = 'previous-releases-results']//a", xmlAttrs))

# drop summary page for now
year.pages <- year.pages[!grepl("1904-1994", year.pages)]

tableGet <- function(url) {
  tables <- xpathSApply(htmlParse(url), "//div[@class = 'srp-pubs-list']//a", xmlAttrs)
  tables <- tables[grepl("reference", tables)]
  excel.out <- xpathSApply(htmlParse(paste0(ons.base.url, tables)),
                           "//div[@class = 'download-options']//a[@class  = 'xls']",
                           xmlAttrs)
  # the class and the href are both attributes
  return(excel.out[1, ])
}

year.tables <- sapply(year.pages, tableGet)

year.tables <- paste0(ons.base.url, year.tables)

# drop columns which are auto-named

wrapXLS <- function(url, sheet = 7) {
  # wrap read.xls with a separate download call
  # because read.xls has problems w/ web downloads
  temp <- tempdir()
  download.file(url, file.path(temp, basename(url)),
                cacheOK = FALSE, quiet = TRUE)
  browser()
  xls.df <- read.xls(file.path(temp, basename(url)), sheet = sheet, 
                     stringsAsFactors = FALSE, skip = 2)
  unlink(temp)
  # drop columns with no content
  xls.df <- xls.df[, names(xls.df)[!grepl("X(\\.?[0-9]*)?", names(xls.df))]]
  xls.df[, "Year"] <- basename(sub("([0-9]{4})/[^/]*$", "\\1", url))
  return(xls.df)
}

alluk.df <- do.call(rbind, lapply(year.tables, wrapXLS))

## Scotland

# http://www.gro-scotland.gov.uk/statistics/theme/vital-events/births/popular-names/archive/2009/detailed-tables.html

# http://www.gro-scotland.gov.uk/statistics/theme/vital-events/births/popular-names/archive/2010/detailed-tables.html
