library(XML)

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
  return(excel.out)
}

year.tables <- sapply(year.pages, tableGet)

# names(year.tables) <- basename(sub("([0-9]{4})/.*", "\\1", names(year.tables)))

year.tables <- paste0(ons.base.url, year.tables)



## Scotland

# http://www.gro-scotland.gov.uk/statistics/theme/vital-events/births/popular-names/archive/2009/detailed-tables.html

# http://www.gro-scotland.gov.uk/statistics/theme/vital-events/births/popular-names/archive/2010/detailed-tables.html
