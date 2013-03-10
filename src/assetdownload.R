## XLS download utility function

downloadXLS <- function(url, pattern) {
  
  # Download files as binary and push to a temp directory
  # because read.xls has problems w/ web downloads
  temp.file <- tempfile(pattern = pattern, fileext = ".xls")
  f <- getBinaryURL(url)
  writeBin(f, temp.file)
  return(temp.file)
}

## SSA Download
downloadSSA <- function() {
  assets.path <- file.path(getwd(), "assets", "us")
  temp <- tempfile(pattern = "ssa", fileext = ".zip")
  # download and unzip
  # See http://www.ssa.gov/oact/babynames/limits.html for info 
  download.file('http://www.ssa.gov/oact/babynames/names.zip', temp)
  unzip(temp, exdir = assets.path)
  unlink(temp, file.path(getwd(), "assets", "us", "*.pdf"))
  closeAllConnections()
}

## Scotland Dowload
downloadScotland <- function() {
  gro.base <- "http://www.gro-scotland.gov.uk"
  
  gro.files <- c("files2/stats/popular-forenames/babiesnames09-table4.csv", 
                 "files2/stats/popular-forenames/babiesnames2010-table4.csv")
  assets.path <- file.path(getwd(), "assets", "scotlandgro")
  dlname <- function(url) {
    year <- paste0("20", gsub("[a-z]*(?:[0-9]{2})?([0-9]{2}).*$", "\\1", basename(url)))
    download.file(url,
                  destfile = file.path(assets.path,
                                       paste0("gro", year, ".csv")))
  }
  lapply(paste(gro.base, gro.files, sep = "/"), dlname)
  closeAllConnections()
}




## Nisra download

downloadNISRA <- function() {
  indexGet <- function(index.url) {
    index.doc <- htmlParse(index.url)
    # NISRA links to baby names from their index
    archive <- xpathSApply(index.doc, 
                           "//a[contains(text(), 'Full Baby Names')]", 
                           xmlAttrs)
    return(unname(archive))
  }
  assets.path <- file.path(getwd(), "assets", "nisra")
  nisra.index <- "http://www.nisra.gov.uk/demography/default.asp28.htm"
  url <- indexGet(nisra.index)
  f <- getBinaryURL(url)
  writeBin(f, file.path(assets.path, "nisra.xls"))
  closeAllConnections()
}


## ONS download

downloadONS <- function() {
  # somewhat fragile path to individual data pages
  indexGet <- function() {
    # index page for data 
    ons.index <- "rel/vsob1/baby-names--england-and-wales/index.html"
    index.doc <- htmlParse(file.path(ons.base.url, ons.index))
    year.pages <- xpathSApply(index.doc, 
                              "//div[@class = 'previous-releases-results']//a", 
                              xmlAttrs)
    
    year.pages <- paste(ons.base.url,
                        year.pages,
                        sep="")
    # drop summary page for now
    year.pages <- year.pages[!grepl("1904-1994", year.pages)]
    return(year.pages)
  }
  
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
  ons.base.url <- "http://www.ons.gov.uk/ons"
  year.pages <- indexGet()
  year.tables <- sapply(year.pages, tableGet)
  year.tables <- paste0(ons.base.url, year.tables)
  
  assets.path <- file.path(getwd(), "assets", "ons")
  dlname <- function(url) {
    f <- getBinaryURL(url)
    writeBin(f, 
             file.path(assets.path,basename(url)))
  }
  lapply(year.tables, dlname)
  closeAllConnections()
}

