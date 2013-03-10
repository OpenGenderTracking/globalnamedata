#####
# Read names from the UK's Office of National Statistics (ONS)
#
# Names are included in Excel spreadsheets broken up by gender
# and year. Links to those spreadsheets are gathered from 
# the data navigator provided by ONS
#####

# scraping ONS index pages
library(XML)

# necessary because ONS keeps records as .xls (multi-sheet)
# reading XLS files with gdata requires a perl installation
library(gdata)

# if needed, the path to perl can be set as an argument here
if (length(xlsFormats()) != 2) {
  installXLSXsupport()
}



readONSNames <- function() {

  ons.base.url <- "http://www.ons.gov.uk"
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


  year.pages <- indexGet()


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

  year.tables <- paste0(ons.base.url, year.tables)

  # drop columns which are auto-named

  wrapXLS <- function(url) {
    
    temp.file <- downloadXLS(url, pattern = "ONS")

    # find which sheet we need to look at as well as
    # which gender are we looking at.
    sheet.names <- sheetNames(temp.file)
    sheet.loc <- grepl("(Boy|Girl)s names", sheet.names)
    if (any(sheet.loc)) {
      # only will happen if ONS changes structure of xls files
      if (sum(sheet.loc) > 1) {
        stop("too many sheets found")
      }
      sheet.number <- which(sheet.loc)

      xls.df <- read.xls(temp.file, sheet = sheet.number, method = "csv",
                         skip = 2, stringsAsFactors = FALSE)
      xls.df <- xls.df[, names(xls.df)[!grepl("X(\\.?[0-9]*)?", names(xls.df))]]
      xls.df[, "Sex"] <- ifelse(grepl("Boy", sheet.names[sheet.loc]), "M", "F")
      xls.df[, "Year"] <- basename(sub("([0-9]{4})/[^/]*$", "\\1", url))
    } else {
      stop("no full sheet found")
    }

    unlink(temp.file)
    closeAllConnections()

    # cleanup df
    xls.df <- xls.df[, c("Name", "Count", "Sex", "Year")]
    
    xls.df <- cleanupNC(xls.df)
    xls.df[, "Year"] <- as.numeric(xls.df[, "Year"])

    return(xls.df)
  }

  alluk.df <- do.call(rbind, lapply(year.tables, wrapXLS))

  alluk.df <- ddply(alluk.df, "Year", function(x) matchSexes(x))
  return(alluk.df)
}

# output

ew.df <- readONSNames()


