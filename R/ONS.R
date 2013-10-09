
#' Read and return name data for England and Wales from the
#'   Office of National Statistics
#'
#' Download data from the ONS website and convert into a single data frame
#' Downloading and converting data will take some time. The resultant dataset is
#' provided as \code{\link{ewnames}}
#'
#' @return Data frame with columns for Name, Year, and counts for 
#'   gender incidence
#' @keywords england
#' @seealso \code{\link{readScotlandNames}}, \code{\link{readNISRANames}}, 
#'   \code{\link{readSSANames}}
#' @export
#' @importFrom RCurl getBinaryURL
#' @importFrom XML htmlParse xpathSApply xmlAttrs
readONSNames <- function() {
  # if needed, the path to perl can be set as an argument here
  if (length(xlsFormats()) != 2) {
    installXLSXsupport()
  }

  ## ONS download
  downloadONS <- function() {
    ons.base.url <- "http://www.ons.gov.uk/ons"
    ons.index <- "rel/vsob1/baby-names--england-and-wales/index.html"
    xpath.release <- "//div[@class = 'previous-releases-results']//a"
    xpath.xls <- "//a[contains(@href, 'xls')]"
    table.selector <- "//div[@class = 'srp-pubs-list']//a"
    summary.years <- "1904-1994"
    # somewhat fragile path to individual data pages
    indexGet <- function() {
      # index page for data 
      
      index.doc <- htmlParse(file.path(ons.base.url, ons.index))
      
      year.pages <- xpathSApply(index.doc, xpath.release, xmlAttrs)
      
      year.pages <- paste0(ons.base.url, year.pages)
      # drop summary page for now
      year.pages <- year.pages[!grepl(summary.years, year.pages)]
      return(year.pages)
    }
    
    tableGet <- function(url, filter = "reference") {
      tables <- xpathSApply(htmlParse(url), 
                            table.selector, 
                            xmlAttrs)
      tables <- tables[grepl(filter, tables)]
      
      excel.out <- xpathSApply(htmlParse(paste0(ons.base.url, tables)),
                               xpath.xls,
                               xmlAttrs)
      attributes(excel.out) <- NULL
      return(excel.out)
    }
    
    assets.path <- file.path(tempdir(), "assets", "ons")
    dir.create(assets.path, recursive = TRUE)
    year.pages <- indexGet()
    # call tableGet for each year linked in the index
    # flatten output further
    year.tables <- paste0(ons.base.url, sapply(year.pages, tableGet))

    # download function passed to lapply because
    # we have multiple excel files from a single index
    dlname <- function(url) {
      # Generate UUID for files, avoids file name collisions for 
      # identical basenames
      baseuuid <- paste(sample(c(letters[1:6],0:9),30,replace=TRUE),collapse="")
      fname <- paste0(substr(baseuuid,1,8), "-",
        substr(baseuuid,9,12), "-", "4",
        substr(baseuuid,13,15), "-",
        sample(c("8","9","a","b"),1),
        substr(baseuuid,16,18), "-",
        substr(baseuuid,19,30),
        collapse=""
      )
      writeBin(getBinaryURL(url), file.path(assets.path, fname))
    }
    lapply(year.tables, dlname)
    return(assets.path)
  }
  # reading excel files and converting into tractable form
  # wrapped into a function because ONS stores many excel 
  # sheets (2 per year)
  wrapXLS <- function(file) {
    year.regex <- "[0-9]{4}"
    # find which sheet we need to look at as well as
    # which gender are we looking at.
    sheet.names <- sheetNames(file)
    sheet.loc <- grepl("(Boy|Girl)([^ ]*)? names", sheet.names)
    # Infer year from table of contents sheet
    contents <- read.xls(file, sheet = 1)
    year.string <- names(contents)[grepl(year.regex, names(contents))]
    year <- regmatches(year.string, regexpr(year.regex, year.string))
    if (any(sheet.loc)) {
      # only will happen if ONS changes structure of xls files
      if (sum(sheet.loc) > 1) {
        stop("too many sheets found")
      }
      sheet.number <- which(sheet.loc)

      xls.df <- read.xls(file, sheet = sheet.number, method = "csv",
                         skip = 2, stringsAsFactors = FALSE)
      # sheets contain a consierable number of empty columns.
      good.cols <- names(xls.df)[!grepl("X(\\.?[0-9]*)?", names(xls.df))]
      xls.df <- xls.df[, good.cols]
      xls.df[, "Sex"] <- ifelse(grepl("Boy", sheet.names[sheet.loc]), 
                                "M", "F")
      xls.df[, "Year"] <- as.numeric(year)
    } else {
      # may happen if ONS changes structure or download is corrupted
      stop("no full sheet found")
    }

    # cleanup df
    xls.df <- xls.df[, c("Name", "Count", "Sex", "Year")]
    xls.df <- cleanupNC(xls.df)

    return(xls.df)
  }

  files <- downloadONS()

  alluk.df <- do.call(rbind, lapply(
    list.files(files, full.names = TRUE),
    wrapXLS
  ))

  alluk.df <- ddply(alluk.df, "Year", function(x) {
                    cbind(matchSexes(x), Year = x[1, "Year"])
                  })
  unlink(files, recursive = TRUE)
  closeAllConnections()
  return(alluk.df)
}
