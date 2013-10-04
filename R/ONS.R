
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
    # somewhat fragile path to individual data pages
    indexGet <- function() {
      # index page for data 
      ons.index <- "rel/vsob1/baby-names--england-and-wales/index.html"
      index.doc <- htmlParse(file.path(ons.base.url, ons.index))
      xpath.release <- "//div[@class = 'previous-releases-results']//a"
      year.pages <- xpathSApply(index.doc, 
                                xpath.release, 
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
      excel.doc <- 
      xpath.xls <- "//div[@class='download-options']//a[@class='xls']"
      excel.out <- xpathSApply(htmlParse(paste0(ons.base.url, tables)),
                               xpath.xls,
                               xmlAttrs)
      # the class and the href are both attributes
      return(excel.out[1, ])
    }
    ons.base.url <- "http://www.ons.gov.uk/ons"
    assets.path <- file.path(tempdir(), "assets", "ons")
    dir.create(assets.path, recursive = TRUE)
    year.pages <- indexGet()
    # call tableGet for each year linked in the index
    year.tables <- sapply(year.pages, tableGet)
    year.tables <- paste0(ons.base.url, year.tables)
    # download function passed to lapply because
    # we have multiple excel files from a single index
    dlname <- function(url) {
      writeBin(getBinaryURL(url), 
               file.path(assets.path,basename(url)))
    }
    lapply(year.tables, dlname)
    return(assets.path)
  }
  # reading excel files and converting into tractable form
  # wrapped into a function because ONS stores many excel 
  # sheets (2 per year)
  wrapXLS <- function(file) {

    # find which sheet we need to look at as well as
    # which gender are we looking at.
    sheet.names <- sheetNames(file)
    sheet.loc <- grepl("(Boy|Girl)s names", sheet.names)
    if (any(sheet.loc)) {
      # only will happen if ONS changes structure of xls files
      if (sum(sheet.loc) > 1) {
        stop("too many sheets found")
      }
      sheet.number <- which(sheet.loc)
      year <- basename(sub("^[^0-9]*([0-9]{4}).*$", "\\1", basename(file)))
      xls.df <- read.xls(file, sheet = sheet.number, method = "csv",
                         skip = 2, stringsAsFactors = FALSE)
      # sheets contain a consierable number of empty columns.
      good.cols <- names(xls.df)[!grepl("X(\\.?[0-9]*)?", names(xls.df))]
      xls.df <- xls.df[, good.cols]
      xls.df[, "Sex"] <- ifelse(grepl("Boy", sheet.names[sheet.loc]), 
                                "M", "F")
      xls.df[, "Year"] <- year
    } else {
      # may happen if ONS changes structure or download is corrupted
      stop("no full sheet found")
    }

    # cleanup df
    xls.df <- xls.df[, c("Name", "Count", "Sex", "Year")]
    xls.df <- cleanupNC(xls.df)
    xls.df[, "Year"] <- as.numeric(xls.df[, "Year"])

    return(xls.df)
  }

  ons.path <- downloadONS()
  files <- list.files(ons.path,
                      full.names = TRUE)
  
  alluk.df <- do.call(rbind, lapply(files, wrapXLS))

  alluk.df <- ddply(alluk.df, "Year", function(x) matchSexes(x))
  unlink(ons.path, recursive = TRUE)
  closeAllConnections()
  return(alluk.df)
}
