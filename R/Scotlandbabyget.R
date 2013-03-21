
#' Read and return Scotland name data
#'
#' Download data from the Scotland GRO website and convert into a single data frame
#'
#' @return Data frame with columns for Name, Year, and counts for 
#'   gender incidence
#' @keywords scotland
#' @seealso \code{\link{readONSNames}}, \code{\link{readNISRANames}}, 
#'   \code{\link{readSSANames}}
#' @export
readScotlandNames <- function() {
  ## Scotland Dowload
  downloadScotland <- function() {
    dlname <- function(url) {
      year <- paste0("20", 
                     gsub("[a-z]*(?:[0-9]{2})?([0-9]{2}).*$", 
                          "\\1", basename(url)))
      download.file(url,
                    destfile = file.path(assets.path,
                                         paste0("gro", year, ".csv")))
    }
    gro.base <- "http://www.gro-scotland.gov.uk/files2/stats"
    
    gro.files <- c("popular-forenames/babiesnames09-table4.csv", 
                   "popular-forenames/babiesnames2010-table4.csv")
    assets.path <- file.path(tempdir(), "assets", "scotlandgro")
    lapply(paste(gro.base, gro.files, sep = "/"), dlname)
    closeAllConnections()
    return(assets.path)
  }
  # Scotland provides one csv per year (boys and girls)
  indvData <- function(filepath) {
    data <- read.csv(filepath, skip = 2, stringsAsFactors = FALSE)
    year <- sub("^[^0-9]*([0-9]{4}).*$", "\\1", basename(filepath))
    data <- data[, !grepl("^X$", names(data))]
    # column names in the csv are somewhat unreliable and
    # boys/girls are seperate columns.
    boys <- data[, 1:2]
    girls <- data[, 3:4]
    names(girls) <- names(boys) <- c("Name", "Count")
    girls[, "Year"] <- boys[, "Year"] <- as.numeric(year)
    girls[, "Sex"] <- "F"
    boys[, "Sex"] <- "M"
    data.out <- rbind(girls, boys)
    data.out <- cleanupNC(data.out)

    return(data.out)
  }
  
  scot.path <- downloadScotland()
  files <- list.files(scot.path,
                      full.names = TRUE)

  scotland.df <- do.call(rbind, lapply(files, indvData))
  scotland.df <- ddply(scotland.df, "Year", function(x) matchSexes(x))
  unlink(scot.path)
  return(scotland.df)
}
