
#' Read and return Scotland name data
#'
#' Download data from the Scotland GRO website and convert into a single data frame
#' Downloading and converting data will take some time. The resultant dataset is
#' provided as \code{\link{scotnames}}
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
    dlname <- function(url, year) {
      download.file(url, file.path(assets.path, paste0(year, ".csv")))
    }
    gro.base <- "http://www.gro-scotland.gov.uk/files2/stats"
    
    gro.files <- data.frame(
      Year = c(2009, 2010, 2011, 2012),
      Loc = c("popular-forenames/babiesnames09-table4.csv", 
              "popular-forenames/babiesnames2010-table4.csv",
              "popular-forenames/2011/babiesnames-2011-table4-final.csv",
              "popular-forenames/2012/babiesnames-2012-t4.csv")
      )

    assets.path <- file.path(tempdir(), "assets", "gro")
    dir.create(assets.path, recursive = TRUE)

     d_ply(gro.files, "Year", function(x) {
      dlname(url = paste(gro.base, x[, "Loc"], sep = "/"),
             year = x[, "Year"])
    })
    return(assets.path)
  }
  # Scotland provides one csv per year (boys and girls)
  indvData <- function(filepath) {
    year <- sub("([0-9]{4}).*$", "\\1", basename(filepath))
    # Structure for .csvs changes slightly y/y 
    # hardcoded for now
    gc <- 4:5
    if (year < 2011) {
      skip <- 3
    } else {
      skip <- 5
    }
    if (year == 2012) {
      gc <- 5:6
    }
    data <- read.csv(filepath,
                     skip = skip,
                     stringsAsFactors = FALSE,
                     header = FALSE)
    data <- data[, !grepl("^X(\\.[0-9]*)?$", names(data))]

    # An unfortunate for loop to catch missing values which mark the end of
    # data (removing missing values alone leaves some extraneous values)
    end <- 1
    while(!is.na(data[end, 2])) {
      end <- end + 1
    }
    data <- data[1:(end - 1), ]
    # column names in the csv are somewhat unreliable and
    # boys/girls are seperate columns.
    boys <- data[, 1:2]
    girls <- data[, gc]
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
  scotland.df <- ddply(scotland.df, "Year", function(x) {
                    cbind(matchSexes(x), Year = x[1, "Year"])
                  })
  unlink(scot.path, recursive = TRUE)
  closeAllConnections()
  return(scotland.df)
}
