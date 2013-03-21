
#' Read and return United States name data from the 
#'   Social Security Administration
#'
#' Download data from the SSA website and convert into a single data frame
#'
#' @return Data frame with columns for Name, Year, and counts for 
#'   gender incidence
#' @keywords america
#' @seealso \code{\link{readONSNames}}, \code{\link{readNISRANames}}, 
#'   \code{\link{readScotlandNames}}
#' @export
readSSANames <- function() {
  ## SSA Download
  downloadSSA <- function() {
    assets.path <- file.path(tempdir(), "assets", "us")
    temp <- tempfile(pattern = "ssa", fileext = ".zip")
    # download and unzip
    # See http://www.ssa.gov/oact/babynames/limits.html for info 
    download.file('http://www.ssa.gov/oact/babynames/names.zip', temp)
    unzip(temp, exdir = assets.path)
    unlink(temp, file.path(assets.path, "*.pdf"))
    closeAllConnections()
    return(assets.path)
  }
  # Read a csv from a file, adding the year from the file to 
  # a new column
  readWrap <- function(filepath) {
    yr.out <- read.csv(filepath, 
                       col.names = c("Name", "Sex", "Count"), 
                       header = FALSE, as.is = TRUE)
    yr.out[, "Year"] <- as.numeric(gsub("yob([0-9]{4})\\.txt", 
                                        "\\1", 
                                        basename(filepath)))
    yr.out <- cleanupNC(yr.out)
    return(yr.out)
  }

  # Combine all years into a single dataframe
  # get vector of path names
  
  ssa.path <- downloadSSA()
  files <- list.files(ssa.path,  
                      full.names = TRUE)
  
  us.df <- do.call(rbind, lapply(files, readWrap))

  # this will take a while. You're looping over 100+ years 
  # comprising ~2 million rows
  us.df <- ddply(us.df, "Year", function(x) matchSexes(x))
  unlink(ssa.path)
  return(us.df)
}
