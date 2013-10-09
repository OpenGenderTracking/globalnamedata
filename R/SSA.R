
#' Read and return United States name data from the 
#'   Social Security Administration
#'
#' Download data from the SSA website and convert into a single data frame
#' Downloading and converting data will take some time. 
#'
#' @param type string Specifying national or state level data
#'
#' @return Data frame with columns for Name, Year, and counts for 
#'   gender incidence
#' @keywords america
#' @seealso \code{\link{readONSNames}}, \code{\link{readNISRANames}}, 
#'   \code{\link{readGRONames}}
#' @export

readSSANames <- function(type) {
  base.url <- "http://www.ssa.gov/oact/babynames"
  type <- match.arg(type, choices = c("national", "state"))
  
  # Type will not chance as we iterate through files, so define these outside
  # readIndv()
  source <- switch(type,
                   national = paste(base.url, "names.zip", sep = "/"),
                   state = paste(base.url, "state/namesbystate.zip", sep = "/"))
  cols <- switch(type,
                  # By-year data format: 
                  # Mary,F,6919
                  # Anna,F,2698
                  # Emma,F,2034
                  # ...
                 national = c("Name", "Sex", "Count"),
                  # By-state data format: 
                  # OR,F,1910,Dorothy,57
                  # OR,F,1910,Mary,54
                  # OR,F,1910,Helen,48
                  # ...
                 state = c("State", "Sex", "Year", "Name", "Count"))
  out.names <- c("Name", "Sex", "Year", "Count", "State")

  # Extract content of each text file and convert to a conformable data.frame
  readIndv <- function(path) {
    ind <- read.csv(path, col.names = cols, header = FALSE, as.is = TRUE)
    ind <- cleanupNC(ind)

    if(type == "national") {
      out <- matchSexes(ind)
      out[, "Year"] <- as.numeric(gsub("yob([0-9]{4})\\.txt",
                                       "\\1",
                                       basename(path)))
    } else {
      out <- ddply(ind, "Year", function(x) {
                    cbind(matchSexes(x), Year = x[1, "Year"])
                  })
      out[, "State"] <- ind[1, "State"]
    }

    return(out)
  }

  # Combine all years into a single dataframe
  # get vector of path names
  
  ssa <- zipDir(source)
  us.df <- do.call(rbind, lapply(list.files(ssa, full.names = TRUE), readIndv))

  if(type == "state") {
    us.df[, "State"] <- as.factor(us.df[, "State"])
  }

  # Cleanup files and connections
  unlink(ssa, recursive = TRUE)
  closeAllConnections()
  return(us.df)
}
