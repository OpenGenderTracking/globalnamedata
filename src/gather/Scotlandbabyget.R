## Scotland
## Full name data is only provided for 2009 and 2010.
## All other years are aggregated/top rankings

readScotlandNames <- function(download = FALSE) {
  if (download) {
    downloadScotland()
  }
  
  indvData <- function(filepath) {
    data <- read.csv(filepath, skip = 2, stringsAsFactors = FALSE)
    year <- sub("^[^0-9]*([0-9]{4}).*$", "\\1", basename(filepath))
    data <- data[, !grepl("^X$", names(data))]
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
  
  files <- list.files(file.path(getwd(), "assets", "scotlandgro"),
                      full.names = TRUE)

  scotland.df <- do.call(rbind, lapply(files, indvData))
  scotland.df <- ddply(scotland.df, "Year", function(x) matchSexes(x))
  return(scotland.df)
}
