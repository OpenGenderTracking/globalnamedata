#####
###
### Scotland's General Register Office provides
### full name data only for 2009 and 2010.
###
#####

# accepts a single argument (download) and returns a data frame
# for gender/name combinations
readScotlandNames <- function(download = FALSE) {
  if (download) {
    downloadScotland()
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
  
  files <- list.files(file.path(getwd(), "assets", "scotlandgro"),
                      full.names = TRUE)

  scotland.df <- do.call(rbind, lapply(files, indvData))
  scotland.df <- ddply(scotland.df, "Year", function(x) matchSexes(x))
  return(scotland.df)
}
