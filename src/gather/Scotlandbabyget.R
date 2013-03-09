## Scotland
## Full name data is only provided for 2009 and 2010.
## All other years are aggregated/top rankings

readScotlandNames <- function() {
  indvData <- function(url) {
    data <- read.csv(url, skip = 2, stringsAsFactors = FALSE)
    year <- paste0("20", gsub("[a-z]*(?:[0-9]{2})?([0-9]{2}).*$", "\\1", basename(url)))
    data <- data[, !grepl("^X$", names(data))]
    boys <- data[, 1:2]
    girls <- data[, 3:4]
    names(girls) <- names(boys) <- c("Name", "Count")
    girls[, "Year"] <- boys[, "Year"] <- as.numeric(year)
    girls[, "Sex"] <- "F"
    boys[, "Sex"] <- "M"
    data.out <- rbind(girls, boys)

    data.out[, "Count"] <- gsub(",|\\.+|;|\\s+", "", data.out[, "Count"])
    data.out <- data.out[grepl("^[0-9]+$", data.out[, "Count"]), ]
    data.out[, "Name"] <- iconv(data.out[, "Name"], from = "latin1", to = "UTF-8")
    data.out <- data.out[nchar(data.out[, "Name"]) > 0, ]
    data.out[, "Count"] <- as.numeric(data.out[, "Count"])
    return(data.out)
  }
  urls <- c("http://www.gro-scotland.gov.uk/files2/stats/popular-forenames/babiesnames09-table4.csv",
            "http://www.gro-scotland.gov.uk/files2/stats/popular-forenames/babiesnames2010-table4.csv")
  scotland.df <- do.call(rbind, lapply(urls, indvData))
  scotland.df <- ddply(scotland.df, "Year", function(x) matchSexes(x))
  return(scotland.df)
}


scot.df <- readScotlandNames()




