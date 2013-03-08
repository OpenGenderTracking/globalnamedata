# Ireland urls

# http://www.nisra.gov.uk/archive/demography/publications/babynames/Full_Name_List_9711.xls


temp.nisra <- downloadXLS(url = "http://www.nisra.gov.uk/archive/demography/publications/babynames/Full_Name_List_9711.xls",
                          pattern = "nisra")

nisra.boys <- read.xls(temp.nisra, sheet = 2, 
                       stringsAsFactors = FALSE)
nisra.girls <- read.xls(temp.nisra, sheet = 3, 
                       stringsAsFactors = FALSE)

naiveFillDown <- function(char) {
  buffer <- NA
  for (i in seq_along(char)) { 
    if(nchar(char[i]) > 0) {
      buffer <- char[i]
    } else {
      char[i] <- buffer
    }
  }
  return(char)
}


nisraSplit <- function(data) {
  data[1, ] <- naiveFillDown(as.character(data[1, ]))
  data <- data[, !grepl("Rank", data[2, ])]
  year.ind <- as.numeric(data[1, ])
  df.list <- vector(mode = "list", length = length(unique(year.ind)))
  for (i in seq_along(unique(year.ind))) {
    # I feel filthy
    col.ind <- year.ind %in% unique(year.ind)[i]
    df.split <- data[, col.ind]
    df.split <- df.split[3:nrow(df.split), ]
    df.split[, "Year"] <- year.ind[col.ind][1]
    names(df.split) <- c("Name", "Count", "Year")
    df.split[, "Name"] <- iconv(df.split[, "Name"], from = "latin1", to = "UTF-8")
    df.split <- df.split[nchar(df.split[, "Name"]) > 0, ]
    df.split[, "Count"] <- gsub(",|\\.+|;|\\s+", "", df.split[, "Count"])
    df.split <- df.split[grepl("^[0-9]+$", df.split[, "Count"]), ]
    df.split[, "Count"] <- as.numeric(df.split[, "Count"])
    df.list[[i]] <- df.split
  }
  return(do.call(rbind, df.list))
}
nisra.girls.df <- nisraSplit(nisra.girls)
nisra.girls.df[, "Sex"] <- "F"


nisra.boys.df <- nisraSplit(nisra.boys)
nisra.boys.df[, "Sex"] <- "M"




nisra.df <- rbind(nisra.girls.df, nisra.boys.df)

nisra.df <- ddply(nisra.df, "Year", function(x) matchSexes(x))


    

