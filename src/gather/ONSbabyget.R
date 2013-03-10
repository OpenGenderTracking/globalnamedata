#####
# Read names from the UK's Office of National Statistics (ONS)
#
# Names are included in Excel spreadsheets broken up by gender
# and year. Links to those spreadsheets are gathered from 
# the data navigator provided by ONS
#####

readONSNames <- function(download = FALSE) {
  if (download) {
    downloadONS()
  }

  require(gdata)
  # if needed, the path to perl can be set as an argument here
  if (length(xlsFormats()) != 2) {
    installXLSXsupport()
  }

  # drop columns which are auto-named

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

      xls.df <- read.xls(file, sheet = sheet.number, method = "csv",
                         skip = 2, stringsAsFactors = FALSE)
      xls.df <- xls.df[, names(xls.df)[!grepl("X(\\.?[0-9]*)?", names(xls.df))]]
      xls.df[, "Sex"] <- ifelse(grepl("Boy", sheet.names[sheet.loc]), "M", "F")
      xls.df[, "Year"] <- basename(sub("^[^0-9]*([0-9]{4}).*$", "\\1", basename(file)))
    } else {
      stop("no full sheet found")
    }

    # cleanup df
    xls.df <- xls.df[, c("Name", "Count", "Sex", "Year")]
    
    xls.df <- cleanupNC(xls.df)
    xls.df[, "Year"] <- as.numeric(xls.df[, "Year"])

    return(xls.df)
  }
  
  files <- list.files(file.path(getwd(), "assets", "ons"),
                      full.names = TRUE)
  
  alluk.df <- do.call(rbind, lapply(files, wrapXLS))

  alluk.df <- ddply(alluk.df, "Year", function(x) matchSexes(x))
  return(alluk.df)
}
