
#' Read and return Northern Ireland Name Data
#'
#' Download data from the NISRA website and convert into a single data frame
#'
#' @return Data frame with columns for Name, Year, and counts for 
#'   gender incidence
#' @keywords ireland
#' @seealso \code{\link{readONSNames}}, \code{\link{readScotlandNames}}, 
#'   \code{\link{readSSANames}}
#' @export
#' @importFrom RCurl getBinaryURL
#' @importFrom XML htmlParse xpathSApply xmlAttrs
readNISRANames <- function() {
  # if needed, the path to perl can be set as an argument here
  if (length(xlsFormats()) != 2) {
    installXLSXsupport()
  }

  ## Nisra download
  downloadNISRA <- function() {
    indexGet <- function(index.url) {
      index.doc <- htmlParse(index.url)
      # NISRA links to baby names from their index
      archive <- xpathSApply(index.doc, 
                             "//a[contains(text(), 'Full Baby Names')]", 
                             xmlAttrs)
      return(unname(archive))
    }
    assets.path <- file.path(tempdir(), "assets", "nisra")
    nisra.index <- "http://www.nisra.gov.uk/demography/default.asp28.htm"
    url <- indexGet(nisra.index)
    writeBin(getBinaryURL(url), file.path(assets.path, "nisra.xls"))
    closeAllConnections()
    return(assets.path)
  }

  # because the data is in a single file, we need to split out
  # years and gender
	nisraSplit <- function(data) {
	  # Years are cell labels for multiple columns
	  # We fill down each cell w/ the correct year
	  # to make processing easier
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
    
	  data[1, ] <- naiveFillDown(as.character(data[1, ]))
	  data <- data[, !grepl("Rank", data[2, ])]
	  # year.ind allows us to split out columns 
	  year.ind <- as.numeric(data[1, ])
	  # Create a list because the dataframes will all be different dimensions
	  df.list <- vector(mode = "list", length = length(unique(year.ind)))
	  # years are repeated as columns (with counts, rank, etc)
    # for presentation purposes in the XLS file
    # so we can't rely on column names to disambiguate
    for (i in seq_along(unique(year.ind))) {
	    # year.ind is the same length as there are columns in data
	    col.ind <- year.ind %in% unique(year.ind)[i]
	    df.split <- data[, col.ind]
	    df.split <- df.split[3:nrow(df.split), ]
	    df.split[, "Year"] <- year.ind[col.ind][1]
	    names(df.split) <- c("Name", "Count", "Year")
	    df.split <- cleanupNC(df.split)
	    df.list[[i]] <- df.split
	  }
	  return(do.call(rbind, df.list))
	}

  nisra.path <- downloadNISRA()
  nisra <- file.path(nisra.path, "nisra.xls")

  # I'd like to be more flexible here but the sheet numbering
  # is embedded in the first sheet and building an index from
  # that would be just as brittle and twice as slow

  boys <- read.xls(nisra, sheet = 2, 
                   stringsAsFactors = FALSE)
  girls <- read.xls(nisra, sheet = 3, 
                    stringsAsFactors = FALSE)

	girls.df <- nisraSplit(girls)
	girls.df[, "Sex"] <- "F"

	boys.df <- nisraSplit(boys)
	boys.df[, "Sex"] <- "M"

	df.out <- ddply(rbind(girls.df, boys.df), 
                  "Year", function(x) matchSexes(x))

  unlink(nisra.path)
	return(df.out)
}
