#####
###
### Northern Ireland name dataset
### From 1997-2011
### provided in a single excel file including boys and girls
###
#####

# accepts a single argument (download) and returns a data frame
# for gender/name combinations
readNISRANames <- function(download = FALSE) {
  if (download) {
    downloadNISRA()
  }

  require(gdata)
  # if needed, the path to perl can be set as an argument here
  if (length(xlsFormats()) != 2) {
    installXLSXsupport()
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
  assets.path <- file.path(getwd(), "assets", "nisra")
  nisra <- file.path(assets.path, "nisra.xls")

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
	return(df.out)
}
