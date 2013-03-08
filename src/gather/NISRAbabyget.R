### Northern Ireland name dataset
## From 1997-2011
## provided in a single excel file including boys and girls

readNISRANames <- function() {
	nisra.url <- "http://www.gov.uk/archive/demography/publications/babynames/Full_Name_List_9711.xls"
	temp.file <- downloadXLS(url = nisra.url,
                          pattern = "nisra")
	# I'd like to be more flexible here but the sheet numbering
	# is embedded in the first sheet and building an index from
	# that would be just as brittle and twice as slow
	boys <- read.xls(temp.file, sheet = 2, 
	                       stringsAsFactors = FALSE)
	girls <- read.xls(temp.file, sheet = 3, 
	                       stringsAsFactors = FALSE)
	unlink(temp.file)
	
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


	nisraSplit <- function(data) {
	  data[1, ] <- naiveFillDown(as.character(data[1, ]))
	  data <- data[, !grepl("Rank", data[2, ])]
	  # year.ind allows us to split out columns 
	  year.ind <- as.numeric(data[1, ])
	  # we create a list because the dataframes will all be different dimensions
	  df.list <- vector(mode = "list", length = length(unique(year.ind)))
	  for (i in seq_along(unique(year.ind))) {
	    # I feel filthy
	    # year.ind is the same length as there are columns in data
	    col.ind <- year.ind %in% unique(year.ind)[i]
	    df.split <- data[, col.ind]
	    df.split <- df.split[3:nrow(df.split), ]
	    df.split[, "Year"] <- year.ind[col.ind][1]
	    names(df.split) <- c("Name", "Count", "Year")
	    # despite the documentation, some wide characters end up in the 
	    # dataset
	    df.split[, "Name"] <- iconv(df.split[, "Name"], from = "latin1", to = "UTF-8")
	    df.split <- df.split[nchar(df.split[, "Name"]) > 0, ]
	    ## Handling counts
	    # remove non-numeric elements
	    df.split[, "Count"] <- gsub(",|\\.+|;|\\s+", "", df.split[, "Count"])
	    # remove rows
	    df.split <- df.split[grepl("^[0-9]+$", df.split[, "Count"]), ]
	    df.split[, "Count"] <- as.numeric(df.split[, "Count"])
	    df.list[[i]] <- df.split
	  }
	  return(do.call(rbind, df.list))
	}

	girls.df <- nisraSplit(girls)
	girls.df[, "Sex"] <- "F"


	boys.df <- nisraSplit(boys)
	boys.df[, "Sex"] <- "M"

	df.out <- ddply(rbind(girls.df, boys.df), "Year", function(x) matchSexes(x))
	return(df.out)
}


    

