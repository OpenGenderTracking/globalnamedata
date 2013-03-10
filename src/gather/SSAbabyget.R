#####
# Download Baby Names from SSA and flatten into single data file
# 
# The SSA provides all name data in a single zip file which contains
# .csv files for each year. Both genders are included in each year
# file
#####


## Setup temp files
# Not strictly necessary, but we don't need to retain the zip
readSSANames <- function() {
  names.tmpdir <- tempdir()
  temp <- tempfile()
  output.file <- file.path(getwd(), "data/SSA.csv")

  # download and unzip
  # See http://www.ssa.gov/oact/babynames/limits.html for info 
  download.file('http://www.ssa.gov/oact/babynames/names.zip', temp)
  unzip(temp, exdir = names.tmpdir)

  # get vector of path names
  # sanity check list if working in an RStudio project
  yr.name.files <- list.files(path = names.tmpdir, 
                              pattern = "yob[0-9]{4}\\.txt", 
                              full.names = TRUE)


  # Read a csv from a file, adding the year from the file to 
  # a new column
  readWrap <- function(filepath) {
    
    yr.out <- read.csv(filepath, 
                       col.names = c("Name", "Sex", "Count"), 
                       header = FALSE, as.is = TRUE)
    yr.out[, "Year"] <- as.numeric(gsub("yob([0-9]{4})\\.txt", "\\1", basename(filepath)))
    yr.out <- cleanupNC(yr.out)
    return(yr.out)
  }

  # Combine all years into a single dataframe

  us.names.df <- do.call(rbind, lapply(yr.name.files, readWrap))


  # this will take a while. You're looping over 100+ years 
  # comprising ~2 million rows
  us.names.df <- ddply(us.names.df, "Year", function(x) matchSexes(x))
  unlink(c(names.tmpdir, temp))
  return(us.names.df)
}

# read and output to data frame

us.df <- readSSANames()


