#####
#
# Download Baby Names and flatten into single data file
#
#####

## Setup temp files
# Not strictly necessary, but we don't need to retain the zip

names.tmpdir <- tempdir()
temp <- tempfile()

# download and unzip
# See http://www.ssa.gov/oact/babynames/limits.html for info 
download.file('http://www.ssa.gov/oact/babynames/names.zip', temp)
unzip(temp, exdir = names.tmpdir)

# get vector of path names
yr.name.files <- list.files(path = names.tmpdir, pattern = "[^.]*\\.txt", full.names = TRUE)

# Read a csv from a file, adding the year from the file to 
# a new column
readWrap <- function(filepath) {
  
  yr.out <- read.csv(filepath, 
                     col.names = c("Name", "Sex", "Count"), 
                     header = FALSE, as.is = TRUE)
  yr.out[, "Year"] <- gsub("yob([0-9]{4})\\.txt", "\\1", basename(filepath))
  return(yr.out)
}

# Combine all years into a single dataframe
us.names.df <- do.call(rbind, lapply(yr.name.files, readWrap))

## Cleanup

unlink(c(names.tmpdir, temp))
rm(temp, yr.name.files, names.tmpdir)
