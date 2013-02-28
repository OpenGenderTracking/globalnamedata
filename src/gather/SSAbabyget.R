#####
#
# Download Baby Names from SSA and flatten into single data file
#
#####

# libraries required for the last step. 
library(plyr)
library(reshape2)

## Setup temp files
# Not strictly necessary, but we don't need to retain the zip

names.tmpdir <- tempdir()
temp <- tempfile()

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
  yr.out[, "Year"] <- gsub("yob([0-9]{4})\\.txt", "\\1", basename(filepath))
  return(yr.out)
}

# Combine all years into a single dataframe

us.names.df <- do.call(rbind, lapply(yr.name.files, readWrap))

# Condense names to single row

matchSexes <- function(x) {
  # melt and cast are two broad data handling patterns
  # think of them as the two steps in constructing a
  # pivot table
  x.melt <- melt(x, id.vars = c("Name", "Sex"), measure.vars = "Count")
  x.out <- dcast(x.melt, Name ~ Sex, sum)
  x.out[, "Name"] <- as.character(x.out[, "Name"])
  # Faster/safer than unique(x[, "Year"])
  # no easy way to extract from the passed argument
  x.out[, "Year"] <- x[, "Year"][1]
  return(x.out)
}

# this will take a while. You're looping over 100+ years 
# comprising ~2 million rows

us.names.df <- ddply(us.names.df, "Year", function(x) matchSexes(x))


## Cleanup from import
# not required but still

unlink(c(names.tmpdir, temp))
rm(temp, yr.name.files, names.tmpdir, readWrap)
