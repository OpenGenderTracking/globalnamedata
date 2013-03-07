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
  yr.out[, "Year"] <- gsub("yob([0-9]{4})\\.txt", "\\1", basename(filepath))
  return(yr.out)
}

# Combine all years into a single dataframe

us.names.df <- do.call(rbind, lapply(yr.name.files, readWrap))

# Condense names to single row

matchSexes <- function(x) {
  # melt and cast are two broad data handling patterns
  # think of them as the two steps in constructing a
  # pivot table:
  
  # x is of the following form:
  #        Name Sex Count Year
  # 1      Mary   F  7065 1880
  # 2      Anna   F  2604 1880
  # 3      Emma   F  2003 1880
  # 4 Elizabeth   F  1939 1880
  # 5    Minnie   F  1746 1880
  # 6  Margaret   F  1578 1880
  
  x.out <- dcast(x[, -4], Name ~ Sex, sum, value.var = "Count")
  ## x.out structure
  #     Name  F   M
  # 1  Aaron  0 102
  # 2     Ab  0   5
  # 3  Abbie 71   0
  # 4 Abbott  0   5
  # 5   Abby  6   0
  # 6    Abe  0  50
  
  # Add a year column. Year is the same for all rows since
  # names are grouped per file per year.
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


# persist file to disk

write.table(us.names.df, output.file, 
            quote=FALSE, sep=",", row.names = FALSE, col.names = TRUE)

