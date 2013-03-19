#####
###
### Download Baby Names from SSA and flatten into single data file
### 
### The SSA provides all name data in a single zip file which contains
### .csv files for each year. Both genders are included in each year file
### 
#####

# accepts a single argument (download) and returns a data frame
# for gender/name combinations
readSSANames <- function(download = FALSE) {
  if (download) {
    downloadSSA()
  }
  require(plyr)
  # Read a csv from a file, adding the year from the file to 
  # a new column
  readWrap <- function(filepath) {
    yr.out <- read.csv(filepath, 
                       col.names = c("Name", "Sex", "Count"), 
                       header = FALSE, as.is = TRUE)
    yr.out[, "Year"] <- as.numeric(gsub("yob([0-9]{4})\\.txt", 
                                        "\\1", 
                                        basename(filepath)))
    yr.out <- cleanupNC(yr.out)
    return(yr.out)
  }

  # Combine all years into a single dataframe
  # get vector of path names
  
  yr.name.files <- list.files(path = file.path(getwd(), "assets", "us"),  
                              full.names = TRUE)
  
  us.names.df <- do.call(rbind, lapply(yr.name.files, readWrap))

  # this will take a while. You're looping over 100+ years 
  # comprising ~2 million rows
  us.names.df <- ddply(us.names.df, "Year", function(x) matchSexes(x))
  return(us.names.df)
}
