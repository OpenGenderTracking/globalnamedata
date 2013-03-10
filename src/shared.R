########
###
### Packages and utility functions for downloading name data
###
########

##
## Utility functions
##

# Condense names to single row
# Many sources include male and female names 
# in different locations

matchSexes <- function(x) {
  require(plyr)
  require(reshape2)
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
  
  x.out <- dcast(x[, c("Name", "Sex", "Count")], Name ~ Sex, sum, value.var = "Count")
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

# Common cleanup functions

cleanupNC <- function(data) {
  ## Count
  data[, "Count"] <- gsub(",|\\.+|;|\\s+", "", data[, "Count"])
  # remove rows
  data <- data[grepl("^[0-9]+$", data[, "Count"]), ]
  data[, "Count"] <- as.numeric(data[, "Count"])

  ## Name
  # NISRA and Scotland return some multi-byte characters
  if (any(is.na(nchar(data[, "Name"], allowNA = TRUE)))) {
    data[, "Name"] <- iconv(data[, "Name"],
                            from = "latin1",
                            to = "UTF-8")
  }
  data[, "Name"] <- gsub("^\\s+|\\s+$", "", data[, "Name"])
  data <- data[nchar(data[, "Name"]) > 0, ]
  rownames(data) <- as.character(1:nrow(data))
  return(data)
}
