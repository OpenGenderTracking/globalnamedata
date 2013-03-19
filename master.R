#####
###
### Load in functions for Global Name Data
###
#####

# should be run with source("master.R", chdir = TRUE)
# This sets working directory to the project directory
# currently necessary for filesystem operations

# loads in all relevant functions
source.list <- list.files(c(file.path(getwd(), "src"),
                            file.path(getwd(), "src", "gather"),
                            file.path(getwd(), "src", "process")),
                          pattern = "*.R$", full.names=TRUE)

sapply(source.list, source)
# cleans up source list
rm(source.list)
