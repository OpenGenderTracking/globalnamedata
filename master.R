#####
###
### Load in functions for Global Name Data
###
#####


source.list <- list.files(c(file.path(getwd(), "src"),
                            file.path(getwd(), "src", "gather"),
                            file.path(getwd(), "src", "process")),
                          pattern = "*.R$", full.names=TRUE)
# sets working directory to the project directory
# currently necessary for filesystem operations
setwd(getwd())

# loads in all relevant functions
sapply(source.list, source)
# cleans up source list
rm(source.list)