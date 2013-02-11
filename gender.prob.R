
library(plyr)
library(reshape2)

# Some table black magic
# do.call (with list() passed to args) lets us IIFE this
# table is a (fast) contingency table builder
# but we need to map it quickly to the Names column

# we don't need it because we get there w/ cast/melt
# but it's cool nonetheless

# us.names.df[, "Both"] <- do.call(function() {
#                                   x <- table(us.names.df[, c("Name", "Sex")])
#                                   both <- row.names(x)[x[, 1] & x[, 2]]
#                                   us.names.df[, "Name"] %in% both
#                                 }, list())


matchSexes <- function(x) {
  # melt and cast are two broad data handling patterns
  # think of them as the two steps in constructing a
  # pivot table
  x.melt <- melt(x, id.vars = c("Name", "Sex"), measure.vars = "Count")
  x.out <- dcast(x.melt, Name ~ Sex, sum)
  x.out[, "Name"] <- as.character(x.out[, "Name"])
  # Totals may be useful eventually
  x.out[, "Total"] <- sum(x[, "Count"])
  # Faster/safer than unique(x[, "Year"])
  # no easy way to extract from the passed argument
  x.out[, "Year"] <- x[, "Year"][1]
  return(x.out)
}

# this will take a while. You're looping over 100+ years 
# comprising ~2 million rows

us.names.df <- ddply(us.names.df, "Year", function(x) matchSexes(x))

# structure will look like this:

#     Name  F   M Year  Total
# 1  Aaron  0 102 1880 201486
# 2     Ab  0   5 1880 201486
# 3  Abbie 71   0 1880 201486
# 4 Abbott  0   5 1880 201486
# 5   Abby  6   0 1880 201486
# 6    Abe  0  50 1880 201486

# Each name is now associated w/ a *single* row
# so we only need to look up one "key" as it were

# Handling years now, so we convert it to numeric 

us.names.df[, "Year"] <- as.numeric(us.names.df[, "Year"])

# Nate's year logic from https://gist.github.com/natematias/4743564
# used for consistency

nateMod <- function(x) {
  out <- x
  # hahahaha That's awesome. This is
  # actually handy http://rmazing.wordpress.com/2013/01/30/the-magic-empty-bracket/
  out[] <- 1
  # unless I'm reading it wrong, there's a 
  # bug in the ruby script. Should be 1 + ...
  # 1 - ... weights old/new years more than 1960-1980
  out[x > 1980] <- 1 + (1960 - x[x > 1980])/100
  out[x < 1960] <- 1 + (x[x < 1960] - 1980)/100
  return(out)
}

us.names.df[, "YearModifier"] <- nateMod(us.names.df[, "Year"])

# Compute the bare proportion of female names

us.names.df <- ddply(us.names.df, "Year", transform, PropF = F/(F + M))

# Male is just 1 - PropF

us.names.df[, "PropM"] <- 1 - us.names.df[, "PropF"]

# This will take a while as we are looping over *names* (there are a lot)

# Sums up the proportion of female (male) names and multiplies by our modifier
# from Nate's gist
us.final.df <- ddply(us.names.df, 
                        "Name", function(x) {
                          data.frame(ModProportionF = sum(x[, "PropF"] * x[, "YearModifier"]),
                            				 ModProportionM = sum(x[, "PropM"] * x[, "YearModifier"]))
                        })

# Counts appearances by years (used to normalize the result)
us.final.df <- merge(us.final.df, count(us.names.df, "Name"), by = "Name")
 
# This is a bit of spaghetti code, but it's our normalization  
us.final.df[, "ImputedProbF"] <- us.final.df[, "ModProportionF"] / us.final.df[, "freq"]
us.final.df[, "ImputedProbM"] <- us.final.df[, "ModProportionM"] / us.final.df[, "freq"]

# I don't know if a boolean is faster to read from a csv in Ruby
# but this allows us to reject names which have *no* other sex
# occurrences 
us.final.df[, "AnyFemale"] <- us.final.df[, "ImputedProbF"] > 0
us.final.df[, "AnyMale"] <- us.final.df[, "ImputedProbM"] > 0

# cleanup the final df
us.final.df <- us.final.df[, c("Name", "freq", 
                               "AnyFemale", "ImputedProbF",
                               "AnyMale", "ImputedProbM")]

names(us.final.df)[2] <- "YearsAppearing"



