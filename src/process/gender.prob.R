######
#
# functions for gender computation
#
######

# turn counts into imputed probability of a name being male
# or female. Very basic at the moment

probProcess <- function(data) {
  require(plyr)
  # structure will look like this:
  
  #     Name  F   M Year
  # 1  Aaron  0 102 1880
  # 2     Ab  0   5 1880
  # 3  Abbie 71   0 1880
  # 4 Abbott  0   5 1880
  # 5   Abby  6   0 1880
  # 6    Abe  0  50 1880
  
  # Each name is now associated w/ a *single* row
  # so we only need to look up one "key" as it were
  
  
  # Handling years now, so we convert it to numeric 
  data[, "Year"] <- as.numeric(data[, "Year"])
  
  # Nate's year logic from https://gist.github.com/natematias/4743564
  # used for consistency
  
  nateMod <- function(x, penalty = 100) {
    out <- x
    # hahahaha That's awesome. This is
    # actually handy http://rmazing.wordpress.com/2013/01/30/the-magic-empty-bracket/
    out[] <- 1
    # unless I'm reading it wrong, there's a 
    # bug in the ruby script. Should be 1 + ...
    # 1 - ... weights old/new years more than 1960-1980
    
    # Post 1980 names shouldn't be penalized b/c we want to
    # capture m/f dynamics in new 1st gen american names
    
    # out[x > 1980] <- 1 + (1960 - x[x > 1980])/penalty
    out[x < 1960] <- 1 + (x[x < 1960] - 1980)/penalty
    return(out)
  }
  
  data[, "YearModifier"] <- nateMod(data[, "Year"])
  
  # Compute the bare proportion of female names
  # ddply not necessary, since we're not changing the mapping
  
  data[, "PropF"] <- with(data, F/(F + M))
  
  # Male is just 1 - PropF
  
  data[, "PropM"] <- 1 - data[, "PropF"]
  
  ## in practical terms very few names in a given year are ambiguous
  ## roughly 10% are not 1 or 0 and > 40% of those are 0-0.1 or 0.8-1
  ## There is still some value to retaining this information
  
  ## Sums up the proportion of female (male) names 
  
  # populate name column quickly 
  data.out <- data.frame(Name = sort(unique(data[, "Name"])), stringsAsFactors = FALSE)
  
  # MUCH faster than ddply or tapply
  data.out[, "SumPropF"] <- with(data, rowsum(PropF*YearModifier, Name))
  data.out[, "SumPropM"] <- with(data, rowsum(PropM*YearModifier, Name))
  
  # Counts appearances by years (used to normalize the result)
  data.out <- merge(data.out, count(data, "Name"), by = "Name")
  
  # This is a bit of spaghetti code, but it's our normalization  
  data.out[, "ImputedProbF"] <- data.out[, "SumPropF"] / data.out[, "freq"]
  data.out[, "ImputedProbM"] <- data.out[, "SumPropM"] / data.out[, "freq"]
  
  # cleanup the final df
  data.out <- data.out[, c("Name", "freq", 
                           "ImputedProbF",
                           "ImputedProbM")]
  
  names(data.out)[2] <- "YearsAppearing"
  
  return(data.out)
}
