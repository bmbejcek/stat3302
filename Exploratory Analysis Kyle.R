## ======================================================================
## Stat 3302 Project:
##
##   Exploratory data analysis, model building, selection, 
##   and diagnostics around the Barry Bonds 2001 Plate
##   Appearances. 
##
##   http://www.amstat.org/publications/jse/datasets/bonds2001.txt
##
##   Brett Bejcek, Kyle Voytovich, Last updated March 2016
## ======================================================================

# import data
bonds <- read.table("Bonds Data", header = TRUE)

# define new variables
bonds$success <- as.numeric(bonds$result != 0)
bonds$onbase <- bonds$first + bonds$second + bonds$third
bonds$anyonbase <- as.numeric((bonds$first + bonds$second + bonds$third) != 0)

# function to...
count_successes <- function(variable, result) {
  # result is a 0/1 vector for Bonds not getting on base / getting on base
  success <- function(result) {
    return(sum(result == 1))
  }
  failure <- function(result) {
    return(sum(result == 0))
  }
  
  suc <- aggregate(list(result = result),
                   by = list(variable = variable),
                   FUN = success)
  
  fai <- aggregate(list(result = result),
                   by = list(variable = variable),
                   FUN = failure)
  
  res <- data.frame(variable = suc$variable,
                    success = suc$result,
                    failure = fai$result,
                    total = suc$result + fai$result)
  
  return(res)
}