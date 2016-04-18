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

wd <- "/Users/iowner/OneDrive/School/Spring_2016/Stat 3302/Project/Stat 3302 Project/"
setwd(wd)

bonds <- read.table("Bonds Data", header = TRUE)

# count_successes: returns a data frame of the number of successes and
#   failures in the result divided by the values that variable takes
#     @result - 0/1 vector where 0 is a failure and 1 is a success
#     @variable - values to split successes and failures into
#     @name - name of variable in output data frame
count_successes <- function(result, variable, name) {
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
  names(res) <- c(name, "success", "failure", "total")
  
  return(res)
}

# percent_successes: convert raw counts to percentages
percent_successes <- function(df) {
  result <- df
  result$success <- result$success / result$total
  result$failure <- result$failure / result$total
  return(result)
}

# Whether bonds gets on base
bonds$success <- as.numeric(bonds$result != 0)

# Is it a home game?
# Is someone on first base?
# Is someone on second base?
# Is someone on third base?
# How many people are on base?
# How many outs are there?
# What inning is it?
# What appearance is it for bonds?
# What's the opposing pitcher's era?

bonds$onbase <- bonds$first + bonds$second + bonds$third
bonds$inningcap <- ifelse(bonds$inning >= 10, 10, bonds$inning)
bonds$appearancecap <- ifelse(bonds$appearance >= 5, 5, bonds$appearance)
bonds$erarange <- floor(bonds$era)

home <- count_successes(bonds$success, bonds$home, "home")
first <- count_successes(bonds$success, bonds$first, "first")
second <- count_successes(bonds$success, bonds$second, "second")
third <- count_successes(bonds$success, bonds$third, "third")
onbase <- count_successes(bonds$success, bonds$onbase, "onbase")
outs <- count_successes(bonds$success, bonds$outs, "outs")
inning <- count_successes(bonds$success, bonds$inningcap, "inningcap")
appearance <- count_successes(bonds$success, bonds$appearancecap, "appearancecap")
era <- count_successes(bonds$success, bonds$erarange, "erarange")

homep <- percent_successes(home)
firstp <- percent_successes(first)
secondp <- percent_successes(second)
thirdp <- percent_successes(third)
onbasep <- percent_successes(onbase)
outsp <- percent_successes(outs)
inningp <- percent_successes(inning)
appearancep <- percent_successes(appearance)
erap <- percent_successes(era)

# models for whether bonds gets on base

# number of people on base
bonds.glm <- glm(bonds$success ~ factor(bonds$onbase), family = binomial)
summary(bonds.glm)
anova(bonds.glm, test = "Chisq")

# number of people on base + appearance
bonds2.glm <- glm(bonds$success ~ factor(bonds$onbase) + bonds$appearance,
                  family = binomial)
summary(bonds2.glm)
anova(bonds2.glm, test = "Chisq")

# graphs
library(ggplot2)
library(boot)

ggplot(appearancep, aes(x = appearancecap, y = success)) +
  geom_point(color = "#000000", size = 5) +
  guides(color = "FALSE", size = FALSE) +
  scale_x_continuous(labels = c("1", "2", "3", "4", "5+")) +
  xlab("\nAppearance Number") + ylab("Prob of Success\n") +
  theme(axis.text = element_text(size = 15),
        axis.title = element_text(size = 20))

ggplot(appearancep, aes(x = I(1/appearancecap), y = success)) +
  geom_point(color = "#000000", size = 5) +
  guides(color = "FALSE", size = FALSE) +
  xlab("\nReciprocal Appearance Number") + ylab("Prob of Success\n") +
  theme(axis.text = element_text(size = 15),
        axis.title = element_text(size = 20))

ggplot(appearancep, aes(x = appearancecap, y = logit(success))) +
  geom_point()
ggplot(appearancep, aes(x = I(1/appearancecap), y = logit(success))) +
  geom_point()




