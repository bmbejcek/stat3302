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

##Load Dataset
bonds <- read.csv("~/Stat 3302/Stat 3302 Project/Bonds Data", sep="")

##Create onBase variable
bonds$onBase<- bonds$result>0
bonds$onBase <- 1*(bonds$onBase)


attach(bonds)

##Does Inning Affect Performance?

bonds.glm <- glm(factor(bonds$onBase) ~ factor(bonds$inning), family = binomial)
summary(bonds.glm)

##Does Bases loaded affect?

bonds$basesLoaded <- bonds$first == 1 & bonds$second == 1 & bonds$third == 1
bonds$basesLoaded <- 1*(bonds$basesLoaded)
bonds2.glm <- glm(factor(bonds$onBase)~factor(bonds$basesLoaded), family = binomial)
summary(bonds2.glm)

##Model with anyone on base

bonds$anyOnBase <- bonds$first == 1 | bonds$second == 1 | bonds$third == 1
bonds$anyOnBase <- 1*(bonds$anyOnBase)

bonds3.glm <- glm(factor(bonds$onBase)~factor(bonds$anyOnBase), family = binomial)
summary(bonds3.glm)
anova(bonds3.glm, test = "Chisq")

##Model with num on base

bonds$noOnBase <- (bonds$first+bonds$second+bonds$third)

bonds4.glm <- glm(factor(bonds$onBase)~factor(bonds$noOnBase), family = binomial)
summary(bonds4.glm)
anova(bonds4.glm, test = "Chisq")

##Model with anyone on base and ERA

bonds$anyOnBase <- bonds$first == 1 | bonds$second == 1 | bonds$third == 1
bonds$anyOnBase <- 1*(bonds$anyOnBase)

bonds5.glm <- glm(factor(bonds$onBase)~factor(bonds$anyOnBase)+bonds$era, family = binomial)
summary(bonds5.glm)
anova(bonds5.glm, test = "Chisq")

##Model with anyone on base and appearance

bonds6.glm <- glm(factor(bonds$onBase)~factor(bonds$anyOnBase)+bonds$appearance, family = binomial)
summary(bonds6.glm)
anova(bonds6.glm, test = "Chisq")

##Model with anyone on base and appearance and outs

bonds7.glm <- glm(factor(bonds$onBase)~factor(bonds$anyOnBase)+bonds$appearance + bonds$outs, family = binomial)
summary(bonds7.glm)
anova(bonds7.glm, test = "Chisq")

##Model with anyone on base and appearance and difference in score

bonds$diffInScore <- bonds$giants-bonds$opposing
bonds8.glm <- glm(factor(bonds$onBase)~factor(bonds$anyOnBase)+bonds$appearance + bonds$diffInScore, family = binomial)
summary(bonds8.glm)
anova(bonds8.glm, test = "Chisq")

##Model with anyone on base and appearance and addition in score

bonds$additionInScore <- bonds$giants+bonds$opposing
bonds9.glm <- glm(factor(bonds$onBase)~factor(bonds$anyOnBase)+bonds$appearance + bonds$additionInScore, family = binomial)
summary(bonds9.glm)
anova(bonds9.glm, test = "Chisq")

##Model with anyone on base and appearance and home game

bonds9.glm <- glm(factor(bonds$onBase)~factor(bonds$anyOnBase)+bonds$appearance + bonds$home, family = binomial)
summary(bonds9.glm)
anova(bonds9.glm, test = "Chisq")

##Model with everything bc I love overfitting data

bonds9.glm <- glm(factor(bonds$onBase)~bonds$game+bonds$appearance, family = binomial)
summary(bonds9.glm)
anova(bonds9.glm, test = "Chisq")

bonds10.glm <- glm(factor(bonds$onBase)~factor(bonds$anyOnBase)+bonds$appearance + bonds$game, family = binomial)
summary(bonds10.glm)
anova(bonds10.glm, test = "Chisq")

bonds11.glm <- glm(factor(bonds$onBase)~factor(bonds$noOnBase)+bonds$appearance + bonds$game, family = binomial)
summary(bonds11.glm)
anova(bonds11.glm, test = "Chisq")

bonds12.glm <- glm(factor(bonds$intentional)~bonds$game, family = binomial)
summary(bonds12.glm)
anova(bonds12.glm, test = "Chisq")
