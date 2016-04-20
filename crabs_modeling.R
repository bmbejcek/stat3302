## ======================================================================
## We continue with further analysis of the crabs dataset.
##
## Peter Craigmile, pfc@stat.osu.edu, Last updated Feb 2016
## ======================================================================

## We started our analysis in the file 'crabs_factors.R'.
## This analysis assumes that you run the commands in that file first!



## ======================================================================
## Now continue with the model builiding...
## ======================================================================

## summarize whether there is a satellite nearby according to
## the color.

table.color <- table(color.name, has.satellite)
props.color <- table.color/rowSums(table.color)
round(cbind(table.color, props.color), 3)


## summarize whether there is a satellite nearby according to
## the spine condition.

table.spine <- table(crabs$spine, has.satellite)
props.spine <- table.spine/rowSums(table.spine)
round(cbind(table.spine, props.spine), 3)


## summarize whether there is a satellite nearby according to
## the weight.

rounded.weight <- round(crabs$weight*2)/2
table.weight  <- table(rounded.weight, has.satellite)
prop.weight <- (table.weight/rowSums(table.weight))[,2]

rounded.weight2 <- round(crabs$weight*4)/4
table.weight2  <- table(rounded.weight2, has.satellite)
prop.weight2 <- (table.weight2/rowSums(table.weight2))[,2]

par(mfrow=c(2,2), bty="L", cex=0.8, mar=c(4.1, 4.1, 2, 1))

plot(sort(unique(rounded.weight)), prop.weight,
     xlab="weight (to nearest 0.5kg)", ylab="proportion")
plot(sort(unique(rounded.weight2)), prop.weight2,
     xlab="weight (to nearest 0.25kg)", ylab="proportion")
plot(sort(unique(rounded.weight)), logit(prop.weight),
     xlab="weight (to nearest 0.5kg) ", ylab="logit")
plot(sort(unique(rounded.weight2)), logit(prop.weight2),
     xlab="weight (to nearest 0.25kg)", ylab="logit")




## summarize whether there is a satellite nearby according to
## the width.

rounded.width <- round(crabs$width)
table.width  <- table(rounded.width, has.satellite)
prop.width <- (table.width/rowSums(table.width))[,2]

par(mfrow=c(2,1), bty="L", cex=0.8, mar=c(4.1, 4.1, 2, 1))

plot(sort(unique(rounded.width)), prop.width,
     xlab="width (to nearest 1cm)", ylab="proportion")
plot(sort(unique(rounded.width)), logit(prop.width),
     xlab="width (to nearest 1cm)", ylab="logit")





## set up the model parameterization.
options(contrasts=c("contr.treatment", "contr.poly"))


## Fit the logistic regression model predicting the probability of
## having a satellite nearby using the width.

glm.width <- glm(has.satellite ~ width, data=crabs, family=binomial)
summary(glm.width)
anova(glm.width, test="Chisq")




## Check the diagnostic plots

fits <- fitted(glm.width)
dev.resids <- resid(glm.width)
pear.resids <- resid(glm.width, type="pearson")

par(mfrow=c(3,2), bty="L", cex=0.7, mar=c(4.1, 4.1, 1, 1))

plot(fits, dev.resids, xlab="fitted values", ylab="deviance residuals", ylim=c(-2,2))
abline(h=0, lty=2)

plot(sort(unique(rounded.width)), prop.width,
     xlab="width (to nearest 1cm)", ylab="probability")
lines(sort(crabs$width), fits[order(crabs$width)], lty=2)

plot(crabs$width, dev.resids, xlab="width", ylab="deviance residuals", ylim=c(-2, 2))
abline(h=0, lty=2)

plot(crabs$width, pear.resids, xlab="width", ylab="Pearson residuals", ylim=c(-2, 2))
abline(h=0, lty=2)

qqnorm(dev.resids, ylab="deviance residuals", main="", ylim=c(-3, 3), xlim=c(-3,3))
qqline(dev.resids)





## add color to the model of width

glm.width.color <- glm(has.satellite ~ width + color.name,
                       data=crabs, family=binomial)
summary(glm.width.color)
anova(glm.width.color, test="Chisq")


## add weight to the model of width

glm.width.weight <- glm(has.satellite ~ width + weight,
                       data=crabs, family=binomial)
summary(glm.width.weight)
anova(glm.width.weight, test="Chisq")


## define the variable 'is.very.dark'
## is 0 if color is light medium, medium, or dark medium
## is 1 if color is dark.

is.very.dark <- factor(ifelse(crabs$color <= 3, "not dark", "dark"))


## add 'is.very.dark' to the model of width

glm.width.is.very.dark <- glm(has.satellite ~ width + is.very.dark,
                              data=crabs, family=binomial)
summary(glm.width.is.very.dark)
anova(glm.width.is.very.dark, test="Chisq")


par(mfrow=c(2,2))
plot(glm.width.is.very.dark)



## show the effects of the model without interactions

## predict on a sequence of widths
ws <- seq(min(crabs$width), max(crabs$width), length=100)

pred.dark     <- predict(glm.width.is.very.dark,
                         data.frame(width=ws,
                                    is.very.dark=rep("dark",100)),
                         se.fit=T)

pred.not.dark <- predict(glm.width.is.very.dark,
                         data.frame(width=ws,
                                    is.very.dark=rep("not dark",100)),
                         se.fit=T)

par(mfrow=c(2,1), bty="L", cex=0.8, mar=c(4.1, 4.1, 2, 1))

## plot the predictions on the logit scale
plot(ws, pred.dark$fit,
     type="l", ylim=c(-3,4), xlab="width", ylab="logit")
lines(ws, pred.not.dark$fit, lty=2)
legend(21, 4, c("dark", "not dark"), lty=1:2)

## plot on probability scale.
plot(ws, inv.logit(pred.dark$fit),
     type="l", ylim=c(0,1), xlab="width", ylab="probability")
lines(ws, inv.logit(pred.not.dark$fit), lty=2)
legend(21, 1, c("dark", "not dark"), lty=1:2)




## add pointwise confidence interval to these plots

par(mfrow=c(2,2), bty="L", cex=0.8, mar=c(4.1, 4.1, 2, 1))

## plot the predictions on the logit scale
plot(ws, pred.dark$fit,
     type="l", ylim=c(-4,5), xlab="width", ylab="logit")
lines(ws, pred.dark$fit + 1.96 * pred.dark$se.fit, lty=2)
lines(ws, pred.dark$fit - 1.96 * pred.dark$se.fit, lty=2)
mtext("Crab is very dark", side=3, cex=1, line=0.5)
legend(21, 5, c("estimate", "95% CI bounds"), lty=1:2, cex=0.8)

plot(ws, pred.not.dark$fit,
     type="l", ylim=c(-4,5), xlab="width", ylab="logit")
lines(ws, pred.not.dark$fit + 1.96 * pred.not.dark$se.fit, lty=2)
lines(ws, pred.not.dark$fit - 1.96 * pred.not.dark$se.fit, lty=2)
mtext("Crab is not very dark", side=3, cex=1, line=0.5)

## plot the predictions on the logit scale
plot(ws, inv.logit(pred.dark$fit),
     type="l", ylim=c(0,1), xlab="width", ylab="probability")
lines(ws, inv.logit(pred.dark$fit + 1.96 * pred.dark$se.fit), lty=2)
lines(ws, inv.logit(pred.dark$fit - 1.96 * pred.dark$se.fit), lty=2)

plot(ws, inv.logit(pred.not.dark$fit),
     type="l", ylim=c(0,1), xlab="width", ylab="probability")
lines(ws, inv.logit(pred.not.dark$fit + 1.96 * pred.not.dark$se.fit), lty=2)
lines(ws, inv.logit(pred.not.dark$fit - 1.96 * pred.not.dark$se.fit), lty=2)




## add an interaction term

glm.width.vdark.inter <- glm(has.satellite ~ width * is.very.dark,
                              data=crabs, family=binomial)
summary(glm.width.vdark.inter)
anova(glm.width.vdark.inter, test="Chisq")


## show the effect of the interactions

## predict on a sequence of widths
ws <- seq(min(crabs$width), max(crabs$width), length=100)

pred.dark     <- predict(glm.width.vdark.inter,
                         data.frame(width=ws,
                                    is.very.dark=rep("dark",100)),
                         se.fit=T)

pred.not.dark <- predict(glm.width.vdark.inter,
                         data.frame(width=ws,
                                    is.very.dark=rep("not dark",100)),
                         se.fit=T)


par(mfrow=c(2,1), bty="L", cex=0.8, mar=c(4.1, 4.1, 2, 1))

## plot the predictions on the logit scale
plot(ws, pred.dark$fit,
     type="l", ylim=c(-2,5), xlab="width", ylab="logit")
lines(ws, pred.not.dark$fit, lty=2)
legend(21, 5, c("dark", "not dark"), lty=1:2)

## plot on probability scale.
plot(ws, inv.logit(pred.dark$fit),
     type="l", ylim=c(0,1), xlab="width", ylab="probability")
lines(ws, inv.logit(pred.not.dark$fit), lty=2)
legend(21, 1, c("dark", "not dark"), lty=1:2)





## add pointwise confidence interval to these plots

par(mfrow=c(2,2), bty="L", cex=0.8, mar=c(4.1, 4.1, 2, 1))

## plot the predictions on the logit scale
plot(ws, pred.dark$fit,
     type="l", ylim=c(-4,5), xlab="width", ylab="logit")
lines(ws, pred.dark$fit + 1.96 * pred.dark$se.fit, lty=2)
lines(ws, pred.dark$fit - 1.96 * pred.dark$se.fit, lty=2)
mtext("Crab is very dark", side=3, cex=1, line=0.5)
legend(21, 5, c("estimate", "95% CI bounds"), lty=1:2, cex=0.8)

plot(ws, pred.not.dark$fit,
     type="l", ylim=c(-4,5), xlab="width", ylab="logit")
lines(ws, pred.not.dark$fit + 1.96 * pred.not.dark$se.fit, lty=2)
lines(ws, pred.not.dark$fit - 1.96 * pred.not.dark$se.fit, lty=2)
mtext("Crab is not very dark", side=3, cex=1, line=0.5)

## plot the predictions on the logit scale
plot(ws, inv.logit(pred.dark$fit),
     type="l", ylim=c(0,1), xlab="width", ylab="probability")
lines(ws, inv.logit(pred.dark$fit + 1.96 * pred.dark$se.fit), lty=2)
lines(ws, inv.logit(pred.dark$fit - 1.96 * pred.dark$se.fit), lty=2)

plot(ws, inv.logit(pred.not.dark$fit),
     type="l", ylim=c(0,1), xlab="width", ylab="probability")
lines(ws, inv.logit(pred.not.dark$fit + 1.96 * pred.not.dark$se.fit), lty=2)
lines(ws, inv.logit(pred.not.dark$fit - 1.96 * pred.not.dark$se.fit), lty=2)


