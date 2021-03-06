## MAKING GRAPHS 4 SUPPORT
#Graphs4Support
#Twitter
#Movement
#5+

onBaseGraph <- read.csv("~/Stat 3302/Stat 3302 Project/onBaseGraph.csv")
dat.m <- melt(onBaseGraph,id.vars = "onbase")
ggplot(dat.m,aes(x = onbase, y = value)) + geom_bar(aes(fill = variable),position = "dodge", stat="identity") + xlab("\nNumber of Runners on Base") + ylab("Count\n")+ theme(axis.text=element_text(size=12),
        axis.title=element_text(size=20), legend.text=element_text(size=20)) + guides(fill=guide_legend(title=NULL)) + scale_fill_manual(values=c("#56B4E9", "#F11E1E"))

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

Group10 <- read.csv("~/Stat 3302/Stat 3302 Project/Group10.csv")
ggplot(Group10, aes(x = Group, y = Percentage)) +
  geom_point(size = 5) +
  xlab("\nGroup Number") + ylab("Probability of Getting on Base\n") +
  theme(axis.text = element_text(size = 15),
        axis.title = element_text(size = 20))

## Check the diagnostic plots

fits <- fitted(bonds15.glm)
dev.resids <- resid(bonds15.glm)
pear.resids <- resid(bonds15.glm, type="pearson")

par(cex = 1.4)

plot(fits, dev.resids, xlab="fitted values", ylab="deviance residuals", ylim=c(-2,2))
abline(h=0, lty=2)

plot(bonds$game, dev.resids, xlab="game", ylab="deviance residuals", ylim=c(-2, 2))
abline(h=0, lty=2)

qqnorm(dev.resids, ylab="deviance residuals", main="", ylim=c(-3, 3), xlim=c(-3,3))
qqline(dev.resids)



