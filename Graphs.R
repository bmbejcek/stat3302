##MAKING GRAPHS FOR SUPPORT

onBaseGraph <- read.csv("~/Stat 3302/Stat 3302 Project/onBaseGraph.csv")
dat.m <- melt(onBaseGraph,id.vars = "onbase")
ggplot(dat.m,aes(x = onbase, y = value)) + geom_bar(aes(fill = variable),position = "dodge", stat="identity") + xlab("Number of Players on Base") + ylab("Count")