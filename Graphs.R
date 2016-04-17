##MAKING GRAPHS FOR SUPPORT

onBaseGraph <- read.csv("~/Stat 3302/Stat 3302 Project/onBaseGraph.csv")
dat.m <- melt(onBaseGraph,id.vars = "onbase")
ggplot(dat.m,aes(x = onbase, y = value)) + geom_bar(aes(fill = variable),position = "dodge", stat="identity") + xlab("\nNumber of Runners on Base") + ylab("Count\n")+ theme(axis.text=element_text(size=12),
        axis.title=element_text(size=20), legend.text=element_text(size=20)) + guides(fill=guide_legend(title=NULL)) + scale_fill_manual(values=c("#56B4E9", "#F11E1E"))
