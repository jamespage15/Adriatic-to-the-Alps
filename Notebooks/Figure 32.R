#### Figure 32 ####

#### Packages ####
library(ggplot2)

#### Import Data ####
fineware <- read.csv("fineware.csv")

#### ARS Fabric Types ####
african <- subset(fineware, Provenance_1=="North_Africa")
african2 <- african[complete.cases(african[ , 5]),]
ggplot(african2, aes(x=Fabric)) + geom_bar(stat="count", fill='grey1') + labs(title="", x="", y="") + theme_minimal(base_size = 14) + theme(panel.background =element_blank(), panel.grid.major =element_line("dimgray",size=0.5, linetype="dashed"), panel.grid.minor=element_line("grey",size=0.5, linetype="dotted"), panel.border = element_rect(colour = "black", fill=NA, size=1)) + scale_y_continuous(breaks=seq(from=0,to=1500,by=100)) + scale_x_discrete(labels = c(NULL = "Unknown"))  
