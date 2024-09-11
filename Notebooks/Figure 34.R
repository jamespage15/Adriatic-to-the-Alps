#### Figure 34 ####

#### Packages ####
library(ggplot2)

#### Import Data ####
fineware <- read.csv("fineware.csv")

### ETS Provenances ####
eastern <- subset(fineware, Provenance_1=="Eastern_Mediterranean")
eastern2 <- eastern[complete.cases(eastern[ , 5]),]
ggplot(eastern2, aes(x=Fabric)) + geom_bar(stat="count", fill='grey1', width=0.75) + labs(title="", x="", y="") + theme_minimal(base_size = 14) + theme(panel.background =element_blank(), panel.grid.major =element_line("dimgray",size=0.5, linetype="dashed"), panel.grid.minor=element_line("grey",size=0.5, linetype="dotted"), panel.border = element_rect(colour = "black", fill=NA, size=1)) + scale_y_continuous(breaks=seq(from=0,to=350,by=50))  
