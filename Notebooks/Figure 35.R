#### Figure 35 ####

#### Packages ####
library(ggplot2)

#### Import Data ####
fineware <- read.csv("fineware.csv")

### GTS Provenances ####
gallic <- subset(fineware, Provenance_1=="Gaul")
gallic$Provenance_2 <- factor(gallic$Provenance_2,levels = c("Southern_Gaul", "Central_Gaul", "Not_Available"))
ggplot(gallic, aes(x=Provenance_2)) + geom_bar(stat="count", fill='grey1', width=0.5) + labs(title="", x="", y="") +  theme_minimal(base_size = 14) + theme(panel.background =element_blank(), panel.grid.major =element_line("dimgray",size=0.5, linetype="dashed"), panel.grid.minor=element_line("grey",size=0.5, linetype="dotted"), panel.border = element_rect(colour = "black", fill=NA, size=1)) + scale_y_continuous(breaks=seq(from=0,to=350,by=50)) + scale_x_discrete(labels=c("Southern_Gaul"="Southern Gaul", "Central_Gaul"="Central Gaul", "Not_Available"="Unknown"))
