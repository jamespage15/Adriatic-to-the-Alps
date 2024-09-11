#### Figure 33 ####

#### Packages ####
library(ggplot2)

#### Import Data ####
fineware <- read.csv("fineware.csv")

#### CITS Provenances ####
italian <- subset(fineware, Provenance_1=="Central_Italy")
italian$Provenance_2 <- factor(italian$Provenance_2,levels = c("Arezzo", "Central_Italy", "Not_Available"))
ggplot(italian, aes(x=Provenance_2)) + geom_bar(stat="count", fill='grey1', width=0.5) + labs(title="", x="", y="") +  theme_minimal(base_size = 14) + theme(panel.background =element_blank(), panel.grid.major =element_line("dimgray",size=0.5, linetype="dashed"), panel.grid.minor=element_line("grey",size=0.5, linetype="dotted"), panel.border = element_rect(colour = "black", fill=NA, size=1)) + scale_y_continuous(breaks=seq(from=0,to=1700,by=100)) + scale_x_discrete(labels = c(NULL = "Unknown")) + scale_x_discrete(labels=c("Central_Italy"="Central Italy", "Not_Available"="Unknown"))
