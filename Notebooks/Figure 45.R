#### Figure 45 ####

#### Packages ####
library(datplot)
library(dplyr)
library(ggplot2)
library(patchwork)

#### Import Data ####
fineware <- read.csv("fineware.csv")

#### Plot Theme ####
Plot_Theme<-theme(panel.background =element_blank(), panel.grid.major =element_line("dimgray",size=0.5, linetype="dashed"), panel.grid.minor=element_line("grey",size=0.5, linetype="dotted"), panel.border = element_rect(colour = "black", fill=NA, size=1))
Plot_Scale<-scale_x_continuous(breaks=seq(from=-200,to=900,by=100), limits=c(-200,900),name="")

#### ARS Fabric Chronology ####
african <- subset(fineware, Provenance_1=="North_Africa")
african2 <- african[complete.cases(african[ , 5]),]

### Remove forms without production dates ###
african3 <- african2[african2$Lower_Date_Form != "Not_Available", ]
african3 <- african3[african3$Upper_Date_Form != "Not_Available", ]


### Make production start and end dates numeric ###
african3$DAT_min <- as.numeric(african3$Lower_Date_Form)
african3$DAT_max <- as.numeric(african3$Upper_Date_Form)

### Creation of dating steps (multiplies each object into a number of observations dependent on the object’s dating range)
african_steps1<-african3 %>% 
  select(REFINI_ID, Fabric, DAT_min, DAT_max) %>% 
  datsteps(stepsize=1)

### Addition of weights to assign equal importance to all dated forms ###
african_steps1<-scaleweight(african_steps1,var="all")

### Breakdown of Distribution ###
binwidth<-attributes(african_steps1)$stepsize 
scalevalue<-get.histogramscale(african_steps1,binwidth=binwidth) 

### Breakdown of Distribution by Fabric ###
ggplot(african_steps1,aes(x=DAT_step, colour=variable), show.legend = FALSE) + geom_density(bw=5, aes(y = after_stat(count)), size=1.0, alpha = 0.25,) + Plot_Theme + Plot_Scale + scale_colour_manual(name  ="Fabric", values = c("#E7B800", "#C4961A", "#D16103", "#C3D7A4", "#52854C", "#4E84C4", "#293352")) + labs(x="",y="Frequency", legend="", title="", subtitle = "", legend="Fabric", attributes(fineware)$source,sep="")

