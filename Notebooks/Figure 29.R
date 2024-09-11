#### Figure 29 ####

#### Packages ####
library(datplot)
library(dplyr)
library(ggplot2)
library(patchwork)

#### Import Data ####
amphora <- read.csv("amphora.csv")

### Plot Theme ###
Plot_Theme<-theme(panel.background =element_blank(), panel.grid.major =element_line("dimgray",size=0.5, linetype="dashed"), panel.grid.minor=element_line("grey",size=0.5, linetype="dotted"), panel.border = element_rect(colour = "black", fill=NA, size=1))
Plot_Scale<-scale_x_continuous(breaks=seq(from=-400,to=900,by=100), limits=c(-400,900),name="")

#### Altinum Dates ####
amphora1 <- subset(amphora, Location_Specific=="Altinum")
amphora1 <- amphora1[amphora1$Provenance_1 != "Not_Available", ]

### Remove forms without production dates ###
amphora1 <- amphora1[amphora1$Lower_Date_Form != "Not_Available", ]
amphora1 <- amphora1[amphora1$Upper_Date_Form != "Not_Available", ]

### Make production start and end dates numeric ###
amphora1$DAT_min <- as.numeric(amphora1$Lower_Date_Form)
amphora1$DAT_max <- as.numeric(amphora1$Upper_Date_Form)

### Creation of dating steps (multiplies each object into a number of observations dependent on the object’s dating range)
amphora_steps1<-amphora1 %>% 
  select(AMINI_ID, Provenance_1, DAT_min, DAT_max) %>% 
  datsteps(stepsize=1)

### Addition of weights to assign equal importance to all dated forms ###
amphora_steps1<-scaleweight(amphora_steps1,var="all")
binwidth<-attributes(amphora_steps1)$stepsize 
scalevalue<-get.histogramscale(amphora_steps1,binwidth=binwidth)                                                                                                                                                                                                       

### Breakdown of Distribution by Contents ###
p1 <- ggplot(amphora_steps1,aes(x=DAT_step, colour=variable)) + geom_density(bw=5, aes(y = after_stat(count)), size=1.0, alpha = 0.25,) + Plot_Theme + Plot_Scale + scale_colour_manual(name  ="Provenance", labels = c("Adriatic Littoral", "Eastern Mediterranean", "Gaul", "Iberian Peninsula", "North Africa", "Tyrrhenian Littoral"), values = c("steelblue2", "indianred2", "sandybrown", "khaki3", "#81A88D", "mediumorchid3")) + labs(y="", legend="Provenance", subtitle = "Altinum", attributes(amphora)$source,sep="") 


#### Aquileia Dates ####
amphora2 <- subset(amphora, Location_Specific=="Aquileia")
amphora2 <- amphora2[amphora2$Provenance_1 != "Not_Available", ]

### Remove forms without production dates ###
amphora2 <- amphora2[amphora2$Lower_Date_Form != "Not_Available", ]
amphora2 <- amphora2[amphora2$Upper_Date_Form != "Not_Available", ]

### Make production start and end dates numeric ###
amphora2$DAT_min <- as.numeric(amphora2$Lower_Date_Form)
amphora2$DAT_max <- as.numeric(amphora2$Upper_Date_Form)

### Creation of dating steps (multiplies each object into a number of observations dependent on the object’s dating range)
amphora_steps2<-amphora2 %>% 
  select(AMINI_ID, Provenance_1, DAT_min, DAT_max) %>% 
  datsteps(stepsize=1)

### Addition of weights to assign equal importance to all dated forms ###
amphora_steps2<-scaleweight(amphora_steps2,var="all")
binwidth<-attributes(amphora_steps2)$stepsize 
scalevalue<-get.histogramscale(amphora_steps2,binwidth=binwidth)                                                                                                                                                                                                       

### Breakdown of Distribution by Contents ###
p2 <- ggplot(amphora_steps2,aes(x=DAT_step, colour=variable)) + geom_density(bw=5, aes(y = after_stat(count)), size=1.0, alpha = 0.25,) + Plot_Theme + Plot_Scale + scale_colour_manual(name  ="Provenance", labels = c("Adriatic Littoral", "Eastern Mediterranean", "Iberian Peninsula", "North Africa", "Tyrrhenian Littoral"), values = c("steelblue2", "indianred2", "khaki3", "#81A88D", "mediumorchid3")) + labs(y="Frequency", legend="Provenance", subtitle="Aquileia", attributes(amphora)$source,sep="") + theme(legend.position = "none")


#### Ariminum Dates ####
amphora3 <- subset(amphora, Location_Specific=="Ariminum")
amphora3 <- amphora3[amphora3$Provenance_1 != "Not_Available", ]

### Remove forms without production dates ###
amphora3 <- amphora3[amphora3$Lower_Date_Form != "Not_Available", ]
amphora3 <- amphora3[amphora3$Upper_Date_Form != "Not_Available", ]

### Make production start and end dates numeric ###
amphora3$DAT_min <- as.numeric(amphora3$Lower_Date_Form)
amphora3$DAT_max <- as.numeric(amphora3$Upper_Date_Form)

### Creation of dating steps (multiplies each object into a number of observations dependent on the object’s dating range)
amphora_steps3<-amphora3 %>% 
  select(AMINI_ID, Provenance_1, DAT_min, DAT_max) %>% 
  datsteps(stepsize=1)

### Addition of weights to assign equal importance to all dated forms ###
amphora_steps3<-scaleweight(amphora_steps3,var="all")
binwidth<-attributes(amphora_steps3)$stepsize 
scalevalue<-get.histogramscale(amphora_steps3,binwidth=binwidth)                                                                                                                                                                                                       

### Breakdown of Distribution by Contents ###
p3 <- ggplot(amphora_steps3,aes(x=DAT_step, colour=variable)) + geom_density(bw=5, aes(y = after_stat(count)), size=1.0, alpha = 0.25,) + Plot_Theme + Plot_Scale + scale_colour_manual(name  ="Provenance", labels = c("Adriatic Littoral", "Eastern Mediterranean", "North Africa", "Tyrrhenian Littoral"), values = c("steelblue2", "indianred2", "#81A88D", "mediumorchid3")) + labs(y="", legend="Provenance", subtitle = "Ariminum", attributes(amphora)$source,sep="") 


#### Ravenna Dates ####
amphora4 <- subset(amphora, Location_Specific=="Ravenna")
amphora4 <- amphora4[amphora4$Provenance_1 != "Not_Available", ]

### Remove forms without production dates ###
amphora4 <- amphora4[amphora4$Lower_Date_Form != "Not_Available", ]
amphora4 <- amphora4[amphora4$Upper_Date_Form != "Not_Available", ]

### Make production start and end dates numeric ###
amphora4$DAT_min <- as.numeric(amphora4$Lower_Date_Form)
amphora4$DAT_max <- as.numeric(amphora4$Upper_Date_Form)

### Creation of dating steps (multiplies each object into a number of observations dependent on the object’s dating range)
amphora_steps4<-amphora4 %>% 
  select(AMINI_ID, Provenance_1, DAT_min, DAT_max) %>% 
  datsteps(stepsize=1)

### Addition of weights to assign equal importance to all dated forms ###
amphora_steps4<-scaleweight(amphora_steps4,var="all")
binwidth<-attributes(amphora_steps4)$stepsize 
scalevalue<-get.histogramscale(amphora_steps4,binwidth=binwidth)                                                                                                                                                                                                       

### Breakdown of Distribution by Contents ###
p4 <- ggplot(amphora_steps4,aes(x=DAT_step, colour=variable)) + geom_density(bw=5, aes(y = after_stat(count)), size=1.0, alpha = 0.25,) + Plot_Theme + Plot_Scale + scale_colour_manual(name  ="Provenance", labels = c("Adriatic Littoral", "Eastern Mediterranean", "Gaul", "Tyrrhenian Littoral"), values = c("steelblue2", "indianred2", "sandybrown", "mediumorchid3")) + labs(y="Frequency", legend="Provenance", subtitle = "Ravenna", attributes(amphora)$source,sep="") + theme(legend.position = "none")


#### Classis Dates ####
amphora5 <- subset(amphora, Location_Specific=="Classis")
amphora5 <- amphora5[amphora5$Provenance_1 != "Not_Available", ]

### Remove forms without production dates ###
amphora5 <- amphora5[amphora5$Lower_Date_Form != "Not_Available", ]
amphora5 <- amphora5[amphora5$Upper_Date_Form != "Not_Available", ]

### Make production start and end dates numeric ###
amphora5$DAT_min <- as.numeric(amphora5$Lower_Date_Form)
amphora5$DAT_max <- as.numeric(amphora5$Upper_Date_Form)

### Creation of dating steps (multiplies each object into a number of observations dependent on the object’s dating range)
amphora_steps5<-amphora5 %>% 
  select(AMINI_ID, Provenance_1, DAT_min, DAT_max) %>% 
  datsteps(stepsize=1)

### Addition of weights to assign equal importance to all dated forms ###
amphora_steps5<-scaleweight(amphora_steps5,var="all")
binwidth<-attributes(amphora_steps5)$stepsize 
scalevalue<-get.histogramscale(amphora_steps5,binwidth=binwidth)                                                                                                                                                                                                       

### Breakdown of Distribution by Contents ###
p5 <- ggplot(amphora_steps5,aes(x=DAT_step, colour=variable)) + geom_density(bw=5, aes(y = after_stat(count)), size=1.0, alpha = 0.25,) + Plot_Theme + Plot_Scale + scale_colour_manual(name  ="Provenance", labels = c("Eastern Mediterranean", "North Africa", "Tyrrhenian Littoral"), values = c("indianred2", "#81A88D", "mediumorchid3")) + labs(y="Frequency", legend="Provenance", subtitle = "Classis", attributes(amphora)$source,sep="") + theme(legend.position = "none")


#### Luna Dates ####
amphora6 <- subset(amphora, Location_Specific=="Luna")
amphora6 <- amphora6[amphora6$Provenance_1 != "Not_Available", ]

### Remove forms without production dates ###
amphora6 <- amphora6[amphora6$Lower_Date_Form != "Not_Available", ]
amphora6 <- amphora6[amphora6$Upper_Date_Form != "Not_Available", ]

### Make production start and end dates numeric ###
amphora6$DAT_min <- as.numeric(amphora6$Lower_Date_Form)
amphora6$DAT_max <- as.numeric(amphora6$Upper_Date_Form)

### Creation of dating steps (multiplies each object into a number of observations dependent on the object’s dating range)
amphora_steps6<-amphora6 %>% 
  select(AMINI_ID, Provenance_1, DAT_min, DAT_max) %>% 
  datsteps(stepsize=1)

### Addition of weights to assign equal importance to all dated forms ###
amphora_steps6<-scaleweight(amphora_steps6,var="all")
binwidth<-attributes(amphora_steps6)$stepsize 
scalevalue<-get.histogramscale(amphora_steps6,binwidth=binwidth)                                                                                                                                                                                                       

### Breakdown of Distribution by Contents ###
p6 <- ggplot(amphora_steps6,aes(x=DAT_step, colour=variable)) + geom_density(bw=5, aes(y = after_stat(count)), size=1.0, alpha = 0.25,) + Plot_Theme + Plot_Scale + scale_colour_manual(name  ="Provenance", labels = c("Adriatic Littoral", "Eastern Mediterranean", "Gaul", "Iberian Peninsula", "North Africa", "Tyrrhenian Littoral"), values = c("steelblue2", "indianred2", "sandybrown", "khaki3", "#81A88D", "mediumorchid3")) + labs(y="", legend="Provenance", subtitle = "Luna", attributes(amphora)$source,sep="") 


#### Plot Combination ####
Ports <- (p2 + p1)/(p5 + p3)/(p4 + p6) 
Ports
