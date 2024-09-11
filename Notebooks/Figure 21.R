#### Figure 21 ####

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

#### Production Dates ####

### Contents Analysis: Wine ####

amphora1 <- subset(amphora, Contents=="Wine")
amphora1 <- amphora1[amphora1$Location_Specific != "Classis", ]
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
p1 <- ggplot(amphora_steps1,aes(x=DAT_step, colour=variable)) + geom_density(bw=5, aes(y = after_stat(count)), size=1.0, alpha = 0.25,) + Plot_Theme + Plot_Scale + scale_colour_manual(name  ="Provenance", labels = c("Adriatic Littoral", "Eastern Mediterranean", "Gaul", "Iberian Peninsula", "North Africa", "Tyrrhenian Littoral"), values = c("steelblue2", "indianred2", "sandybrown", "khaki3", "#81A88D", "mediumorchid3")) + labs(y="Frequency", legend="Provenance", title="Production Dates", subtitle = "Wine - A", attributes(amphora)$source,sep="") + theme(legend.position = "none")

#### Contents Analysis: Oil ####

amphora2 <- subset(amphora, Contents=="Oil")
amphora2 <- amphora2[amphora2$Location_Specific != "Classis", ]
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
p2 <- ggplot(amphora_steps2,aes(x=DAT_step, colour=variable)) + geom_density(bw=5, aes(y = after_stat(count)), size=1.0, alpha = 0.25,) + Plot_Theme + Plot_Scale + scale_colour_manual(name  ="Provenance", labels = c("Adriatic Littoral", "Eastern Mediterranean", "Iberian Peninsula", "North Africa"), values = c("steelblue2", "indianred2", "khaki3", "#81A88D", "grey", "mediumorchid3")) + labs(y="Frequency", legend="Provenance", subtitle="Oil - C", attributes(amphora)$source,sep="") + theme(legend.position = "none")


#### Contents Analysis: Fish Products ####

amphora3 <- subset(amphora, Contents=="Fish_Products")
amphora3 <- amphora3[amphora3$Location_Specific != "Classis", ]
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
p3 <- ggplot(amphora_steps3,aes(x=DAT_step, colour=variable)) + geom_density(bw=5, aes(y = after_stat(count)), size=1.0, alpha = 0.25,) + Plot_Theme + Plot_Scale + scale_colour_manual(name  ="Provenance", labels = c("Adriatic Littoral", "Gaul", "Iberian Peninsula", "North Africa"), values = c("steelblue2", "sandybrown", "khaki3", "#81A88D", "grey", "mediumorchid3")) + labs(y="Frequency", legend="Provenance", subtitle="Fish Products - E", attributes(amphora)$source,sep="") + theme(legend.position = "none")

#### Deposition Dates ####

amphora4 <- amphora

#### Subsetting Deposition Dates ####
amphora4 <- amphora4[amphora4$Location_Specific != "Classis", ]
amphora4 <- amphora4[amphora4$Deposit_ID...3 != "11", ]
amphora4 <- amphora4[amphora4$Deposit_ID...3 != "16", ]
amphora4 <- amphora4[amphora4$Deposit_ID...3 != "38", ]
amphora4 <- amphora4[amphora4$Deposit_ID...3 != "46", ]
amphora4 <- amphora4[amphora4$Deposit_ID...3 != "50", ]
amphora4 <- amphora4[amphora4$Deposit_ID...3 != "51", ]
amphora4 <- amphora4[amphora4$Deposit_ID...3 != "52", ]
amphora4 <- amphora4[amphora4$Deposit_ID...3 != "53", ]
amphora4 <- amphora4[amphora4$Deposit_ID...3 != "60", ]
amphora4 <- amphora4[amphora4$Deposit_ID...3 != "61", ]
amphora4 <- amphora4[amphora4$Deposit_ID...3 != "87", ]
amphora4 <- amphora4[amphora4$Deposit_ID...3 != "103", ]
amphora4 <- amphora4[amphora4$Deposit_ID...3 != "105", ]
amphora4 <- amphora4[amphora4$Deposit_ID...3 != "106", ]
amphora4 <- amphora4[amphora4$Deposit_ID...3 != "107", ]
amphora4 <- amphora4[amphora4$Deposit_ID...3 != "108", ]
amphora4 <- amphora4[amphora4$Deposit_ID...3 != "109", ]
amphora4 <- amphora4[amphora4$Deposit_ID...3 != "110", ]
amphora4 <- amphora4[amphora4$Deposit_ID...3 != "111", ]
amphora4 <- amphora4[amphora4$Deposit_ID...3 != "112", ]
amphora4 <- amphora4[amphora4$Deposit_ID...3 != "113", ]
amphora4 <- amphora4[amphora4$Deposit_ID...3 != "139", ]
amphora4 <- amphora4[amphora4$Deposit_ID...3 != "140", ]
amphora4 <- amphora4[amphora4$Deposit_ID...3 != "144", ]
amphora4 <- amphora4[amphora4$Deposit_ID...3 != "145", ]
amphora4 <- amphora4[amphora4$Deposit_ID...3 != "198", ]
amphora4 <- amphora4[amphora4$Provenance_1 != "Not_Available", ]

#### Contents Analysis: Wine ####
amphora5 <- subset(amphora4, Contents=="Wine")

### Remove forms without production dates ###
amphora5 <- amphora5[amphora5$Lower_Date_Deposit != "Not_Available", ]
amphora5 <- amphora5[amphora5$Upper_Date_Deposit != "Not_Available", ]

### Make production start and end dates numeric ###
amphora5$DAT_min <- as.numeric(amphora5$Lower_Date_Deposit)
amphora5$DAT_max <- as.numeric(amphora5$Upper_Date_Deposit)

### Creation of dating steps (multiplies each object into a number of observations dependent on the object’s dating range)
amphora_steps5<-amphora5 %>% 
  select(AMINI_ID, Provenance_1, DAT_min, DAT_max) %>% 
  datsteps(stepsize=1)

### Addition of weights to assign equal importance to all dated forms ###
amphora_steps5<-scaleweight(amphora_steps5,var="all")
binwidth<-attributes(amphora_steps5)$stepsize 
scalevalue<-get.histogramscale(amphora_steps5,binwidth=binwidth)                                                                                                                                                                                                       

### Breakdown of Distribution by Contents ###
p4 <- ggplot(amphora_steps5,aes(x=DAT_step, colour=variable)) + geom_density(bw=5, aes(y = after_stat(count)), size=1.0, alpha = 0.25,) + Plot_Theme + Plot_Scale + scale_colour_manual(name  ="Provenance", labels = c("Adriatic Littoral", "Eastern Mediterranean", "Gaul", "Iberian Peninsula", "North Africa", "Tyrrhenian Littoral"), values = c("steelblue2", "indianred2", "sandybrown", "khaki3", "#81A88D", "mediumorchid3")) + labs(y="", legend="Provenance", title="Deposition Dates", subtitle = "Wine - B", attributes(amphora)$source,sep="") 

#### Contents Analysis: Oil ####

amphora6 <- subset(amphora4, Contents=="Oil")

### Remove forms without production dates ###
amphora6 <- amphora6[amphora6$Lower_Date_Deposit != "Not_Available", ]
amphora6 <- amphora6[amphora6$Upper_Date_Deposit != "Not_Available", ]

### Make production start and end dates numeric ###
amphora6$DAT_min <- as.numeric(amphora6$Lower_Date_Deposit)
amphora6$DAT_max <- as.numeric(amphora6$Upper_Date_Deposit)

### Creation of dating steps (multiplies each object into a number of observations dependent on the object’s dating range)
amphora_steps6<-amphora6 %>% 
  select(AMINI_ID, Provenance_1, DAT_min, DAT_max) %>% 
  datsteps(stepsize=1)

### Addition of weights to assign equal importance to all dated forms ###
amphora_steps6<-scaleweight(amphora_steps6,var="all")
binwidth<-attributes(amphora_steps6)$stepsize 
scalevalue<-get.histogramscale(amphora_steps6,binwidth=binwidth)                                                                                                                                                                                                       

### Breakdown of Distribution by Contents ###
p5 <- ggplot(amphora_steps6,aes(x=DAT_step, colour=variable)) + geom_density(bw=5, aes(y = after_stat(count)), size=1.0, alpha = 0.25,) + Plot_Theme + Plot_Scale + scale_colour_manual(name  ="Provenance", labels = c("Adriatic Littoral", "Eastern Mediterranean", "Iberian Peninsula", "North Africa"), values = c("steelblue2", "indianred2", "khaki3", "#81A88D", "grey", "mediumorchid3")) + labs(y="", legend="Provenance", subtitle="Oil - D", attributes(amphora)$source,sep="") 


#### Contents Analysis: Fish Products ####

amphora7 <- subset(amphora4, Contents=="Fish_Products")

### Remove forms without production dates ###
amphora7 <- amphora7[amphora7$Lower_Date_Deposit != "Not_Available", ]
amphora7 <- amphora7[amphora7$Upper_Date_Deposit != "Not_Available", ]

### Make production start and end dates numeric ###
amphora7$DAT_min <- as.numeric(amphora7$Lower_Date_Deposit)
amphora7$DAT_max <- as.numeric(amphora7$Upper_Date_Deposit)

### Creation of dating steps (multiplies each object into a number of observations dependent on the object’s dating range)
amphora_steps7<-amphora7 %>% 
  select(AMINI_ID, Provenance_1, DAT_min, DAT_max) %>% 
  datsteps(stepsize=1)

### Addition of weights to assign equal importance to all dated forms ###
amphora_steps7<-scaleweight(amphora_steps7,var="all")
binwidth<-attributes(amphora_steps7)$stepsize 
scalevalue<-get.histogramscale(amphora_steps7,binwidth=binwidth)                                                                                                                                                                                                       

### Breakdown of Distribution by Contents ###
p6 <- ggplot(amphora_steps7,aes(x=DAT_step, colour=variable)) + geom_density(bw=5, aes(y = after_stat(count)), size=1.0, alpha = 0.25,) + Plot_Theme + Plot_Scale + scale_colour_manual(name  ="Provenance", labels = c("Adriatic Littoral", "Gaul", "Iberian Peninsula", "North Africa"), values = c("steelblue2", "sandybrown", "khaki3", "#81A88D", "grey", "mediumorchid3")) + labs(y="", legend="Provenance", subtitle="Fish Products - F", attributes(amphora)$source,sep="") 

#### Plot Combination ####

ProvenanceCont <- (p1 + p4)/(p2 + p5)/(p3 + p6) 
ProvenanceCont



