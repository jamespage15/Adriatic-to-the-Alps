#### Figure 19 ####

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

#### Frequency ####

#### Plot 1 ####

### Remove forms without production dates ###
amphora1 <- amphora[amphora$Lower_Date_Form != "Not_Available", ]
amphora1 <- amphora1[amphora1$Upper_Date_Form != "Not_Available", ]

### Removal of vessels without a provenance ###
amphora1 <- amphora1[amphora1$Provenance_1 != "Not_Available", ]

### Removal of Classis ###
amphora1 <- amphora1[amphora1$Location_Specific != "Classis", ]

### Make production start and end dates numeric ###
amphora1$DAT_min <- as.numeric(amphora1$Lower_Date_Form)
amphora1$DAT_max <- as.numeric(amphora1$Upper_Date_Form)

### Creation of dating steps (multiplies each object into a number of observations dependent on the object’s dating range)
amphora_steps1<-amphora1 %>% 
  select(AMINI_ID, Provenance_1, DAT_min, DAT_max) %>% 
  datsteps(stepsize=5)

### Addition of weights to assign equal importance to all dated forms ###
amphora_steps1<-scaleweight(amphora_steps1,var="all")

### Breakdown of Distribution ###
binwidth<-attributes(amphora_steps1)$stepsize 
scalevalue<-get.histogramscale(amphora_steps1,binwidth=binwidth) 

### Breakdown of Distribution by Provenance ###
p1 <- ggplot(amphora_steps1,aes(x=DAT_step, colour=variable), show.legend = FALSE) + geom_density(bw=5, aes(y = after_stat(count)), size=1.0, alpha = 0.25,) + Plot_Theme + Plot_Scale + scale_colour_manual(name  ="Provenance", labels = c("Adriatic Littoral", "Eastern Mediterranean", "Gaul", "Iberian Peninsula", "North Africa", "Tyrrhenian Littoral"), values = c("steelblue2", "indianred2", "sandybrown", "khaki3", "#81A88D", "mediumorchid3")) + labs(y="Frequency", legend="Provenance", title="Production Dates", subtitle = "A - Quantities", legend="Provenance", attributes(amphora)$source,sep="") + theme(legend.position = "none")

#### Plot 2 ####

### Removal of Classis ###
amphora2 <- amphora[amphora$Location_Specific != "Classis", ]

### Removal of vessels without a provenance ###
amphora2 <- amphora2[amphora2$Provenance_1 != "Not_Available", ]

### Remove forms without deposit dates ###
amphora2 <- amphora2[amphora2$Deposit_ID...3 != "11", ]
amphora2 <- amphora2[amphora2$Deposit_ID...3 != "16", ]
amphora2 <- amphora2[amphora2$Deposit_ID...3 != "38", ]
amphora2 <- amphora2[amphora2$Deposit_ID...3 != "46", ]
amphora2 <- amphora2[amphora2$Deposit_ID...3 != "50", ]
amphora2 <- amphora2[amphora2$Deposit_ID...3 != "51", ]
amphora2 <- amphora2[amphora2$Deposit_ID...3 != "52", ]
amphora2 <- amphora2[amphora2$Deposit_ID...3 != "53", ]
amphora2 <- amphora2[amphora2$Deposit_ID...3 != "60", ]
amphora2 <- amphora2[amphora2$Deposit_ID...3 != "61", ]
amphora2 <- amphora2[amphora2$Deposit_ID...3 != "87", ]
amphora2 <- amphora2[amphora2$Deposit_ID...3 != "103", ]
amphora2 <- amphora2[amphora2$Deposit_ID...3 != "105", ]
amphora2 <- amphora2[amphora2$Deposit_ID...3 != "106", ]
amphora2 <- amphora2[amphora2$Deposit_ID...3 != "107", ]
amphora2 <- amphora2[amphora2$Deposit_ID...3 != "108", ]
amphora2 <- amphora2[amphora2$Deposit_ID...3 != "109", ]
amphora2 <- amphora2[amphora2$Deposit_ID...3 != "110", ]
amphora2 <- amphora2[amphora2$Deposit_ID...3 != "111", ]
amphora2 <- amphora2[amphora2$Deposit_ID...3 != "112", ]
amphora2 <- amphora2[amphora2$Deposit_ID...3 != "113", ]
amphora2 <- amphora2[amphora2$Deposit_ID...3 != "139", ]
amphora2 <- amphora2[amphora2$Deposit_ID...3 != "140", ]
amphora2 <- amphora2[amphora2$Deposit_ID...3 != "144", ]
amphora2 <- amphora2[amphora2$Deposit_ID...3 != "145", ]
amphora2 <- amphora2[amphora2$Deposit_ID...3 != "198", ]

### Make production start and end dates numeric ###
amphora2$DAT_min <- as.numeric(amphora2$Lower_Date_Deposit)
amphora2$DAT_max <- as.numeric(amphora2$Upper_Date_Deposit)

### Creation of dating steps (multiplies each object into a number of observations dependent on the object’s dating range)
amphora_steps2<-amphora2 %>% 
  select(AMINI_ID, Provenance_1, DAT_min, DAT_max) %>% 
  datsteps(stepsize="auto")

### Addition of weights to assign equal importance to all dated forms ###
amphora_steps2<-scaleweight(amphora_steps2,var="all")

### Breakdown of Distribution by Provenance ###
p2 <- ggplot(amphora_steps2,aes(x=DAT_step, colour=variable)) + geom_density(bw=5, aes(y = after_stat(count)), size=1.0, alpha = 0.25,) + Plot_Theme + Plot_Scale + scale_colour_manual(name  ="Provenance", labels = c("Adriatic Littoral", "Eastern Mediterranean", "Gaul", "Iberian Peninsula", "North Africa", "Tyrrhenian Littoral"), values = c("steelblue2", "indianred2", "sandybrown", "khaki3", "#81A88D", "mediumorchid3")) + labs(y="", title = "Deposition Dates", legend="Provenance", subtitle="B - Quantities", legend="Provenance", attributes(amphora)$source,sep="") 

#### Diversity ####

#### Plot 3 ####

### Remove forms without production dates ###
amphora3 <- amphora[amphora$Lower_Date_Form != "Not_Available", ]
amphora3 <- amphora3[amphora3$Upper_Date_Form != "Not_Available", ]

### Removal of Classis ###
amphora3 <- amphora3[amphora3$Location_Specific != "Classis", ]

### Removal of amphorae without a provenance ###
amphora3 <- amphora3[amphora3$Provenance_1 != "Not_Available", ]

### Make production start and end dates numeric ###
amphora3$DAT_min <- as.numeric(amphora3$Lower_Date_Form)
amphora3$DAT_max <- as.numeric(amphora3$Upper_Date_Form)

### Creation of dating steps (multiplies each object into a number of observations dependent on the object’s dating range)
amphora_steps3<-amphora3 %>% 
  select(Standard_Form_ID...2, Provenance_1, DAT_min, DAT_max) %>% 
  datsteps(stepsize=1)

### Addition of weights to assign equal importance to all dated forms ###
amphora_steps3<-scaleweight(amphora_steps3,var="all")

### Remove Duplicates ###
amphora_steps3 <- amphora_steps3[!duplicated(amphora_steps3), ]

### Breakdown of Distribution by Provenance ###+ theme(legend.position = "none")
p3 <- ggplot(amphora_steps3,aes(x=DAT_step, colour=variable), show.legend = FALSE) + geom_density(bw=5, aes(y = after_stat(count)), size=1.0, alpha = 0.25,) + Plot_Theme + Plot_Scale + scale_colour_manual(name  ="Provenance", labels = c("Adriatic Littoral", "Eastern Mediterranean", "Gaul", "Iberian Peninsula", "North Africa", "Tyrrhenian Littoral"), values = c("steelblue2", "indianred2", "sandybrown", "khaki3", "#81A88D", "mediumorchid3")) + labs(y="Frequency", legend="Provenance", subtitle = "C - Diversity", legend="Provenance", attributes(amphora)$source,sep="") + theme(legend.position = "none")

#### Plot 4 ####

### Remove forms without deposit dates ###
amphora4 <- amphora[amphora$Lower_Date_Deposit != "Not_Available", ]
amphora4 <- amphora4[amphora4$Upper_Date_Deposit != "Not_Available", ]
amphora4 <- amphora4[amphora4$Location_Specific != "Classis", ]
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

### Removal of vessels without a provenance ###
amphora4 <- amphora4[amphora4$Provenance_1 != "Not_Available", ]

### Make production start and end dates numeric ###
amphora4$DAT_min <- as.numeric(amphora4$Lower_Date_Deposit)
amphora4$DAT_max <- as.numeric(amphora4$Upper_Date_Deposit)

### Creation of dating steps (multiplies each object into a number of observations dependent on the object’s dating range)
amphora_steps4<-amphora4 %>% 
  select(Standard_Form_ID...2, Provenance_1, DAT_min, DAT_max) %>% 
  datsteps(stepsize=1)

### Addition of weights to assign equal importance to all dated forms ###
amphora_steps4<-scaleweight(amphora_steps4,var="all")

### Remove Duplicates ###
amphora_steps4 <- amphora_steps4[!duplicated(amphora_steps4[c(1,6)]),]

### Breakdown of Distribution by Provenance ###
p4 <- ggplot(amphora_steps4,aes(x=DAT_step, colour=variable), show.legend = FALSE) + geom_density(bw=5, aes(y = after_stat(count)), size=1.0, alpha = 0.25,) + Plot_Theme + Plot_Scale + scale_colour_manual(name  ="Provenance", labels = c("Adriatic Littoral", "Eastern Mediterranean", "Gaul", "Iberian Peninsula", "North Africa", "Tyrrhenian Littoral"), values = c("steelblue2", "indianred2", "sandybrown", "khaki3", "#81A88D", "mediumorchid3")) + labs(y="", legend="Provenance", subtitle = "D - Diversity", legend="Provenance", attributes(amphora)$source,sep="") 


#### Plot Combination ####
Provenances <- (p1 + p2)/(p3 + p4)
Provenances