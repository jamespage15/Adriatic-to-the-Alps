#### Figure 30 ####

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

### Plot 1 ####

### Remove forms without production dates ###
amphora1 <- amphora[amphora$Lower_Date_Form != "Not_Available", ]
amphora1 <- amphora1[amphora1$Upper_Date_Form != "Not_Available", ]

### Removal of vessels without a provenance ###
amphora1 <- amphora1[amphora1$Provenance_1 != "Not_Available", ]

#### Site Selection ####
categories <- c("Forlì", "Este", "Padua", "Verona", "Vicenza", "Modena", "Clastidium", "Cremona", "Trento")
amphora1 <- amphora1[amphora1$Location_Specific %in% categories,]

### Make production start and end dates numeric ###
amphora1$DAT_min <- as.numeric(amphora1$Lower_Date_Form)
amphora1$DAT_max <- as.numeric(amphora1$Upper_Date_Form)

### Creation of dating steps (multiplies each object into a number of observations dependent on the object’s dating range)
amphora_steps1<-amphora1 %>% 
  select(AMINI_ID, Provenance_1, DAT_min, DAT_max) %>% 
  datsteps(stepsize=1)

### Addition of weights to assign equal importance to all dated forms ###
amphora_steps1<-scaleweight(amphora_steps1,var="all")

### Breakdown of Distribution ###
binwidth<-attributes(amphora_steps1)$stepsize 
scalevalue<-get.histogramscale(amphora_steps1,binwidth=binwidth)                                                                                                                                                                                                       

### Breakdown of Distribution by Provenance ###
p1 <- ggplot(amphora_steps1,aes(x=DAT_step, colour=variable), show.legend = FALSE) + geom_density(bw=5, aes(y = after_stat(count)), size=1.2, alpha = 0.25,) + Plot_Theme + Plot_Scale + scale_colour_manual(name  ="Provenance", labels = c("Adriatic Littoral", "Eastern Mediterranean", "Iberian Peninsula", "North Africa", "Tyrrhenian Littoral"), values = c("steelblue2", "indianred2", "khaki3", "#81A88D", "mediumorchid3")) + labs(y="Frequency", legend="Provenance", title="Zones of Consumption", subtitle = "Cluster 1", legend="Provenance", attributes(amphora)$source,sep="") 


### Plot 2 ####

### Remove forms without production dates ###
amphora2 <- amphora[amphora$Lower_Date_Form != "Not_Available", ]
amphora2 <- amphora2[amphora2$Upper_Date_Form != "Not_Available", ]

### Removal of vessels without a provenance ###
amphora2 <- amphora2[amphora2$Provenance_1 != "Not_Available", ]

#### Site Selection ####
categories <- c("Bedriacum", "Brescia", "Civitas Camunnorum", "Como", "Milan", "Industria", "Novara", "Oderzo")
amphora2 <- amphora2[amphora2$Location_Specific %in% categories,]

### Make production start and end dates numeric ###
amphora2$DAT_min <- as.numeric(amphora2$Lower_Date_Form)
amphora2$DAT_max <- as.numeric(amphora2$Upper_Date_Form)

### Creation of dating steps (multiplies each object into a number of observations dependent on the object’s dating range)
amphora_steps2<-amphora2 %>% 
  select(AMINI_ID, Provenance_1, DAT_min, DAT_max) %>% 
  datsteps(stepsize=1)

### Addition of weights to assign equal importance to all dated forms ###
amphora_steps2<-scaleweight(amphora_steps2,var="all")

### Breakdown of Distribution ###
binwidth<-attributes(amphora_steps2)$stepsize 
scalevalue<-get.histogramscale(amphora_steps2,binwidth=binwidth)                                                                                                                                                                                                       

### Breakdown of Distribution by Provenance ###
p2 <- ggplot(amphora_steps2,aes(x=DAT_step, colour=variable), show.legend = FALSE) + geom_density(bw=5, aes(y = after_stat(count)), size=1.2, alpha = 0.25,) + Plot_Theme + Plot_Scale + scale_colour_manual(name  ="Provenance", labels = c("Adriatic Littoral", "Eastern Mediterranean", "Gaul", "Iberian Peninsula", "North Africa", "Tyrrhenian Littoral"), values = c("steelblue2", "indianred2", "sandybrown", "khaki3", "#81A88D", "mediumorchid3")) + labs(y="Frequency", legend="Provenance", title="", subtitle = "Cluster 2", legend="Provenance", attributes(amphora)$source,sep="") 


### Plot 3 ####

### Remove forms without production dates ###
amphora3 <- amphora[amphora$Lower_Date_Form != "Not_Available", ]
amphora3 <- amphora3[amphora3$Upper_Date_Form != "Not_Available", ]

### Removal of vessels without a provenance ###
amphora3 <- amphora3[amphora3$Provenance_1 != "Not_Available", ]

#### Site Selection ####
categories <- c("Ivrea", "Vercelli", "Laus Pompeia", "Libarna", "Alba", "Augusta Bagiennorum")
amphora3 <- amphora3[amphora3$Location_Specific %in% categories,]

### Make production start and end dates numeric ###
amphora3$DAT_min <- as.numeric(amphora3$Lower_Date_Form)
amphora3$DAT_max <- as.numeric(amphora3$Upper_Date_Form)

### Creation of dating steps (multiplies each object into a number of observations dependent on the object’s dating range)
amphora_steps3<-amphora3 %>% 
  select(AMINI_ID, Provenance_1, DAT_min, DAT_max) %>% 
  datsteps(stepsize=1)

### Addition of weights to assign equal importance to all dated forms ###
amphora_steps3<-scaleweight(amphora_steps3,var="all")

### Breakdown of Distribution ###
binwidth<-attributes(amphora_steps3)$stepsize 
scalevalue<-get.histogramscale(amphora_steps3,binwidth=binwidth)                                                                                                                                                                                                       

### Breakdown of Distribution by Provenance ###
p3 <- ggplot(amphora_steps3,aes(x=DAT_step, colour=variable), show.legend = FALSE) + geom_density(bw=5, aes(y = after_stat(count)), size=1.2, alpha = 0.25,) + Plot_Theme + Plot_Scale + scale_colour_manual(name  ="Provenance", labels = c("Adriatic Littoral", "Eastern Mediterranean", "Gaul", "Iberian Peninsula", "North Africa", "Tyrrhenian Littoral"), values = c("steelblue2", "indianred2", "sandybrown", "khaki3", "#81A88D", "mediumorchid3")) + labs(y="Frequency", legend="Provenance", title="", subtitle = "Cluster 3", legend="Provenance", attributes(amphora)$source,sep="") 

#### Plot Combination ####
Zones <- (p1 / p2 / p3)
Zones
