#### Figure 62 ####

#### Packages ####
library(datplot)
library(dplyr)
library(ggplot2)
library(patchwork)

#### Import Data ####
amphora <- read.csv("amphora.csv")
fineware <- read.csv("fineware.csv")

#### Plot Theme ####
Plot_Theme<-theme(panel.background =element_blank(), panel.grid.major =element_line("dimgray",size=0.5, linetype="dashed"), panel.grid.minor=element_line("grey",size=0.5, linetype="dotted"), panel.border = element_rect(colour = "black", fill=NA, size=1))
Plot_Scale<-scale_x_continuous(breaks=seq(from=-400,to=900,by=100), limits=c(-400,900),name="")

#### Amphorae ####

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
p1 <- ggplot(amphora_steps1,aes(x=DAT_step, colour=variable), show.legend = FALSE) + geom_density(bw=5, aes(y = after_stat(count)), size=1.0, alpha = 0.25,) + Plot_Theme + Plot_Scale + scale_colour_manual(name  ="Provenance", labels = c("Adriatic Littoral", "Eastern Mediterranean", "Gaul", "Iberian Peninsula", "North Africa", "Tyrrhenian Littoral"), values = c("steelblue2", "indianred4", "darkorange3", "khaki3", "#81A88D", "mediumorchid3")) + labs(y="Frequency", legend="Provenance", title="Production Dates", subtitle = "A - Amphorae", legend="Provenance", attributes(amphora)$source,sep="") + theme(legend.position = "none")

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
p2 <- ggplot(amphora_steps2,aes(x=DAT_step, colour=variable)) + geom_density(bw=5, aes(y = after_stat(count)), size=1.0, alpha = 0.25,) + Plot_Theme + Plot_Scale + scale_colour_manual(name  ="Provenance", labels = c("Adriatic Littoral", "Eastern Mediterranean", "Gaul", "Iberian Peninsula", "North Africa", "Tyrrhenian Littoral"), values = c("steelblue2", "indianred4", "darkorange3", "khaki3", "#81A88D", "mediumorchid3")) + labs(y="", title = "Deposition Dates", legend="Provenance", subtitle="B - Amphorae", legend="Provenance", attributes(amphora)$source,sep="") 


#### Finewares ####

#### Plot 3 ####

### Remove forms without production dates ###
fineware1 <- fineware[fineware$Lower_Date_Form != "Not_Available", ]
fineware1 <- fineware1[fineware1$Upper_Date_Form != "Not_Available", ]

### Removal of vessels without a provenance ###
fineware1 <- fineware1[fineware1$Provenance_1 != "Not_Available", ]

### Make production start and end dates numeric ###
fineware1$DAT_min <- as.numeric(fineware1$Lower_Date_Form)
fineware1$DAT_max <- as.numeric(fineware1$Upper_Date_Form)

### Creation of dating steps (multiplies each object into a number of observations dependent on the object’s dating range)
fineware_steps1<-fineware1 %>% 
  select(REFINI_ID, Provenance_1, DAT_min, DAT_max) %>% 
  datsteps(stepsize=1)

### Addition of weights to assign equal importance to all dated forms ###
fineware_steps1<-scaleweight(fineware_steps1,var="all")

### Breakdown of Distribution ###
binwidth<-attributes(fineware_steps1)$stepsize 
scalevalue<-get.histogramscale(fineware_steps1,binwidth=binwidth) 

### Breakdown of Distribution by Provenance ###
p3 <- ggplot(fineware_steps1,aes(x=DAT_step, colour=variable), show.legend = FALSE) + geom_density(bw=5, aes(y = after_stat(count)), size=1.0, alpha = 0.25,) + Plot_Theme + Plot_Scale + scale_colour_manual(name  ="Provenance", labels = c("Adriatic Littoral", "Central Italy", "Eastern Mediterranean", "Gaul", "North Africa", "Northern Italy"), values = c("steelblue2", "gold3", "indianred4", "darkorange3", "#81A88D", "mediumpurple4")) + labs(y="Frequency", legend="Provenance", subtitle = "C - Finewares", legend="Provenance", attributes(fineware)$source,sep="") + theme(legend.position = "none")

#### Plot 4 ####

### Remove forms without production dates ###
fineware2 <- fineware[fineware$Deposit_ID...3 != "1", ]
fineware2 <- fineware2[fineware2$Deposit_ID...3 != "5", ]
fineware2 <- fineware2[fineware2$Deposit_ID...3 != "7", ]
fineware2 <- fineware2[fineware2$Deposit_ID...3 != "11", ]
fineware2 <- fineware2[fineware2$Deposit_ID...3 != "12", ]
fineware2 <- fineware2[fineware2$Deposit_ID...3 != "15", ]
fineware2 <- fineware2[fineware2$Deposit_ID...3 != "24", ]
fineware2 <- fineware2[fineware2$Deposit_ID...3 != "25", ]
fineware2 <- fineware2[fineware2$Deposit_ID...3 != "26", ]
fineware2 <- fineware2[fineware2$Deposit_ID...3 != "29", ]
fineware2 <- fineware2[fineware2$Deposit_ID...3 != "30", ]
fineware2 <- fineware2[fineware2$Deposit_ID...3 != "31", ]
fineware2 <- fineware2[fineware2$Deposit_ID...3 != "32", ]
fineware2 <- fineware2[fineware2$Deposit_ID...3 != "36", ]
fineware2 <- fineware2[fineware2$Deposit_ID...3 != "37", ]
fineware2 <- fineware2[fineware2$Deposit_ID...3 != "38", ]
fineware2 <- fineware2[fineware2$Deposit_ID...3 != "39", ]
fineware2 <- fineware2[fineware2$Deposit_ID...3 != "47", ]
fineware2 <- fineware2[fineware2$Deposit_ID...3 != "51", ]
fineware2 <- fineware2[fineware2$Deposit_ID...3 != "52", ]
fineware2 <- fineware2[fineware2$Deposit_ID...3 != "53", ]
fineware2 <- fineware2[fineware2$Deposit_ID...3 != "54", ]
fineware2 <- fineware2[fineware2$Deposit_ID...3 != "61", ]
fineware2 <- fineware2[fineware2$Deposit_ID...3 != "62", ]
fineware2 <- fineware2[fineware2$Deposit_ID...3 != "88", ]
fineware2 <- fineware2[fineware2$Deposit_ID...3 != "89", ]
fineware2 <- fineware2[fineware2$Deposit_ID...3 != "90", ]
fineware2 <- fineware2[fineware2$Deposit_ID...3 != "91", ]
fineware2 <- fineware2[fineware2$Deposit_ID...3 != "92", ]
fineware2 <- fineware2[fineware2$Deposit_ID...3 != "94", ]
fineware2 <- fineware2[fineware2$Deposit_ID...3 != "96", ]
fineware2 <- fineware2[fineware2$Deposit_ID...3 != "99", ]
fineware2 <- fineware2[fineware2$Deposit_ID...3 != "101", ]
fineware2 <- fineware2[fineware2$Deposit_ID...3 != "102", ]
fineware2 <- fineware2[fineware2$Deposit_ID...3 != "136", ]
fineware2 <- fineware2[fineware2$Deposit_ID...3 != "140", ]
fineware2 <- fineware2[fineware2$Deposit_ID...3 != "141", ]
fineware2 <- fineware2[fineware2$Deposit_ID...3 != "142", ]
fineware2 <- fineware2[fineware2$Deposit_ID...3 != "143", ]
fineware2 <- fineware2[fineware2$Deposit_ID...3 != "144", ]
fineware2 <- fineware2[fineware2$Deposit_ID...3 != "145", ]
fineware2 <- fineware2[fineware2$Deposit_ID...3 != "146", ]
fineware2 <- fineware2[fineware2$Deposit_ID...3 != "147", ]
fineware2 <- fineware2[fineware2$Deposit_ID...3 != "148", ]
fineware2 <- fineware2[fineware2$Deposit_ID...3 != "149", ]
fineware2 <- fineware2[fineware2$Deposit_ID...3 != "156", ]
fineware2 <- fineware2[fineware2$Deposit_ID...3 != "157", ]
fineware2 <- fineware2[fineware2$Deposit_ID...3 != "158", ]
fineware2 <- fineware2[fineware2$Deposit_ID...3 != "159", ]
fineware2 <- fineware2[fineware2$Deposit_ID...3 != "160", ]

### Removal of vessels without a provenance ###
fineware2 <- fineware2[fineware2$Provenance_1 != "Not_Available", ]

### Make site start and end dates numeric ###
fineware2$DAT_min <- as.numeric(fineware2$Lower_Date_Deposit)
fineware2$DAT_max <- as.numeric(fineware2$Upper_Date_Deposit)

### Creation of dating steps (multiplies each object into a number of observations dependent on the object’s dating range)
fineware_steps2<-fineware2 %>% 
  select(REFINI_ID, Provenance_1, DAT_min, DAT_max) %>% 
  datsteps(stepsize=1)

### Addition of weights to assign equal importance to all dated forms ###
fineware_steps2<-scaleweight(fineware_steps2,var="all")

### Breakdown of Distribution by Provenance ###
p4 <- ggplot(fineware_steps2,aes(x=DAT_step, colour=variable)) + geom_density(bw=5, aes(y = after_stat(count)), size=1.0, alpha = 0.25,) + Plot_Theme + Plot_Scale + scale_colour_manual(name  ="Provenance", labels = c("Adriatic Littoral", "Central Italy", "Eastern Mediterranean", "Gaul", "North Africa", "Northern Italy"), values = c("steelblue2", "gold3", "indianred4", "darkorange3", "#81A88D", "mediumpurple4")) + labs(y="", legend="Provenance", subtitle="D - Finewares", legend="Provenance", attributes(fineware)$source,sep="") 

#### Plot Combination ####
Provenances <- (p1 + p2)/(p3 + p4)
Provenances

