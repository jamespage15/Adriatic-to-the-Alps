#### Figure 46 ####

#### Packages ####
library(datplot)
library(dplyr)
library(ggplot2)
library(patchwork)

#### Import Data ####
amphora <- read.csv("amphora.csv")
fineware <- read.csv("fineware.csv")

### Plot Theme ###
Plot_Theme<-theme(panel.background =element_blank(), panel.grid.major =element_line("dimgray",size=0.5, linetype="dashed"), panel.grid.minor=element_line("grey",size=0.5, linetype="dotted"), panel.border = element_rect(colour = "black", fill=NA, size=1))
Plot_Scale<-scale_x_continuous(breaks=seq(from=-300,to=800,by=100), limits=c(-300,800),name="")

#### Plot 1 ####

### Select North African amphorae ###
amphora1 <- subset(amphora, Provenance_1=="North_Africa")

### Remove forms without production dates ###
amphora1 <- amphora1[amphora1$Lower_Date_Form != "Not_Available", ]
amphora1 <- amphora1[amphora1$Upper_Date_Form != "Not_Available", ]

### Make production start and end dates numeric ###
amphora1$DAT_min <- as.numeric(amphora1$Lower_Date_Form)
amphora1$DAT_max <- as.numeric(amphora1$Upper_Date_Form)

### Creation of dating steps (multiplies each object into a number of observations dependent on the object’s dating range)
amphora_steps<-amphora1 %>% 
  select(AMINI_ID, Provenance_1, DAT_min, DAT_max) %>% 
  datsteps(stepsize=1)

### Addition of weights to assign equal importance to all dated forms ###
amphora_steps<-scaleweight(amphora_steps,var="all")

### Plotting of North African Amphora Distribution ###
p1 <- ggplot(amphora_steps,aes(x=DAT_step,))+ geom_density(bw=5, aes(y = after_stat(count)), alpha=0.9, fill="sandybrown") + labs(title="North African Amphorae", y="Frequency", ) + Plot_Scale + Plot_Theme

#### Plot 2 ####

### Select ARS ###
fineware1 <- subset(fineware, Provenance_1=="North_Africa")

### Remove forms without production dates ###
fineware1 <- fineware1[fineware1$Lower_Date_Form != "Not_Available", ]
fineware1 <- fineware1[fineware1$Upper_Date_Form != "Not_Available", ]

### Make production start and end dates numeric ###
fineware1$DAT_min <- as.numeric(fineware1$Lower_Date_Form)
fineware1$DAT_max <- as.numeric(fineware1$Upper_Date_Form)

### Creation of dating steps (multiplies each object into a number of observations dependent on the object’s dating range)
fineware_steps1<-fineware1 %>% 
  select(REFINI_ID, Provenance_1, DAT_min, DAT_max) %>% 
  datsteps(stepsize=1)

### Addition of weights to assign equal importance to all dated forms ###
fineware_steps1<-scaleweight(fineware_steps1,var="all")

### Plotting of ARS Distribution ###
p2 <- ggplot(fineware_steps1,aes(x=DAT_step,))+ geom_density(bw=5, aes(y = after_stat(count)), alpha=0.9, fill="sandybrown") + labs(title="African Red Slip Ware", y="Frequency", ) + Plot_Scale + Plot_Theme

#### Plot Combination ####
Chronologies <- (p1 / p2)
Chronologies