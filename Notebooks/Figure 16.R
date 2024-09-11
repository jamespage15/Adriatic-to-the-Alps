#### Figure 16 ####

#### Packages ####
library(datplot)
library(dplyr)
library(ggplot2)
library(patchwork)

#### Import Data ####
amphora <- read.csv("amphora.csv")

#### Plot Theme ####
Plot_Theme<-theme(panel.background =element_blank(), panel.grid.major =element_line("dimgray",size=0.5, linetype="dashed"), panel.grid.minor=element_line("grey",size=0.5, linetype="dotted"), panel.border = element_rect(colour = "black", fill=NA, size=1))
Plot_Scale<-scale_x_continuous(breaks=seq(from=-400,to=900,by=100), limits=c(-400,900),name="")

#### Production Dates ####

#### Plot 1 ####

### Remove forms without production dates ###
amphora1 <- amphora[amphora$Lower_Date_Form != "Not_Available", ]
amphora1 <- amphora[amphora$Upper_Date_Form != "Not_Available", ]

### Make production start and end dates numeric ###
amphora1$DAT_min <- as.numeric(amphora1$Lower_Date_Form)
amphora1$DAT_max <- as.numeric(amphora1$Upper_Date_Form)

### Creation of dating steps (multiplies each object into a number of observations dependent on the object’s dating range)
amphora_steps<-amphora1 %>% 
  select(AMINI_ID, Provenance_1, DAT_min, DAT_max) %>% 
  datsteps(stepsize=1)

### Addition of weights to assign equal importance to all dated forms ###
amphora_steps<-scaleweight(amphora_steps,var="all")

### Plotting of Amphora Distribution across Northern Italy ###
p1 <- ggplot(amphora_steps,aes(x=DAT_step,))+ geom_density(bw=5, aes(y = after_stat(count)), alpha=0.9, fill="sandybrown") + labs(title="Production Dates", subtitle="A", y="Frequency", ) + Plot_Scale + Plot_Theme

#### Plot 2 ####

### Remove forms without production dates ###
amphora2 <- amphora[amphora$Lower_Date_Form != "Not_Available", ]
amphora2 <- amphora2[amphora2$Upper_Date_Form != "Not_Available", ]

### Remove Classis ###
amphora2 <- amphora2[amphora2$Location_Specific != "Classis", ]

### Make production start and end dates numeric ###
amphora2$DAT_min <- as.numeric(amphora2$Lower_Date_Form)
amphora2$DAT_max <- as.numeric(amphora2$Upper_Date_Form)

### Creation of dating steps (multiplies each object into a number of observations dependent on the object’s dating range)
amphora_steps2<-amphora2 %>% 
  select(AMINI_ID, Provenance_1, DAT_min, DAT_max) %>% 
  datsteps(stepsize=1)

### Addition of weights to assign equal importance to all dated forms ###
amphora_steps2<-scaleweight(amphora_steps2,var="all")

### Plotting of Amphora Distribution across Northern Italy ###
p2 <- ggplot(amphora_steps2,aes(x=DAT_step,))+ geom_density(bw=5, aes(y = after_stat(count)), alpha=0.9, fill="sandybrown") + labs(subtitle="C (excluding Classis)", y="Frequency", ) + Plot_Scale + Plot_Theme

#### Deposition Dates ####

#### Plot 3 ####

### Remove forms without production dates ###
amphora3 <- amphora[amphora$Deposit_ID...3 != "11", ]
amphora3 <- amphora3[amphora3$Deposit_ID...3 != "16", ]
amphora3 <- amphora3[amphora3$Deposit_ID...3 != "38", ]
amphora3 <- amphora3[amphora3$Deposit_ID...3 != "46", ]
amphora3 <- amphora3[amphora3$Deposit_ID...3 != "50", ]
amphora3 <- amphora3[amphora3$Deposit_ID...3 != "51", ]
amphora3 <- amphora3[amphora3$Deposit_ID...3 != "52", ]
amphora3 <- amphora3[amphora3$Deposit_ID...3 != "53", ]
amphora3 <- amphora3[amphora3$Deposit_ID...3 != "60", ]
amphora3 <- amphora3[amphora3$Deposit_ID...3 != "61", ]
amphora3 <- amphora3[amphora3$Deposit_ID...3 != "87", ]
amphora3 <- amphora3[amphora3$Deposit_ID...3 != "103", ]
amphora3 <- amphora3[amphora3$Deposit_ID...3 != "105", ]
amphora3 <- amphora3[amphora3$Deposit_ID...3 != "106", ]
amphora3 <- amphora3[amphora3$Deposit_ID...3 != "107", ]
amphora3 <- amphora3[amphora3$Deposit_ID...3 != "108", ]
amphora3 <- amphora3[amphora3$Deposit_ID...3 != "109", ]
amphora3 <- amphora3[amphora3$Deposit_ID...3 != "110", ]
amphora3 <- amphora3[amphora3$Deposit_ID...3 != "111", ]
amphora3 <- amphora3[amphora3$Deposit_ID...3 != "112", ]
amphora3 <- amphora3[amphora3$Deposit_ID...3 != "113", ]
amphora3 <- amphora3[amphora3$Deposit_ID...3 != "139", ]
amphora3 <- amphora3[amphora3$Deposit_ID...3 != "140", ]
amphora3 <- amphora3[amphora3$Deposit_ID...3 != "144", ]
amphora3 <- amphora3[amphora3$Deposit_ID...3 != "145", ]
amphora3 <- amphora3[amphora3$Deposit_ID...3 != "198", ]

### Make site start and end dates numeric ###
amphora3$DAT_min <- as.numeric(amphora3$Lower_Date_Deposit)
amphora3$DAT_max <- as.numeric(amphora3$Upper_Date_Deposit)

### Creation of dating steps (multiplies each object into a number of observations dependent on the object’s dating range)
amphora_steps3<-amphora3 %>% 
  select(AMINI_ID, Provenance_1, DAT_min, DAT_max) %>% 
  datsteps(stepsize=1)

### Addition of weights to assign equal importance to all dated forms ###
amphora_steps3<-scaleweight(amphora_steps3,var="all")

### Plotting of Amphora Distribution across Northern Italy ###
p3 <- ggplot(amphora_steps3,aes(x=DAT_step,))+ geom_density(bw=5, aes(y = after_stat(count)), alpha=0.9, fill="indianred") + labs(y="", title="Deposition Dates", subtitle="B",) + Plot_Scale + Plot_Theme

#### Plot 4 ####

### Removal of Classis ###
amphora4 <- amphora[amphora$Location_Specific != "Classis", ]

### Remove forms without production dates ###
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

### Make production start and end dates numeric ###
amphora4$DAT_min <- as.numeric(amphora4$Lower_Date_Deposit)
amphora4$DAT_max <- as.numeric(amphora4$Upper_Date_Deposit)

### Creation of dating steps (multiplies each object into a number of observations dependent on the object’s dating range)
amphora_steps4<-amphora4 %>% 
  select(AMINI_ID, Provenance_1, DAT_min, DAT_max) %>% 
  datsteps(stepsize=1)

### Addition of weights to assign equal importance to all dated forms ###
amphora_steps4<-scaleweight(amphora_steps4,var="all")

### Plotting of Amphora Distribution across Northern Italy ###
p4 <- ggplot(amphora_steps4,aes(x=DAT_step,))+ geom_density(bw=5, aes(y = after_stat(count)), alpha=0.9, fill="indianred") + labs(y="", subtitle = "D (excluding Classis)",) + Plot_Scale + Plot_Theme

#### Plot Combination ####
TotalDistribution <- (p1 + p3)/(p2 + p4)   
TotalDistribution
