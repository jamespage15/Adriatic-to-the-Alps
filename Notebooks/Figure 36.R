#### Figure 36 ####

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

#### Frequency ####

#### Production Dates ####

#### Plot 1 ####

### Remove forms without production dates ###
fineware1 <- fineware[fineware$Lower_Date_Form != "Not_Available", ]
fineware1 <- fineware[fineware$Upper_Date_Form != "Not_Available", ]

### Make production start and end dates numeric ###
fineware1$DAT_min <- as.numeric(fineware1$Lower_Date_Form)
fineware1$DAT_max <- as.numeric(fineware1$Upper_Date_Form)

### Creation of dating steps (multiplies each object into a number of observations dependent on the object’s dating range)
fineware_steps1<-fineware1 %>% 
  select(REFINI_ID, Provenance_1, DAT_min, DAT_max) %>% 
  datsteps(stepsize=1)

### Addition of weights to assign equal importance to all dated forms ###
fineware_steps1<-scaleweight(fineware_steps1,var="all")

### Plotting of Fineware Distribution across Northern Italy ###
p1 <- ggplot(fineware_steps1,aes(x=DAT_step,))+ geom_density(bw=5, aes(y = after_stat(count)), alpha=0.9, fill="sandybrown") + labs(title="Production Dates", subtitle="A - Quantities", y="Frequency", ) + Plot_Scale + Plot_Theme


#### Deposition Dates ####

#### Plot 2 ####

### Remove forms without production dates ###
fineware2 <- fineware[fineware$Deposit_ID...3 != "5", ]
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

### Make site start and end dates numeric ###
fineware2$DAT_min <- as.numeric(fineware2$Lower_Date_Deposit)
fineware2$DAT_max <- as.numeric(fineware2$Upper_Date_Deposit)

### Creation of dating steps (multiplies each object into a number of observations dependent on the object’s dating range)
fineware_steps2<-fineware2 %>% 
  select(REFINI_ID, Provenance_1, DAT_min, DAT_max) %>% 
  datsteps(stepsize=1)

### Addition of weights to assign equal importance to all dated forms ###
fineware_steps2<-scaleweight(fineware_steps2,var="all")

### Plotting of Fineware Distribution across Northern Italy ###
p2 <- ggplot(fineware_steps2,aes(x=DAT_step,))+ geom_density(bw=5, aes(y = after_stat(count)), alpha=0.9, fill="indianred") + labs(y="", title="Deposition Dates", subtitle="B - Quantities",) + Plot_Scale + Plot_Theme

#### Diversity ####

#### Production Dates ####

#### Plot 3 ####

### Remove forms without production dates ###
fineware3 <- fineware[fineware$Lower_Date_Form != "Not_Available", ]
fineware3 <- fineware3[fineware3$Upper_Date_Form != "Not_Available", ]

### Make production start and end dates numeric ###
fineware3$DAT_min <- as.numeric(fineware3$Lower_Date_Form)
fineware3$DAT_max <- as.numeric(fineware3$Upper_Date_Form)

### Creation of dating steps (multiplies each object into a number of observations dependent on the object’s dating range)
fineware_steps3<-fineware3 %>% 
  select(Standard_Form_ID...2, Provenance_1, DAT_min, DAT_max) %>% 
  datsteps(stepsize=1)

### Addition of weights to assign equal importance to all dated forms ###
fineware_steps3<-scaleweight(fineware_steps3,var="all")

### Remove Duplicates ###
fineware_steps3 <- fineware_steps3[!duplicated(fineware_steps3), ]

### Plotting Overall Diversity ###
p3 <- ggplot(fineware_steps3,aes(x=DAT_step,))+ geom_density(bw=5, aes(y = after_stat(count)), alpha=0.9, fill="sandybrown") + labs(subtitle="C - Diversity", y="Frequency", ) + Plot_Scale + Plot_Theme


#### Deposition Dates ####

#### Plot 4 ####

### Remove forms without deposition dates ###
fineware4 <- fineware[fineware$Lower_Date_Deposit != "Not_Available", ]
fineware4 <- fineware4[fineware4$Upper_Date_Deposit != "Not_Available", ]
fineware4 <- fineware4[fineware4$Deposit_ID...3 != "5", ]
fineware4 <- fineware4[fineware4$Deposit_ID...3 != "7", ]
fineware4 <- fineware4[fineware4$Deposit_ID...3 != "11", ]
fineware4 <- fineware4[fineware4$Deposit_ID...3 != "12", ]
fineware4 <- fineware4[fineware4$Deposit_ID...3 != "15", ]
fineware4 <- fineware4[fineware4$Deposit_ID...3 != "24", ]
fineware4 <- fineware4[fineware4$Deposit_ID...3 != "25", ]
fineware4 <- fineware4[fineware4$Deposit_ID...3 != "26", ]
fineware4 <- fineware4[fineware4$Deposit_ID...3 != "29", ]
fineware4 <- fineware4[fineware4$Deposit_ID...3 != "30", ]
fineware4 <- fineware4[fineware4$Deposit_ID...3 != "31", ]
fineware4 <- fineware4[fineware4$Deposit_ID...3 != "32", ]
fineware4 <- fineware4[fineware4$Deposit_ID...3 != "36", ]
fineware4 <- fineware4[fineware4$Deposit_ID...3 != "37", ]
fineware4 <- fineware4[fineware4$Deposit_ID...3 != "38", ]
fineware4 <- fineware4[fineware4$Deposit_ID...3 != "39", ]
fineware4 <- fineware4[fineware4$Deposit_ID...3 != "47", ]
fineware4 <- fineware4[fineware4$Deposit_ID...3 != "51", ]
fineware4 <- fineware4[fineware4$Deposit_ID...3 != "52", ]
fineware4 <- fineware4[fineware4$Deposit_ID...3 != "53", ]
fineware4 <- fineware4[fineware4$Deposit_ID...3 != "54", ]
fineware4 <- fineware4[fineware4$Deposit_ID...3 != "61", ]
fineware4 <- fineware4[fineware4$Deposit_ID...3 != "62", ]
fineware4 <- fineware4[fineware4$Deposit_ID...3 != "88", ]
fineware4 <- fineware4[fineware4$Deposit_ID...3 != "89", ]
fineware4 <- fineware4[fineware4$Deposit_ID...3 != "90", ]
fineware4 <- fineware4[fineware4$Deposit_ID...3 != "91", ]
fineware4 <- fineware4[fineware4$Deposit_ID...3 != "92", ]
fineware4 <- fineware4[fineware4$Deposit_ID...3 != "94", ]
fineware4 <- fineware4[fineware4$Deposit_ID...3 != "96", ]
fineware4 <- fineware4[fineware4$Deposit_ID...3 != "99", ]
fineware4 <- fineware4[fineware4$Deposit_ID...3 != "101", ]
fineware4 <- fineware4[fineware4$Deposit_ID...3 != "102", ]
fineware4 <- fineware4[fineware4$Deposit_ID...3 != "136", ]
fineware4 <- fineware4[fineware4$Deposit_ID...3 != "140", ]
fineware4 <- fineware4[fineware4$Deposit_ID...3 != "141", ]
fineware4 <- fineware4[fineware4$Deposit_ID...3 != "142", ]
fineware4 <- fineware4[fineware4$Deposit_ID...3 != "143", ]
fineware4 <- fineware4[fineware4$Deposit_ID...3 != "144", ]
fineware4 <- fineware4[fineware4$Deposit_ID...3 != "145", ]
fineware4 <- fineware4[fineware4$Deposit_ID...3 != "146", ]
fineware4 <- fineware4[fineware4$Deposit_ID...3 != "147", ]
fineware4 <- fineware4[fineware4$Deposit_ID...3 != "148", ]
fineware4 <- fineware4[fineware4$Deposit_ID...3 != "149", ]
fineware4 <- fineware4[fineware4$Deposit_ID...3 != "156", ]
fineware4 <- fineware4[fineware4$Deposit_ID...3 != "157", ]
fineware4 <- fineware4[fineware4$Deposit_ID...3 != "158", ]
fineware4 <- fineware4[fineware4$Deposit_ID...3 != "159", ]
fineware4 <- fineware4[fineware4$Deposit_ID...3 != "160", ]

### Make production start and end dates numeric ###
fineware4$DAT_min <- as.numeric(fineware4$Lower_Date_Deposit)
fineware4$DAT_max <- as.numeric(fineware4$Upper_Date_Deposit)

### Creation of dating steps (multiplies each object into a number of observations dependent on the object’s dating range)
fineware_steps4<-fineware4 %>% 
  select(Standard_Form_ID...2, Provenance_1, DAT_min, DAT_max) %>% 
  datsteps(stepsize=1)

### Addition of weights to assign equal importance to all dated forms ###
fineware_steps4<-scaleweight(fineware_steps4,var="all")

### Remove Duplicates ###
fineware_steps4 <- fineware_steps4[!duplicated(fineware_steps4[c(1,6)]),]

### Plotting Overall Diversity ###
p4 <- ggplot(fineware_steps4,aes(x=DAT_step,))+ geom_density(bw=5, aes(y = after_stat(count)), alpha=0.9, fill="indianred") + labs(subtitle="D - Diversity", y="", ) + Plot_Scale + Plot_Theme

#### Plot Combination ####
Chronologies <- (p1 + p2)/(p3 + p4)
Chronologies
