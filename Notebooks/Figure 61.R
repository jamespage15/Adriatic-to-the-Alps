#### Figure 61 ####

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

#### Amphora ####

#### Plot 1 ####

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
p1 <- ggplot(amphora_steps2,aes(x=DAT_step,))+ geom_density(bw=5, aes(y = after_stat(count)), alpha=0.9, fill="sandybrown") + labs(title="Production Dates", subtitle="A - Amphorae", y="Frequency", ) + Plot_Scale + Plot_Theme

#### Plot 2 ####

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
p2 <- ggplot(amphora_steps4,aes(x=DAT_step,))+ geom_density(bw=5, aes(y = after_stat(count)), alpha=0.9, fill="indianred") + labs(y="", title="Deposition Dates", subtitle = "B - Amphorae",) + Plot_Scale + Plot_Theme

#### Finewares ####

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
p3 <- ggplot(fineware_steps1,aes(x=DAT_step,))+ geom_density(bw=5, aes(y = after_stat(count)), alpha=0.9, fill="sandybrown") + labs(subtitle="C - Finewares", y="Frequency", ) + Plot_Scale + Plot_Theme


#### Plot 4 ####

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
p4 <- ggplot(fineware_steps2,aes(x=DAT_step,))+ geom_density(bw=5, aes(y = after_stat(count)), alpha=0.9, fill="indianred") + labs(y="", subtitle="D - Finewares",) + Plot_Scale + Plot_Theme

#### Plot Combination ####
TotalDistribution <- (p1 + p2)/(p3 + p4)   
TotalDistribution