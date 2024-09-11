#### Figure 18 ####

#### Packages ####

library(datplot)
library(dplyr)
library(ggplot2)
library(patchwork)

#### Import Data ####
amphora <- read.csv("amphora.csv")

#### Production Dates ####
amphora1 <- amphora

### Remove forms without production dates ###
amphora1 <- amphora1[amphora1$Lower_Date_Form != "Not_Available", ]
amphora1 <- amphora1[amphora1$Upper_Date_Form != "Not_Available", ]
amphora1 <- amphora1[amphora1$Location_Specific != "Classis", ]

### Make production start and end dates numeric ###
amphora1$DAT_min <- as.numeric(amphora1$Lower_Date_Form)
amphora1$DAT_max <- as.numeric(amphora1$Upper_Date_Form)

### Creation of dating steps (multiplies each object into a number of observations dependent on the object’s dating range)
amphora_steps1<-amphora1 %>% 
  select(Standard_Form_ID...2, Provenance_1, DAT_min, DAT_max) %>% 
  datsteps(stepsize=1)

### Addition of weights to assign equal importance to all dated forms ###
amphora_steps1<-scaleweight(amphora_steps1,var="all")

### Remove Duplicates ###
amphora_steps1 <- amphora_steps1[!duplicated(amphora_steps1), ]

### Plotting Overall Diveristy ###
p1 <- ggplot(amphora_steps1,aes(x=DAT_step,))+ geom_density(bw=5, aes(y = after_stat(count)), alpha=0.9, fill="sandybrown") + labs(subtitle="A - Production Dates", y="Frequency", ) + Plot_Scale + Plot_Theme


#### Deposition Dates ####
amphora2 <- amphora

### Remove forms without deposition dates ###
amphora2 <- amphora2[amphora2$Lower_Date_Deposit != "Not_Available", ]
amphora2 <- amphora2[amphora2$Upper_Date_Deposit != "Not_Available", ]
amphora2 <- amphora2[amphora2$Location_Specific != "Classis", ]
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
  select(Standard_Form_ID...2, Provenance_1, DAT_min, DAT_max) %>% 
  datsteps(stepsize=1)

### Addition of weights to assign equal importance to all dated forms ###
amphora_steps2<-scaleweight(amphora_steps2,var="all")

### Remove Duplicates ###
amphora_steps2 <- amphora_steps2[!duplicated(amphora_steps2[c(1,6)]),]

### Plotting Overall Diveristy ###
p2 <- ggplot(amphora_steps2,aes(x=DAT_step,))+ geom_density(bw=5, aes(y = after_stat(count)), alpha=0.9, fill="indianred") + labs(subtitle="B- Deposition Dates", y="Frequency", ) + Plot_Scale + Plot_Theme

### Graph Combination ###

TotalDiversity <- (p1 / p2)
TotalDiversity
