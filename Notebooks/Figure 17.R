#### Figure 17 ####

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

#### Reclamation Deposits ####

### Subset Reclamation Deposits ###
categories <- c("Amphora reclamation deposit", "Group of amphora used to infill a large hole in the area occupied by Parco Novi Sad.", "Amphora reclamation deposit and roadway", "Amphora drainage deposit", "Amphora foundation deposit for a warehouse", "Amphora reclamation deposits along the banks of the Adige in Verona", "Infilling of a large circular tank with amphorae and other rubbish.", "Amphora reclamation deposit and waste dump", "Amphora reclamation deposit and foundation deposit", "Amphora drainage deposit", "Amphora reclamation deposit beneath a Republican domus")
amphora1 <- amphora[amphora$Archaeological_Context %in% categories,]

### Make Deposition Start and End Dates Numeric ###
amphora1$DAT_min <- as.numeric(amphora1$Lower_Date_Deposit)
amphora1$DAT_max <- as.numeric(amphora1$Upper_Date_Deposit)

### Creation of dating steps (multiplies each object into a number of observations dependent on the object’s dating range)
amphora_steps1<-amphora1 %>% 
  select(AMINI_ID, Provenance_1, DAT_min, DAT_max) %>% 
  datsteps(stepsize=1)

### Addition of weights to assign equal importance to all dated forms ###
amphora_steps1<-scaleweight(amphora_steps1,var="all")


#### Non-Recalamtion Contexts ####

### Select Deposits ###
amphora2 <- amphora[ ! amphora$Archaeological_Context %in% categories, ]
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
amphora2 <- amphora2[amphora2$Location_Specific != "Classis", ]

### Make production start and end dates numeric ###
amphora2$DAT_min <- as.numeric(amphora2$Lower_Date_Deposit)
amphora2$DAT_max <- as.numeric(amphora2$Upper_Date_Deposit)

### Creation of dating steps (multiplies each object into a number of observations dependent on the object’s dating range)
amphora_steps2<-amphora2 %>% 
  select(AMINI_ID, Provenance_1, DAT_min, DAT_max) %>% 
  datsteps(stepsize=1)

### Addition of weights to assign equal importance to all dated forms ###
amphora_steps2<-scaleweight(amphora_steps2,var="all")

#### Plotting Distribution ####
ggplot(NULL,aes(x=DAT_step,))+ geom_density(data=amphora_steps2, bw=5, aes(y = after_stat(count)), alpha=0.5, fill="sandybrown") + geom_density(data=amphora_steps1, bw=5, aes(y = after_stat(count)),alpha=0.9, fill="indianred") + labs(subtitle="", y="Frequency", ) + Plot_Scale + Plot_Theme
                                                                                                                                                                                                                                                                                                                                                

