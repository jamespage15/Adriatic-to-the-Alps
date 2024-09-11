#### Figure 20 ####

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

#### Remove Contents Except for Oil, Wine, and Fish Products ####
amphora <- amphora[amphora$Contents != "Not_Available", ]
amphora <- amphora[amphora$Contents != "Defrutum", ]
amphora <- amphora[amphora$Contents != "Olives", ]
amphora <- amphora[amphora$Contents != "Alum", ]
amphora <- amphora[amphora$Contents != "Fruit", ]
amphora <- amphora[amphora$Contents != "Dates", ]

#### Remove Classis ####
amphora <- amphora[amphora$Location_Specific != "Classis", ]

#### Production Dates ####
amphora1 <- amphora

### Remove forms without production dates ###
amphora1 <- amphora1[amphora1$Lower_Date_Form != "Not_Available", ]
amphora1 <- amphora1[amphora1$Upper_Date_Form != "Not_Available", ]

### Make production start and end dates numeric ###
amphora1$DAT_min <- as.numeric(amphora1$Lower_Date_Form)
amphora1$DAT_max <- as.numeric(amphora1$Upper_Date_Form)

### Creation of dating steps (multiplies each object into a number of observations dependent on the object’s dating range)
amphora_steps1<-amphora1 %>% 
  select(AMINI_ID, Contents, DAT_min, DAT_max) %>% 
  datsteps(stepsize=1)

### Addition of weights to assign equal importance to all dated forms ###
amphora_steps1<-scaleweight(amphora_steps1,var="all")
binwidth<-attributes(amphora_steps1)$stepsize 
scalevalue<-get.histogramscale(amphora_steps1,binwidth=binwidth)                                                                                                                                                                                                       

### Breakdown of Distribution by Contents ###
p1 <- ggplot(amphora_steps1,aes(x=DAT_step, colour=variable)) + geom_density(bw=5, aes(y = after_stat(count)), size=1.0, alpha = 0.25,) + Plot_Theme + Plot_Scale + scale_colour_manual(name  ="Contents", labels = c("Fish Products", "Oil", "Wine"), values = c("cornflowerblue", "goldenrod2", "firebrick")) + labs(y="Frequency", subtitle="A - Production Dates", attributes(amphora)$source,sep="") 

#### Deposition Dates ####
amphora2 <- amphora

### Remove Contents Except for Oil, Wine, and Fish Products ###
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

### Remove forms without production dates ###
amphora2 <- amphora2[amphora2$Lower_Date_Deposit != "Not_Available", ]
amphora2 <- amphora2[amphora2$Upper_Date_Deposit != "Not_Available", ]

### Make production start and end dates numeric ###
amphora2$DAT_min <- as.numeric(amphora2$Lower_Date_Deposit)
amphora2$DAT_max <- as.numeric(amphora2$Upper_Date_Deposit)

### Creation of dating steps (multiplies each object into a number of observations dependent on the object’s dating range)
amphora_steps2<-amphora2 %>% 
  select(AMINI_ID, Contents, DAT_min, DAT_max) %>% 
  datsteps(stepsize=1)

### Addition of weights to assign equal importance to all dated forms ###
amphora_steps2<-scaleweight(amphora_steps2,var="all")
binwidth<-attributes(amphora_steps2)$stepsize 
scalevalue<-get.histogramscale(amphora_steps2,binwidth=binwidth)                                                                                                                                                                                                       

### Breakdown of Distribution by Contents ###
p2 <- ggplot(amphora_steps2,aes(x=DAT_step, colour=variable)) + geom_density(bw=5, aes(y = after_stat(count)), size=1.0, alpha = 0.25,) + Plot_Theme + Plot_Scale + scale_colour_manual(name  ="Contents", labels = c("Fish Products", "Oil", "Wine"), values = c("cornflowerblue", "goldenrod2", "firebrick")) + labs(y="Frequency", subtitle="B - Deposition Dates", attributes(amphora)$source,sep="") 

#### Plot Combination ####

contents <- (p1/p2)
contents
