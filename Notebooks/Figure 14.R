#### Figure 14 ####

#### Packages ####
library(dplyr)
library(ggplot2)
library(scales)
brks <- c(0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1)

#### Import Data ####
amphora <- read.csv("amphora.csv")

### Boxplot for all amphorae ###
amphora1 <- subset(amphora)
amphora1 <- amphora1[amphora1$Provenance_1 != "Not_Available", ]
total <- amphora1 %>% 
  group_by(Location_Specific,Provenance_1) %>%
  summarise(count=n()) %>% 
  mutate(perc=count/sum(count))
View(total)
ggplot(total, aes(x=Provenance_1, y=perc, fill=Provenance_1)) + stat_boxplot(geom ='errorbar') + geom_boxplot(outlier.shape = NA) + geom_jitter(shape=16, position=position_jitter(0.2), alpha = 0.3) + labs(x="", y = "", fill = "Provenance") + theme_bw() + theme(legend.position="none") + scale_y_continuous(breaks = brks, labels = scales::percent(brks)) + scale_fill_manual(values = c("steelblue2", "indianred2", "sandybrown", "khaki3", "#81A88D", "mediumorchid3")) + scale_x_discrete(labels=c("Adriatic_Littoral"="Adriatic Littoral", "Eastern_Mediterranean"="Eastern Mediterranean", "Gaul"="Gaul", "Iberian_Peninsula"="Iberian Peninsula", "North_Africa"="North Africa", "Not_Available"="Unknown", "Tyrrhenian_Littoral"="Tyrrhenian Littoral"))

