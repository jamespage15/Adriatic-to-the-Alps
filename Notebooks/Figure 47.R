#### Figure 47 ####

#### Packages ####
library(dplyr)
library(ggplot2)
library(scales)
brks <- c(0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1)

#### Import Data ####
stone <- read.csv("stone.csv")

### Boxplot for all amphorae ###
total <- stone %>% 
  group_by(Location_Specific,Provenance_1) %>%
  summarise(count=n()) %>% 
  mutate(perc=count/sum(count))
View(total)
ggplot(total, aes(x=Provenance_1, y=perc, fill=Provenance_1)) + stat_boxplot(geom ='errorbar') + geom_boxplot(outlier.shape = NA) + geom_jitter(shape=16, position=position_jitter(0.2), alpha = 0.3) + labs(title="",x="", y = "", fill = "Provenance") + theme_bw() + theme(legend.position="none") + scale_y_continuous(breaks = brks, labels = scales::percent(brks)) + scale_fill_manual(values = c("indianred2", "gold3", "#81A88D", "sandybrown", "steelblue3",  "mediumpurple4", "grey")) + scale_x_discrete(labels=c("Asia_Minor"="Asia Minor", "Central_Italy_and_Liguria"="Central Italy and Liguria", "Egypt_and_North_Africa"="Egypt and North Africa", "Gaul"="Gaul", "Greece_And_Aegean"="Greece and Aegean", "Northern_Italy"="Northern Italy", "Not_Available"="Unknown"))
