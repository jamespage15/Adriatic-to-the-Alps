#### Figure 51 ####

#### Packages ####
library(dplyr)
library(ggdendro)
library(ggplot2)
library(patchwork)
library(reshape2)
library(scales)
brks <- c(0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1)

#### Import Data ####
stone <- read.csv("stone.csv")

#### Remove Unknown Provenances ####
stone1 <- stone[stone$Provenance_1 != "Not_Available", ]

stone2 <- stone1 %>% 
  group_by(Location_Specific,Provenance_1) %>%
  summarise(count=n()) %>% 
  mutate(perc=count/sum(count))
stone2$Location_Specific <- factor(stone2$Location_Specific,levels = c("Aquileia", "Altinum", "Ravenna", "Forlì", "Verona", "Brescia", "Milan", "Como", "Vercelli", "Tortona", "Forum Fulvii", "Augusta Bagiennorum", "Aosta"))
p1 <- ggplot(stone2, aes(x = factor(Location_Specific), y = perc, fill = factor(Provenance_1))) + geom_bar(stat="identity", width = 0.7) + scale_y_continuous(breaks = brks, labels = scales::percent(brks)) + labs(subtitle="",x = "", y = NULL) +  theme_bw(base_size = 14) + theme(axis.text.x = element_text(angle = 90)) + scale_fill_manual(name  ="Provenance", labels = c("Asia Minor", "Central Italy and Liguria", "Egypt and North Africa", "Gaul", "Greece and the Aegean", "Northern Italy"), values = c("indianred2", "gold3", "#81A88D", "sandybrown", "steelblue3",  "mediumpurple4")) + theme(legend.position="top")


stoneMatrix <- acast(stone2, Location_Specific~Provenance_1, value.var="perc", fill=0)
View(stoneMatrix)
stoneperc <- as.data.frame(stoneMatrix)
View(stoneperc)
measures <- stoneperc[,1:6]
pcs <- prcomp(measures)
pcs
plot(pcs)
distMeasures <- dist(measures)
stonepercHClust <- hclust(distMeasures, method="average")
p2 <- ggdendrogram(stonepercHClust, rotate=T) + labs(subtitle="", x="", y="") + theme_bw()


#### Plot Combination ####

Clusters <- (p1 / p2)
Clusters


