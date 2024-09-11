#### Figure 22 ####

#### Packages ####

library(dplyr)
library(ggdendro)
library(ggplot2)
library(patchwork)
library(reshape2)
library(scales)
brks <- c(0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1)

#### Import Data ####
amphora <- read.csv("amphora.csv")

#### Remove Unknown Provenances ####
amphora1 <- amphora[amphora$Provenance_1 != "Not_Available", ]

#### Republic ####
republic <- subset(amphora1, Period=="Late_Republic")
republic <- republic[republic$Location_Specific != "Altinum", ]
republic <- republic[republic$Location_Specific != "Aquileia", ]
republic <- republic[republic$Location_Specific != "Ariminum", ]
republic <- republic[republic$Location_Specific != "Luna", ]
republic <- republic[republic$Location_Specific != "Portus Maurici", ]
republic <- republic[republic$Location_Specific != "Ravenna", ]
republic <- republic[republic$Location_Specific != "Alba", ]
republic <- republic[republic$Location_Specific != "Brescia", ]
republic <- republic[republic$Location_Specific != "Acqui Terme", ]
republic <- republic[republic$Location_Specific != "Este", ]
republic <- republic[republic$Location_Specific != "Augusta Bagiennorum", ]
republic <- republic[republic$Location_Specific != "Clastidium", ]
republic <- republic[republic$Location_Specific != "Industria", ]
republic <- republic[republic$Location_Specific != "Modena", ]
republic <- republic[republic$Location_Specific != "Novara", ]
republic <- republic[republic$Location_Specific != "Como", ]
republic <- republic[republic$Location_Specific != "Oderzo", ]
republic <- republic[republic$Location_Specific != "Reggio Emilia", ]
republic <- republic[republic$Location_Specific != "Vercelli", ]
republic <- republic[republic$Location_Specific != "Verona", ]
republic2 <- republic %>% 
  group_by(Location_Specific,Provenance_1) %>%
  summarise(count=n()) %>% 
  mutate(perc=count/sum(count))
republic2$Location_Specific <- factor(republic2$Location_Specific,levels = c("Padua", "Vicenza", "Forlì", "Verona", "Trento", "Bedriacum", "Cremona", "Milan", "Laus Pompeia", "Como", "Ivrea"))
p1 <- ggplot(republic2, aes(x = factor(Location_Specific), y = perc, fill = factor(Provenance_1))) + geom_bar(stat="identity", width = 0.7) + scale_y_continuous(breaks = brks, labels = scales::percent(brks)) + labs(subtitle ="", x = "", y = NULL, fill = "Provenance") +  theme_bw(base_size = 14) + scale_fill_manual(labels = c("Adriatic Littoral", "North Africa", "Tyrrhenian Littoral"), values = c("steelblue2", "#81A88D", "mediumorchid3")) + theme(axis.text.x = element_text(angle = 90)) 

republicMatrix <- acast(republic2, Location_Specific~Provenance_1, value.var="perc", fill=0)
View(republicMatrix)
amphorapercrep <- as.data.frame(republicMatrix)
View(amphorapercrep)
measures <- amphorapercrep[,1:2]
pcs <- prcomp(measures)
pcs
plot(pcs)
distMeasures <- dist(measures)
amphorapercrepHClust <- hclust(distMeasures, method="average")
p2 <- ggdendrogram(amphorapercrepHClust, rotate=T) + labs(title="Late Republic", subtitle = "", x="", y="") + theme_bw()

#### Imperial ####
imperial <- subset(amphora1, Period=="Imperial")
imperial <- imperial[imperial$Location_Specific != "Altinum", ]
imperial <- imperial[imperial$Location_Specific != "Aquileia", ]
imperial <- imperial[imperial$Location_Specific != "Ariminum", ]
imperial <- imperial[imperial$Location_Specific != "Luna", ]
imperial <- imperial[imperial$Location_Specific != "Portus Maurici", ]
imperial <- imperial[imperial$Location_Specific != "Ravenna", ]
imperial <- imperial[imperial$Location_Specific != "Reggio Emilia", ]
imperial2 <- imperial %>% 
  group_by(Location_Specific,Provenance_1) %>%
  summarise(count=n()) %>% 
  mutate(perc=count/sum(count))
imperial2$Location_Specific <- factor(imperial2$Location_Specific,levels = c("Oderzo", "Padua", "Vicenza", "Este", "Forlì", "Reggio Emilia", "Modena", "Verona", "Trento", "Bedriacum", "Cremona", "Brescia", "Civitas Camunnorum", "Laus Pompeia", "Milan", "Como", "Clastidium", "Novara", "Vercelli", "Industria", "Ivrea", "Alba", "Augusta Bagiennorum", "Acqui Terme", "Libarna"))
p3 <- ggplot(imperial2, aes(x = factor(Location_Specific), y = perc, fill = factor(Provenance_1))) + geom_bar(stat="identity", width = 0.7) + scale_y_continuous(breaks = brks, labels = scales::percent(brks)) + labs(subtitle="",x = "", y = NULL, fill = "Provenance") +  theme_bw(base_size = 14)  + scale_fill_manual(labels = c("Adriatic Littoral", "Eastern Mediterranean", "Gaul", "Iberian Peninsula", "North Africa", "Tyrrhenian Littoral"), values = c("steelblue2", "indianred2", "sandybrown", "khaki3", "#81A88D", "mediumorchid3")) + theme(axis.text.x = element_text(angle = 90)) 


imperialMatrix <- acast(imperial2, Location_Specific~Provenance_1, value.var="perc", fill=0)
View(imperialMatrix)
amphorapercimp <- as.data.frame(imperialMatrix)
View(amphorapercimp)
measures <- amphorapercimp[,1:6]
pcs <- prcomp(measures)
pcs
plot(pcs)
distMeasures <- dist(measures)
amphorapercimpHClust <- hclust(distMeasures, method="average")
p4 <- ggdendrogram(amphorapercimpHClust, rotate=T) + labs(title="Imperial Period", subtitle="", x="", y="") + theme_bw()

#### Late Antiquity ####
late <- subset(amphora1, Period=="Late_Antiquity")
late <- late[late$Location_Specific != "Altinum", ]
late <- late[late$Location_Specific != "Aquileia", ]
late <- late[late$Location_Specific != "Ariminum", ]
late <- late[late$Location_Specific != "Luna", ]
late <- late[late$Location_Specific != "Portus Maurici", ]
late <- late[late$Location_Specific != "Ravenna", ]
late <- late[late$Location_Specific != "Acqui Terme", ]
late <- late[late$Location_Specific != "Augusta Bagiennorum", ]
late <- late[late$Location_Specific != "Bedriacum", ]
late <- late[late$Location_Specific != "Civitas Camunnorum", ]
late <- late[late$Location_Specific != "Cremona", ]
late <- late[late$Location_Specific != "Classis", ]
late <- late[late$Location_Specific != "Industria", ]
late <- late[late$Location_Specific != "Como", ]
late <- late[late$Location_Specific != "Vercelli", ]
late2 <- late %>% 
  group_by(Location_Specific,Provenance_1) %>%
  summarise(count=n()) %>% 
  mutate(perc=count/sum(count))
late2$Location_Specific <- factor(late2$Location_Specific,levels = c("Forlì", "Verona", "Trento", "Brescia", "Milan", "Clastidium"))
p5 <- ggplot(late2, aes(x = factor(Location_Specific), y = perc, fill = factor(Provenance_1))) + geom_bar(stat="identity", width = 0.7) + scale_y_continuous(breaks = brks, labels = scales::percent(brks)) + labs(subtitle="",x = "", y = NULL, fill = "Provenance") +  theme_bw(base_size = 14)  + scale_fill_manual(labels = c("Eastern Mediterranean", "Iberian Peninsula", "North Africa", "Tyrrhenian Littoral"), values = c("indianred2", "khaki3", "#81A88D", "mediumorchid3")) + theme(axis.text.x = element_text(angle = 90))


lateMatrix <- acast(late2, Location_Specific~Provenance_1, value.var="perc", fill=0)
View(lateMatrix)
amphoraperclate <- as.data.frame(lateMatrix)
View(amphoraperclate)
measures <- amphoraperclate[,1:4]
pcs <- prcomp(measures)
pcs
plot(pcs)
distMeasures <- dist(measures)
amphoraperclateHClust <- hclust(distMeasures, method="average")
p6 <- ggdendrogram(amphoraperclateHClust, rotate=T) + labs(title="Late Antiquity", subtitle="", x="", y="") + theme_bw()

#### Plot Combination ####

Clusters <- (p2 + p1)/(p4 + p3)/(p6 + p5)
Clusters

