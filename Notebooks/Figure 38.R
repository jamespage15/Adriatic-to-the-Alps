#### Figure 38 ####

#### Packages ####
library(dplyr)
library(ggdendro)
library(ggplot2)
library(patchwork)
library(reshape2)
library(scales)
brks <- c(0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1)

#### Import Data ####
fineware <- read.csv("fineware.csv")

#### Remove Unknown Provenances ####
fineware1 <- fineware[fineware$Provenance_1 != "Not_Available", ]

#### Imperial ####
imperial <- subset(fineware1, Period=="Imperial")
imperial <- imperial[imperial$Location_Specific != "Altinum", ]
imperial <- imperial[imperial$Location_Specific != "Aquileia", ]
imperial <- imperial[imperial$Location_Specific != "Ariminum", ]
imperial <- imperial[imperial$Location_Specific != "Luna", ]
imperial <- imperial[imperial$Location_Specific != "Ravenna", ]
imperial2 <- imperial %>% 
  group_by(Location_Specific,Provenance_1) %>%
  summarise(count=n()) %>% 
  mutate(perc=count/sum(count))
imperial3 <- data.frame(Location_Specific=c('Turin', 'Turin', 'Turin', 'Turin', 'Novara', 'Novara', 'Novara', 'Vercelli', 'Vercelli', 'Vercelli'), 
                               Provenance_1=c('Central_Italy', 'Gaul', 'North_Africa', 'Northern_Italy', 'Central_Italy', 'Gaul', 'Northern_Italy', 'Central_Italy', 'Gaul', 'Northern_Italy'),
                               count=c(152, 620, 1, 731, 24, 5, 71, 21, 1, 78), 
                              perc=c(0.10166113, 0.41196013, 0.000664452, 0.4857143, 0.24, 0.05, 0.71, 0.21, 0.01, 0.78),
                               stringsAsFactors=FALSE)
imperial4 <- rbind(imperial2, imperial3)
imperial4$Location_Specific <- factor(imperial4$Location_Specific,levels = c("Julia Concordia", "Padua", "Forlì", "Adria", "Bologna", "Modena", "Verona", "Trento", "Bedriacum", "Cremona", "Brescia", "Cividate Camunnorum", "Milan", "Como", "Tortona", "Novara", "Vercelli", "Chieri", "Ivrea", "Turin", "Alba", "Augusta Bagiennorum", "Acqui Terme", "Libarna"))
p1 <- ggplot(imperial4, aes(x = factor(Location_Specific), y = perc, fill = factor(Provenance_1))) + geom_bar(stat="identity", width = 0.7) + scale_y_continuous(breaks = brks, labels = scales::percent(brks)) + labs(subtitle="",x = "", y = NULL, fill = "Provenance") +  theme_bw(base_size = 14)  + scale_fill_manual(labels = c("Adriatic Littoral", "Central Italy", "Eastern Mediterranean", "Gaul", "North Africa", "Northern Italy"), values = c("steelblue2", "gold3", "indianred4", "darkorange3", "#81A88D", "mediumpurple4")) + theme(axis.text.x = element_text(angle = 90))  

imperialMatrix <- acast(imperial4, Location_Specific~Provenance_1, value.var="perc", fill=0)
View(imperialMatrix)
amphorapercimp <- as.data.frame(imperialMatrix)
View(amphorapercimp)
measures <- amphorapercimp[,1:6]
pcs <- prcomp(measures)
pcs
plot(pcs)
distMeasures <- dist(measures)
amphorapercimpHClust <- hclust(distMeasures, method="average")
p2 <- ggdendrogram(amphorapercimpHClust, rotate=T) + labs(title="Imperial Period", x="", y="") + theme_bw() 

#### Late Antiquity ####
late <- subset(fineware1, Period=="Late_Antiquity")
late <- late[late$Location_Specific != "Altinum", ]
late <- late[late$Location_Specific != "Aquileia", ]
late <- late[late$Location_Specific != "Ariminum", ]
late <- late[late$Location_Specific != "Luna", ]
late <- late[late$Location_Specific != "Ravenna", ]
late <- late[late$Location_Specific != "Acqui Terme", ]
late <- late[late$Location_Specific != "Augusta Bagiennorum", ]
late <- late[late$Location_Specific != "Bedriacum", ]
late <- late[late$Location_Specific != "Cividate Camunnorum", ]
late <- late[late$Location_Specific != "Como", ]
late <- late[late$Location_Specific != "Cremona", ]
late2 <- late %>% 
  group_by(Location_Specific,Provenance_1) %>%
  summarise(count=n()) %>% 
  mutate(perc=count/sum(count))
late2$Location_Specific <- factor(late2$Location_Specific,levels = c("Padua", "Forlì", "Verona", "Trento", "Brescia", "Milan", "Chieri", "Alba"))
p3 <- ggplot(late2, aes(x = factor(Location_Specific), y = perc, fill = factor(Provenance_1)),) + geom_bar(stat="identity", width = 0.7,) + scale_y_continuous(breaks = brks, labels = scales::percent(brks)) + labs(subtitle="",x = "", y = NULL, fill = "Provenance") +  theme_bw(base_size = 14)  + scale_fill_manual(labels = c("Adriatic Littoral", "Eastern Mediterranean", "North Africa", "Northern Italy"), values = c("steelblue2", "indianred4", "#81A88D", "mediumpurple4")) + theme(axis.text.x = element_text(angle = 90)) 


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
p4 <- ggdendrogram(amphoraperclateHClust, rotate=T) + labs(title="Late Antiquity", x="", y="") + theme_bw()

#### Plot Combination ####
Clusters <- (p2 + p1)/(p4+p3)
Clusters