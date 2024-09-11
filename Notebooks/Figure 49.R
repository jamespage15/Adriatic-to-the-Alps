#### Figure 49 ####

#### Packages ####
library(dplyr)
library(ggplot2)
library(scales)
brks <- c(0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1)

#### Import Data ####
stone <- read.csv("stone.csv")

#### Colours by Site ####
stone1 <- stone[stone$Provenance_1 != "Not_Available", ]
stone2 <- stone1 %>% 
  group_by(Location_Specific,Colour) %>%
  summarise(count=n()) %>% 
  mutate(perc=count/sum(count))
View(stone2)
stone2$Location_Specific <- factor(stone2$Location_Specific,levels = c("Ravenna", "Aquileia", "Altinum", "Forlì", "Verona", "Brescia", "Milan", "Vercelli", "Como", "Tortona", "Forum Fulvii", "Augusta Bagiennorum", "Aosta"))
ggplot(stone2, aes(x = factor(Location_Specific), y = perc, fill = factor(Colour))) + geom_bar(stat="identity", width = 0.7) + scale_y_continuous(breaks = brks, labels = scales::percent(brks)) + labs(title="",x = "", y = NULL, fill = "Colour") +  theme_bw(base_size = 14) + theme(axis.text.x = element_text(angle = 90)) + scale_fill_manual(values = c("snow4", "bisque3", "snow2"))

