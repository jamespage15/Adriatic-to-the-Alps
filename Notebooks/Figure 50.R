#### Figure 50 ####

#### Packages ####
library(dplyr)
library(ggplot2)
library(scales)
brks <- c(0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1)

#### Import Data ####
stone <- read.csv("stone.csv")

#### Breakdown of Provenances by Colour ####
stone1 <- stone[stone$Provenance_1 != "Not_Available", ]
stone2 <- stone1 %>% 
  group_by(Colour,Provenance_1) %>%
  summarise(count=n()) %>% 
  mutate(perc=count/sum(count))
View(stone2)
ggplot(stone2, aes(x = factor(Colour), y = perc, fill = factor(Provenance_1))) + geom_bar(stat="identity", width = 0.7) + scale_y_continuous(breaks = brks, labels = scales::percent(brks)) +  labs(title="",x = "", y = NULL, fill = "Provenance") +  theme_bw(base_size = 14) + scale_fill_manual(name  ="Provenance", labels = c("Asia Minor", "Central Italy and Liguria", "Egypt and North Africa", "Gaul", "Greece and the Aegean", "Northern Italy"), values = c("indianred2", "gold3", "#81A88D", "sandybrown", "steelblue3",  "mediumpurple4"))
