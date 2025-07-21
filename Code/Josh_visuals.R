library(tidyverse)
library(readxl)

josh1 <- read_excel("Data/josh.xlsx", sheet = "2^-ddCT")
glimpse(josh1)

josh2 <- read_excel("Data/josh.xlsx", sheet = "Week 1 & 3")
glimpse(josh2)


josh1 <- josh1 %>% 
  pivot_longer(cols = c("Week 1", "Week 3"),
               names_to = "Period",
               values_to = "2^-ddCT") %>% 
  rename("DDCT" = "2^-ddCT") %>% 
  as.data.frame()



Solyc01g074040 <- josh1 %>% 
  filter(Type =="Solyc01g074040") %>% 
  ggplot() +
  geom_boxplot(aes(x = Treatment, y = DDCT, fill = Period),
               outlier.colour = NULL,
               outlier.color = "transparent",
               outlier.fill = NULL) +
  geom_point(aes(x = Treatment, y = DDCT, colour = Period),
             position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.8),
             size = 1.8, alpha = 0.75) +
  scale_fill_manual(values = c("#FFE699", "#D6EBF2", 
                               "#B4F7B4")) +  # Set fill colors
  scale_color_manual(values = c("orange", 
                                "blue",
                                "darkgreen")) + 
  labs(y = "2^-ddCT", x= "Genes", title = "Solyc01g074040")+
  theme_bw() 

ggsave(filename = "Solyc01g074040.jpg", 
       path = "C:\\Users\\DELL\\Documents\\Git in R\\Projects\\Figures", 
       height = 2.5, width = 5)


Solyc01g073740 <- josh1 %>% 
  filter(Type =="Solyc01g073740") %>% 
  ggplot() +
  geom_boxplot(aes(x = Treatment, y = DDCT, fill = Period),
               outlier.colour = NULL,
               outlier.color = "transparent",
               outlier.fill = NULL) +
  geom_point(aes(x = Treatment, y = DDCT, colour = Period),
             position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.8),
             size = 1.8, alpha = 0.75) +
  scale_fill_manual(values = c("#FFE699", "#D6EBF2", 
                               "#B4F7B4")) +  # Set fill colors
  scale_color_manual(values = c("orange", 
                                "blue",
                                "darkgreen")) + 
  labs(y = "2^-ddCT", x= "Genes", title = "Solyc01g073740 ")+
  theme_bw() 

ggsave(filename = "Solyc01g073740.jpg", 
       path = "C:\\Users\\DELL\\Documents\\Git in R\\Projects\\Figures", 
       height = 2.5, width = 5)



Solyc01g089853 <- josh1 %>% 
  filter(Type =="Solyc01g089853") %>% 
  ggplot() +
  geom_boxplot(aes(x = Treatment, y = DDCT, fill = Period),
               outlier.colour = NULL,
               outlier.color = "transparent",
               outlier.fill = NULL) +
  geom_point(aes(x = Treatment, y = DDCT, colour = Period),
             position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.8),
             size = 1.8, alpha = 0.75) +
  scale_fill_manual(values = c("#FFE699", "#D6EBF2"))+
  scale_color_manual(values = c("orange", 
                                "blue")) + 
  labs(y = "2^-ddCT", x= "Genes", title = "Solyc01g089853 ")+
  theme_bw() 
ggsave(filename = "Solyc01g089853.jpg", 
       path = "C:\\Users\\DELL\\Documents\\Git in R\\Projects\\Figures", 
       height = 2.5, width = 5)


Solyc01g086640 <- josh1 %>% 
  filter(Type =="Solyc01g086640") %>% 
  ggplot() +
  geom_boxplot(aes(x = Treatment, y = DDCT, fill = Period),
               outlier.colour = NULL,
               outlier.color = "transparent",
               outlier.fill = NULL) +
  geom_point(aes(x = Treatment, y = DDCT, colour = Period),
             position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.8),
             size = 1.8, alpha = 0.75) +
  scale_fill_manual(values = c("#FFE699", "#D6EBF2"))+
  scale_color_manual(values = c("orange", 
                                "blue")) + 
  labs(y = "2^-ddCT", x= "Genes", title = "Solyc01g086640 ")+
  theme_bw() 
ggsave(filename = "Solyc01g086640.jpg", 
       path = "C:\\Users\\DELL\\Documents\\Git in R\\Projects\\Figures", 
       height = 2.5, width = 5)


Solyc01g073920 <- josh1 %>% 
  filter(Type =="Solyc01g073920") %>% 
  ggplot() +
  geom_boxplot(aes(x = Treatment, y = DDCT, fill = Period),
               outlier.colour = NULL,
               outlier.color = "transparent",
               outlier.fill = NULL) +
  geom_point(aes(x = Treatment, y = DDCT, colour = Period),
             position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.8),
             size = 1.8, alpha = 0.75) +
  scale_fill_manual(values = c("#FFE699", "#D6EBF2"))+
  scale_color_manual(values = c("orange", 
                                "blue")) + 
  labs(y = "2^-ddCT", x= "Genes", title = "Solyc01g073920 ")+
  theme_bw() 
ggsave(filename = "Solyc01g073920.jpg", 
       path = "C:\\Users\\DELL\\Documents\\Git in R\\Projects\\Figures", 
       height = 2.5, width = 5)


Solyc01g073950 <- josh1 %>% 
  filter(Type =="Solyc01g073950") %>% 
  ggplot() +
  geom_boxplot(aes(x = Treatment, y = DDCT, fill = Period),
               outlier.colour = NULL,
               outlier.color = "transparent",
               outlier.fill = NULL) +
  geom_point(aes(x = Treatment, y = DDCT, colour = Period),
             position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.8),
             size = 1.8, alpha = 0.75) +
  scale_fill_manual(values = c("#FFE699", "#D6EBF2"))+
  scale_color_manual(values = c("orange", 
                                "blue")) + 
  labs(y = "2^-ddCT", x= "Genes", title = "Solyc01g073950 ")+
  theme_bw() 
ggsave(filename = "Solyc01g073950.jpg", 
       path = "C:\\Users\\DELL\\Documents\\Git in R\\Projects\\Figures", 
       height = 2.5, width = 5)


Solyc01g073960 <- josh1 %>% 
  filter(Type =="Solyc01g073960") %>% 
  ggplot() +
  geom_boxplot(aes(x = Treatment, y = DDCT, fill = Period),
               outlier.colour = NULL,
               outlier.color = "transparent",
               outlier.fill = NULL) +
  geom_point(aes(x = Treatment, y = DDCT, colour = Period),
             position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.8),
             size = 1.8, alpha = 0.75) +
  scale_fill_manual(values = c("#FFE699", "#D6EBF2"))+
  scale_color_manual(values = c("orange", 
                                "blue")) + 
  labs(y = "2^-ddCT", x= "Genes", title = "Solyc01g073960 ")+
  theme_bw() 
ggsave(filename = "Solyc01g073960.jpg", 
       path = "C:\\Users\\DELL\\Documents\\Git in R\\Projects\\Figures", 
       height = 2.5, width = 5)


Solyc01g074030 <- josh1 %>% 
  filter(Type =="Solyc01g074030") %>% 
  ggplot() +
  geom_boxplot(aes(x = Treatment, y = DDCT, fill = Period),
               outlier.colour = NULL,
               outlier.color = "transparent",
               outlier.fill = NULL) +
  geom_point(aes(x = Treatment, y = DDCT, colour = Period),
             position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.8),
             size = 1.8, alpha = 0.75) +
  scale_fill_manual(values = c("#FFE699", "#D6EBF2"))+
  scale_color_manual(values = c("orange", 
                                "blue")) + 
  labs(y = "2^-ddCT", x= "Genes", title = "Solyc01g074030 ")+
  theme_bw() 
ggsave(filename = "Solyc01g074030.jpg", 
       path = "C:\\Users\\DELL\\Documents\\Git in R\\Projects\\Figures", 
       height = 2.5, width = 5)


Solyc01g089850 <- josh1 %>% 
  filter(Type =="Solyc01g089850") %>% 
  ggplot() +
  geom_boxplot(aes(x = Treatment, y = DDCT, fill = Period),
               outlier.colour = NULL,
               outlier.color = "transparent",
               outlier.fill = NULL) +
  geom_point(aes(x = Treatment, y = DDCT, colour = Period),
             position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.8),
             size = 1.8, alpha = 0.75) +
  scale_fill_manual(values = c("#FFE699", "#D6EBF2"))+
  scale_color_manual(values = c("orange", 
                                "blue")) + 
  labs(y = "2^-ddCT", x= "Genes", title = "Solyc01g089850 ")+
  theme_bw() 
ggsave(filename = "Solyc01g089850.jpg", 
       path = "C:\\Users\\DELL\\Documents\\Git in R\\Projects\\Figures", 
       height = 2.5, width = 5)


####################################################################################################


W1Solyc01g074040 <- josh2 %>% 
  filter(Type =="Solyc01g074040", 
         Period == "Week 1") %>% 
  ggplot()+
  geom_boxplot(aes(x = Treatment, y = dCT, fill = Group),
               outlier.colour = NULL,
               outlier.color = "transparent",
               outlier.fill = NULL) +
  geom_point(aes(x = Treatment, y = dCT, colour = Group),
             position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.8),
             size = 1.8, alpha = 0.75) +
  scale_fill_manual(values = c("#FFE699", "#D6EBF2"))+
  scale_color_manual(values = c("orange", 
                                "blue")) + 
  labs(y = "dCT", x= "Genes", title = "Solyc01g074040")+
  theme_bw() 
ggsave(filename = "W1Solyc01g074040.jpg", 
       path = "C:\\Users\\DELL\\Documents\\Git in R\\Projects\\Figures", 
       height = 2.5, width = 5)




W1Solyc01g073740 <- josh2 %>% 
  filter(Type =="Solyc01g073740", 
         Period == "Week 1") %>% 
  ggplot()+
  geom_boxplot(aes(x = Treatment, y = dCT, fill = Group),
               outlier.colour = NULL,
               outlier.color = "transparent",
               outlier.fill = NULL) +
  geom_point(aes(x = Treatment, y = dCT, colour = Group),
             position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.8),
             size = 1.8, alpha = 0.75) +
  scale_fill_manual(values = c("#FFE699", "#D6EBF2"))+
  scale_color_manual(values = c("orange", 
                                "blue")) + 
  labs(y = "dCT", x= "Genes", title = "Solyc01g073740")+
  theme_bw() 
ggsave(filename = "W1Solyc01g073740.jpg", 
       path = "C:\\Users\\DELL\\Documents\\Git in R\\Projects\\Figures", 
       height = 2.5, width = 5)



W1Solyc01g089853 <- josh2 %>% 
  filter(Type =="Solyc01g089853", 
         Period == "Week 1") %>% 
  ggplot()+
  geom_boxplot(aes(x = Treatment, y = dCT, fill = Group),
               outlier.colour = NULL,
               outlier.color = "transparent",
               outlier.fill = NULL) +
  geom_point(aes(x = Treatment, y = dCT, colour = Group),
             position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.8),
             size = 1.8, alpha = 0.75) +
  scale_fill_manual(values = c("#FFE699", "#D6EBF2"))+
  scale_color_manual(values = c("orange", 
                                "blue")) + 
  labs(y = "dCT", x= "Genes", title = "Solyc01g089853")+
  theme_bw() 
ggsave(filename = "W1Solyc01g089853.jpg", 
       path = "C:\\Users\\DELL\\Documents\\Git in R\\Projects\\Figures", 
       height = 2.5, width = 5)




W1Solyc01g086640 <- josh2 %>% 
  filter(Type =="Solyc01g086640", 
         Period == "Week 1") %>% 
  ggplot()+
  geom_boxplot(aes(x = Treatment, y = dCT, fill = Group),
               outlier.colour = NULL,
               outlier.color = "transparent",
               outlier.fill = NULL) +
  geom_point(aes(x = Treatment, y = dCT, colour = Group),
             position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.8),
             size = 1.8, alpha = 0.75) +
  scale_fill_manual(values = c("#FFE699", "#D6EBF2"))+
  scale_color_manual(values = c("orange", 
                                "blue")) + 
  labs(y = "dCT", x= "Genes", title = "Solyc01g086640")+
  theme_bw() 
ggsave(filename = "W1Solyc01g086640.jpg", 
       path = "C:\\Users\\DELL\\Documents\\Git in R\\Projects\\Figures", 
       height = 2.5, width = 5)



W1Solyc01g073920 <- josh2 %>% 
  filter(Type =="Solyc01g073920", 
         Period == "Week 1") %>% 
  ggplot()+
  geom_boxplot(aes(x = Treatment, y = dCT, fill = Group),
               outlier.colour = NULL,
               outlier.color = "transparent",
               outlier.fill = NULL) +
  geom_point(aes(x = Treatment, y = dCT, colour = Group),
             position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.8),
             size = 1.8, alpha = 0.75) +
  scale_fill_manual(values = c("#FFE699", "#D6EBF2"))+
  scale_color_manual(values = c("orange", 
                                "blue")) + 
  labs(y = "dCT", x= "Genes", title = "Solyc01g073920")+
  theme_bw() 
ggsave(filename = "W1Solyc01g073920.jpg", 
       path = "C:\\Users\\DELL\\Documents\\Git in R\\Projects\\Figures", 
       height = 2.5, width = 5)




W1Solyc01g073950 <- josh2 %>% 
  filter(Type =="Solyc01g073950", 
         Period == "Week 1") %>% 
  ggplot()+
  geom_boxplot(aes(x = Treatment, y = dCT, fill = Group),
               outlier.colour = NULL,
               outlier.color = "transparent",
               outlier.fill = NULL) +
  geom_point(aes(x = Treatment, y = dCT, colour = Group),
             position = position_jitterdodge(jitter.width = 0.2,
                                             dodge.width = 0.8),
             size = 1.8, alpha = 0.75) +
  scale_fill_manual(values = c("#FFE699", "#D6EBF2"))+
  scale_color_manual(values = c("orange", 
                                "blue")) + 
  labs(y = "dCT", x= "Genes", title = "Solyc01g073950")+
  theme_bw() 
ggsave(filename = "W1Solyc01g073950.jpg", 
       path = "C:\\Users\\DELL\\Documents\\Git in R\\Projects\\Figures", 
       height = 2.5, width = 5)



W1Solyc01g073960 <- josh2 %>% 
  filter(Type =="Solyc01g073960", 
         Period == "Week 1") %>% 
  ggplot()+
  geom_boxplot(aes(x = Treatment, y = dCT, fill = Group),
               outlier.colour = NULL,
               outlier.color = "transparent",
               outlier.fill = NULL) +
  geom_point(aes(x = Treatment, y = dCT, colour = Group),
             position = position_jitterdodge(jitter.width = 0.2,
                                             dodge.width = 0.8),
             size = 1.8, alpha = 0.75) +
  scale_fill_manual(values = c("#FFE699", "#D6EBF2"))+
  scale_color_manual(values = c("orange", 
                                "blue")) + 
  labs(y = "dCT", x= "Genes", title = "Solyc01g073960")+
  theme_bw() 
ggsave(filename = "W1Solyc01g073960.jpg", 
       path = "C:\\Users\\DELL\\Documents\\Git in R\\Projects\\Figures", 
       height = 2.5, width = 5)


W1Solyc01g074030 <- josh2 %>% 
  filter(Type =="Solyc01g074030", 
         Period == "Week 1") %>% 
  ggplot()+
  geom_boxplot(aes(x = Treatment, y = dCT, fill = Group),
               outlier.colour = NULL,
               outlier.color = "transparent",
               outlier.fill = NULL) +
  geom_point(aes(x = Treatment, y = dCT, colour = Group),
             position = position_jitterdodge(jitter.width = 0.2,
                                             dodge.width = 0.8),
             size = 1.8, alpha = 0.75) +
  scale_fill_manual(values = c("#FFE699", "#D6EBF2"))+
  scale_color_manual(values = c("orange", 
                                "blue")) + 
  labs(y = "dCT", x= "Genes", title = "Solyc01g074030")+
  theme_bw() 
ggsave(filename = "W1Solyc01g074030.jpg", 
       path = "C:\\Users\\DELL\\Documents\\Git in R\\Projects\\Figures", 
       height = 2.5, width = 5)



W1Solyc01g089850 <- josh2 %>% 
  filter(Type =="Solyc01g089850", 
         Period == "Week 1") %>% 
  ggplot()+
  geom_boxplot(aes(x = Treatment, y = dCT, fill = Group),
               outlier.colour = NULL,
               outlier.color = "transparent",
               outlier.fill = NULL) +
  geom_point(aes(x = Treatment, y = dCT, colour = Group),
             position = position_jitterdodge(jitter.width = 0.2,
                                             dodge.width = 0.8),
             size = 1.8, alpha = 0.75) +
  scale_fill_manual(values = c("#FFE699", "#D6EBF2"))+
  scale_color_manual(values = c("orange", 
                                "blue")) + 
  labs(y = "dCT", x= "Genes", title = "Solyc01g089850")+
  theme_bw() 
ggsave(filename = "W1Solyc01g089850.jpg", 
       path = "C:\\Users\\DELL\\Documents\\Git in R\\Projects\\Figures", 
       height = 2.5, width = 5)


########################################################################################



W2Solyc01g074040 <- josh2 %>% 
  filter(Type =="Solyc01g074040", 
         Period == "Week 2") %>% 
  ggplot()+
  geom_boxplot(aes(x = Treatment, y = dCT, fill = Group),
               outlier.colour = NULL,
               outlier.color = "transparent",
               outlier.fill = NULL) +
  geom_point(aes(x = Treatment, y = dCT, colour = Group),
             position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.8),
             size = 1.8, alpha = 0.75) +
  scale_fill_manual(values = c("#FFE699", "#FF9999"))+
  scale_color_manual(values = c("orange", 
                                "#990000")) + 
  labs(y = "dCT", x= "Genes", title = "Solyc01g074040")+
  theme_bw() 
ggsave(filename = "W2Solyc01g074040.jpg", 
       path = "C:\\Users\\DELL\\Documents\\Git in R\\Projects\\Figures", 
       height = 2.5, width = 5)




W2Solyc01g073740 <- josh2 %>% 
  filter(Type =="Solyc01g073740", 
         Period == "Week 2") %>% 
  ggplot()+
  geom_boxplot(aes(x = Treatment, y = dCT, fill = Group),
               outlier.colour = NULL,
               outlier.color = "transparent",
               outlier.fill = NULL) +
  geom_point(aes(x = Treatment, y = dCT, colour = Group),
             position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.8),
             size = 1.8, alpha = 0.75) +
  scale_fill_manual(values = c("#FFE699", "#D6EBF2"))+
  scale_color_manual(values = c("orange", 
                                "blue")) + 
  labs(y = "dCT", x= "Genes", title = "Solyc01g073740")+
  theme_bw() 
ggsave(filename = "W2Solyc01g073740.jpg", 
       path = "C:\\Users\\DELL\\Documents\\Git in R\\Projects\\Figures", 
       height = 2.5, width = 5)



W2Solyc01g089853 <- josh2 %>% 
  filter(Type =="Solyc01g089853", 
         Period == "Week 2") %>% 
  ggplot()+
  geom_boxplot(aes(x = Treatment, y = dCT, fill = Group),
               outlier.colour = NULL,
               outlier.color = "transparent",
               outlier.fill = NULL) +
  geom_point(aes(x = Treatment, y = dCT, colour = Group),
             position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.8),
             size = 1.8, alpha = 0.75) +
  scale_fill_manual(values = c("#FFE699", "#D6EBF2"))+
  scale_color_manual(values = c("orange", 
                                "blue")) + 
  labs(y = "dCT", x= "Genes", title = "Solyc01g089853")+
  theme_bw() 
ggsave(filename = "W2Solyc01g089853.jpg", 
       path = "C:\\Users\\DELL\\Documents\\Git in R\\Projects\\Figures", 
       height = 2.5, width = 5)




W2Solyc01g086640 <- josh2 %>% 
  filter(Type =="Solyc01g086640", 
         Period == "Week 2") %>% 
  ggplot()+
  geom_boxplot(aes(x = Treatment, y = dCT, fill = Group),
               outlier.colour = NULL,
               outlier.color = "transparent",
               outlier.fill = NULL) +
  geom_point(aes(x = Treatment, y = dCT, colour = Group),
             position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.8),
             size = 1.8, alpha = 0.75) +
  scale_fill_manual(values = c("#FFE699", "#D6EBF2"))+
  scale_color_manual(values = c("orange", 
                                "blue")) + 
  labs(y = "dCT", x= "Genes", title = "Solyc01g086640")+
  theme_bw() 
ggsave(filename = "W2Solyc01g086640.jpg", 
       path = "C:\\Users\\DELL\\Documents\\Git in R\\Projects\\Figures", 
       height = 2.5, width = 5)



W2Solyc01g073920 <- josh2 %>% 
  filter(Type =="Solyc01g073920", 
         Period == "Week 2") %>% 
  ggplot()+
  geom_boxplot(aes(x = Treatment, y = dCT, fill = Group),
               outlier.colour = NULL,
               outlier.color = "transparent",
               outlier.fill = NULL) +
  geom_point(aes(x = Treatment, y = dCT, colour = Group),
             position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.8),
             size = 1.8, alpha = 0.75) +
  scale_fill_manual(values = c("#FFE699", "#D6EBF2"))+
  scale_color_manual(values = c("orange", 
                                "blue")) + 
  labs(y = "dCT", x= "Genes", title = "Solyc01g073920")+
  theme_bw() 
ggsave(filename = "W2Solyc01g073920.jpg", 
       path = "C:\\Users\\DELL\\Documents\\Git in R\\Projects\\Figures", 
       height = 2.5, width = 5)




W2Solyc01g073950 <- josh2 %>% 
  filter(Type =="Solyc01g073950", 
         Period == "Week 2") %>% 
  ggplot()+
  geom_boxplot(aes(x = Treatment, y = dCT, fill = Group),
               outlier.colour = NULL,
               outlier.color = "transparent",
               outlier.fill = NULL) +
  geom_point(aes(x = Treatment, y = dCT, colour = Group),
             position = position_jitterdodge(jitter.width = 0.2,
                                             dodge.width = 0.8),
             size = 1.8, alpha = 0.75) +
  scale_fill_manual(values = c("#FFE699", "#D6EBF2"))+
  scale_color_manual(values = c("orange", 
                                "blue")) + 
  labs(y = "dCT", x= "Genes", title = "Solyc01g073950")+
  theme_bw() 
ggsave(filename = "W2Solyc01g073950.jpg", 
       path = "C:\\Users\\DELL\\Documents\\Git in R\\Projects\\Figures", 
       height = 2.5, width = 5)



W2Solyc01g073960 <- josh2 %>% 
  filter(Type =="Solyc01g073960", 
         Period == "Week 2") %>% 
  ggplot()+
  geom_boxplot(aes(x = Treatment, y = dCT, fill = Group),
               outlier.colour = NULL,
               outlier.color = "transparent",
               outlier.fill = NULL) +
  geom_point(aes(x = Treatment, y = dCT, colour = Group),
             position = position_jitterdodge(jitter.width = 0.2,
                                             dodge.width = 0.8),
             size = 1.8, alpha = 0.75) +
  scale_fill_manual(values = c("#FFE699", "#D6EBF2"))+
  scale_color_manual(values = c("orange", 
                                "blue")) + 
  labs(y = "dCT", x= "Genes", title = "Solyc01g073960")+
  theme_bw() 
ggsave(filename = "W2Solyc01g073960.jpg", 
       path = "C:\\Users\\DELL\\Documents\\Git in R\\Projects\\Figures", 
       height = 2.5, width = 5)


W2Solyc01g074030 <- josh2 %>% 
  filter(Type =="Solyc01g074030", 
         Period == "Week 2") %>% 
  ggplot()+
  geom_boxplot(aes(x = Treatment, y = dCT, fill = Group),
               outlier.colour = NULL,
               outlier.color = "transparent",
               outlier.fill = NULL) +
  geom_point(aes(x = Treatment, y = dCT, colour = Group),
             position = position_jitterdodge(jitter.width = 0.2,
                                             dodge.width = 0.8),
             size = 1.8, alpha = 0.75) +
  scale_fill_manual(values = c("#FFE699", "#D6EBF2"))+
  scale_color_manual(values = c("orange", 
                                "blue")) + 
  labs(y = "dCT", x= "Genes", title = "Solyc01g074030")+
  theme_bw() 
ggsave(filename = "W2Solyc01g074030.jpg", 
       path = "C:\\Users\\DELL\\Documents\\Git in R\\Projects\\Figures", 
       height = 2.5, width = 5)



W2Solyc01g089850 <- josh2 %>% 
  filter(Type =="Solyc01g089850", 
         Period == "Week 2") %>% 
  ggplot()+
  geom_boxplot(aes(x = Treatment, y = dCT, fill = Group),
               outlier.colour = NULL,
               outlier.color = "transparent",
               outlier.fill = NULL) +
  geom_point(aes(x = Treatment, y = dCT, colour = Group),
             position = position_jitterdodge(jitter.width = 0.2,
                                             dodge.width = 0.8),
             size = 1.8, alpha = 0.75) +
  scale_fill_manual(values = c("#FFE699", "#D6EBF2"))+
  scale_color_manual(values = c("orange", 
                                "blue")) + 
  labs(y = "dCT", x= "Genes", title = "Solyc01g089850")+
  theme_bw() 
ggsave(filename = "W2Solyc01g089850.jpg", 
       path = "C:\\Users\\DELL\\Documents\\Git in R\\Projects\\Figures", 
       height = 2.5, width = 5)

