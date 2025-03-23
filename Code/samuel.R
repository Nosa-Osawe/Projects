library(tidyverse)
library(readxl)

# use file.choose() to get the file path...

gene <- read.csv("C:\\Users\\DELL\\Documents\\Git in R\\Projects\\Data\\Gene_data.csv")

view(gene)

gene1 <- gene %>% 
  pivot_longer(cols = -c(1,2),
               names_to = "Rep",
               values_to = "Values") %>% 
  mutate(Treatment =factor( Treatment, 
                            levels = c( "Control",
                                        "0.025 g/ml",
                                        "0.05 g/ml",
                                        "0.1 g/ml")),
         Values= as.numeric(Values)) %>% 
  as.data.frame()

# Linear modelling: Comparing the treatments to the control

gene1.KEAP1 <- subset(gene1, Gene=="KEAP1")
keap1.lm <- lm(Values~Treatment, data = gene1.KEAP1 )
summary(keap1.lm)

gene1.GSTD1 <- subset(gene1, Gene=="GSTD1")
GSTD1.lm <- lm(Values~Treatment, data = gene1.GSTD1 )
summary(GSTD1.lm)

gene1.CncC <- subset(gene1, Gene=="CncC")
CncC.lm <- lm(Values~Treatment, data = gene1.CncC )
summary(CncC.lm)

gene1.PGHPx <- subset(gene1, Gene=="PGHPx")
PGHPx.lm <- lm(Values~Treatment, data = gene1.PGHPx )
summary(PGHPx.lm)


#################   CHARTS ##################################################
My_colour <- c("#4daf4a", "#377eb8","#ff7f00","purple","#e31a1c","yellow")
bluess<- c("#87CEEB", "#4682B4", "#4169E1", "#0000FF", "#00008B", "#191970")
red_palette <- c("#FFC0C0", "#FF7F7F", "#FF0000", "#B22222", "#8B0000")   
yellow_palette <- c("#FFFFE0", "#FFFACD", "#FFD700", "#FFA500", "#FF8C00")   
pink_palette <- c("#FFC0CB", "#FF69B4", "#FF1493", "#C71585", "#8B008B")   
green_palette <- c("#98FB98", "#00FA9A", "#32CD32", "#228B22", "#006400") 



# gene1.KEAP1

ggplot(mapping = aes(x= Treatment, y= Values, fill = Treatment ),
       data = gene1.KEAP1)+
  stat_summary(geom = "errorbar", fun.data = 
                 function(yyy) {
                   data.frame(y = mean(yyy), 
                              ymin = mean(yyy) - sd(yyy),
                              ymax = mean(yyy) + sd(yyy))
                 }, 
               linewidth = 1, width= 0.3, color= "black")+
  stat_summary(geom = "bar",fun = mean)+
  scale_fill_manual(values = bluess)+
  scale_colour_manual(values = bluess)+
  labs(y= "KEAP1 Gene Expression")+
  guides(fill="none")+
  theme_classic()

# gene1.GSTD1
ggplot(mapping = aes(x= Treatment, y= Values, fill = Treatment ),
       data = gene1.GSTD1)+
  stat_summary(geom = "errorbar", fun.data = 
                 function(yyy) {
                   data.frame(y = mean(yyy), 
                              ymin = mean(yyy) - sd(yyy),
                              ymax = mean(yyy) + sd(yyy))
                 }, 
               linewidth = 1, width= 0.3, color= "black")+
  stat_summary(geom = "bar",fun = mean)+
  scale_fill_manual(values = green_palette)+
  scale_colour_manual(values = green_palette)+
  labs(y= "GSTD1 Gene Expression")+
  guides(fill="none")+
  theme_classic()

# gene1.CncC

ggplot(mapping = aes(x= Treatment, y= Values, fill = Treatment ),
       data = gene1.CncC)+
  stat_summary(geom = "errorbar", fun.data = 
                 function(yyy) {
                   data.frame(y = mean(yyy), 
                              ymin = mean(yyy) - sd(yyy),
                              ymax = mean(yyy) + sd(yyy))
                 }, 
               linewidth = 1, width= 0.3, color= "black")+
  stat_summary(geom = "bar",fun = mean)+
  scale_fill_manual(values = pink_palette)+
  labs(y= "CncC Gene Expression")+
  guides(fill="none")+
  theme_classic()


# gene1.PGHPx

ggplot(mapping = aes(x= Treatment, y= Values, fill = Treatment ),
       data = gene1.PGHPx)+
  stat_summary(geom = "errorbar", fun.data = 
                 function(yyy) {
                   data.frame(y = mean(yyy), 
                              ymin = mean(yyy) - sd(yyy),
                              ymax = mean(yyy) + sd(yyy))
                 }, 
               linewidth = 1, width= 0.3, color= "black")+
  stat_summary(geom = "bar",fun = mean)+
  scale_fill_manual(values = red_palette)+
  labs(y= "PGHPx Gene Expression")+
  guides(fill="none")+
  theme_classic()



##################################################################@@@@@@@@@@@@@@@

survive <- read_excel("C:\\Users\\DELL\\Desktop\\samuel.xlsx", sheet = 'survival')


survive %>% 
  group_by(Treatment) %>% 
  summarise(across(where(is.numeric), mean)) %>% 
  pivot_longer(
    cols = -Treatment,
    names_to = "Days",
    values_to = "Counts"
  ) %>% 
  mutate(Days = as.integer(str_remove(Days, "D"))) %>% 
  mutate(Treatment= factor(Treatment, 
                            levels = c("Control","0.025g/ml", "0.05g/ml",
                                       "0.1g/ml" ))) %>% 
  as.data.frame() %>% 
  ggplot(aes(x = Days, y = Counts, colour = Treatment, fill = Treatment)) +
  geom_point() +
  stat_smooth(alpha = 0.1) +   
  scale_colour_manual(values = My_colour_choice) +
  scale_fill_manual(values = My_colour_choice) +
  labs( y= "Number of Deaths")+
  scale_x_continuous(breaks = 0:14) +
  theme_light()

#######################################################################################

neg_geo <- read_excel("C:\\Users\\DELL\\Desktop\\samuel.xlsx", sheet = 'neg_geo')

neg_geo<- neg_geo %>% 
  rename(Geotaxis = "Neg_geo(%)") %>%
  mutate(Treatment= factor(Treatment, 
                           levels = c("Control","0.025g/ml", "0.05g/ml",
                                      "0.1g/ml" ))) %>%
  select(Treatment,T1,T2,T3) %>% 
  pivot_longer(cols = -1,
               names_to = "Sessions",
               values_to = "Values") %>% 
   mutate(Values= as.numeric(Values*10)) %>% 
   as.data.frame()
  
  
ggplot(aes(x = Treatment, y = Values, fill = Treatment), data= neg_geo) +
  stat_summary(
    geom = "errorbar", 
    fun.data = mean_se, 
    linewidth = 1, 
    width = 0.4, 
    position = position_dodge(0.7),
    color = "black"
  ) +
  stat_summary(
    geom = "bar", 
    fun = mean, 
    width = 0.7, 
    position = position_dodge(0.75)
  ) +
  scale_fill_manual(values = My_colour_choice)+
  labs(x="Treatment",
       y= "Negative Geotaxism (%)")+
  theme_classic()


lmm<- lm(Values~Treatment, data = neg_geo)
summary(lmm)

neg_geo %>% 
  group_by(Treatment) %>% 
  summarise(Mean_neg_geo=mean(Values),
            Standard_Dev= sd(Values))


#####################################################################################

toxicity<-read_excel("C:\\Users\\DELL\\Desktop\\samuel.xlsx", sheet = 'toxicity')

toxicity%>% 
  group_by(Treatment) %>% 
  summarise(across(where(is.numeric), mean)) %>% 
  pivot_longer(
    cols = -Treatment,
    names_to = "Days",
    values_to = "Counts"
  ) %>% 
  mutate(Days = as.integer(str_remove(Days, "D"))) %>% 
  mutate(Treatment= factor(Treatment, 
                           levels = c("0.1g/ml", "0.5g/ml" ,"1g/ml"))) %>% 
  as.data.frame() %>% 
  ggplot(aes(x = Days, y = Counts, colour = Treatment, fill = Treatment)) +
  geom_point() +
  geom_smooth(stat = "smooth", se=FALSE)+
  scale_colour_manual(values = My_colour_choice) +
  scale_fill_manual(values = My_colour_choice) +
  scale_x_continuous(breaks = 0:7) +
  labs(x= "Days", 
       y= "No. of Deaths")+
  theme_light()
