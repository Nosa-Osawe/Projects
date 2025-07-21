
fullDataset  = read.csv("C:\\Users\\DELL\\Desktop\\ccdata.csv")

prop_fullDataset<- fullDataset %>%
  filter(Name %in% goodSites$Name,
         julianday %in% julianWindow,
         WetLeaves == 0,
         !Name %in% c('Coweeta - BS', 'Coweeta - BB', 'Coweeta - RK')) %>% 
  group_by(Name, Region, ID, ObservationMethod) %>%
  summarize(caterpillar = ifelse(sum(Group == 'caterpillar', na.rm = TRUE) > 0, 1, 0),
            spider = ifelse(sum(Group == 'spider', na.rm = TRUE) > 0, 1, 0),
            beetle = ifelse(sum(Group == 'beetle', na.rm = TRUE) > 0, 1, 0),
            truebug = ifelse(sum(Group == 'truebugs', na.rm = TRUE) > 0, 1, 0),
            hopper = ifelse(sum(Group == 'leafhopper', na.rm = TRUE) > 0, 1, 0),
            ant = ifelse(sum(Group == 'ant', na.rm = TRUE) > 0, 1, 0)) %>% 
  group_by(Name, Region, ObservationMethod) %>% 
  summarise(caterpillar_prop = mean(caterpillar),
            spider_prop = mean(spider),
            beetle_prop = mean(beetle),
            truebug_prop = mean(truebug),
            hopper_prop  = mean(hopper),
            ant_prop = mean(ant),
            Trials = n()) %>% 
  view()

prop_dataset = left_join(prop_fullDataset, sites, by = 'Name')



# Caterpillar 

prop.catLatPlot1<- prop_dataset %>%
  mutate(
    ObservationMethod = factor(ObservationMethod, levels = c("Visual", "Beat sheet")),
    caterpillar_prop = ifelse(ObservationMethod == "Beat sheet", -caterpillar_prop, caterpillar_prop)
  ) %>%
  ggplot(aes(x = Latitude, y = caterpillar_prop, fill = ObservationMethod)) +
  geom_area(alpha = 0.9, position = "identity", color = "black") +
  geom_point(data = prop_dataset %>% filter(ObservationMethod == "Visual"),
             aes(x = Latitude, y = 1.1, size = Trials),
             color = "yellow", shape = 21, fill = "yellow", alpha = 0.3) +
  geom_point(data = prop_dataset %>% filter(ObservationMethod == "Beat sheet"),
             aes(x = Latitude, y = -1.1,  size = Trials),
             shape = 21, fill = "tomato",color = "tomato", alpha = 0.3) +
  geom_hline(yintercept = c(0.5, 0, -0.5), linetype = "dashed", color = "gray40", alpha = 0.4) +
  scale_y_continuous(
    limits = c(-1.2, 1.2),
    breaks = seq(-1, 1, 0.25),
    labels = abs
  ) +
  scale_x_continuous(
    breaks = seq(floor(min(prop_dataset$Latitude)),
                 ceiling(max(prop_dataset$Latitude)),
                 by = 2)
  ) +
  labs(x = "Latitude", y = "Proportion") +
  scale_fill_manual(values = c("yellow", "tomato")) +
  annotation_raster(catImage, ymin = .4, ymax = .55, xmin = 35, xmax = 38)+
  theme_minimal()



# Spiders 

prop.spiLatPlot1<- prop_dataset %>%
  mutate(
    ObservationMethod = factor(ObservationMethod, levels = c("Visual", "Beat sheet")),
    spider_prop = ifelse(ObservationMethod == "Beat sheet", -spider_prop, spider_prop)
  ) %>%
  ggplot(aes(x = Latitude, y = spider_prop, fill = ObservationMethod)) +
  geom_area(alpha = 0.9, position = "identity", color = "black") +
  geom_point(data = prop_dataset %>% filter(ObservationMethod == "Visual"),
             aes(x = Latitude, y = 1.1, size = Trials),
             color = "yellow", shape = 21, fill = "yellow", alpha = 0.3) +
  geom_point(data = prop_dataset %>% filter(ObservationMethod == "Beat sheet"),
             aes(x = Latitude, y = -1.1,  size = Trials),
             shape = 21, fill = "tomato",color = "tomato", alpha = 0.3) +
  geom_hline(yintercept = c(0.5, 0, -0.5), linetype = "dashed", color = "gray40", alpha = 0.4) +
  scale_y_continuous(
    limits = c(-1.2, 1.2),
    breaks = seq(-1, 1, 0.25),
    labels = abs
  ) +
  scale_x_continuous(
    breaks = seq(floor(min(prop_dataset$Latitude)),
                 ceiling(max(prop_dataset$Latitude)),
                 by = 2)
  ) +
  labs(x = "Latitude", y = "Proportion") +
  scale_fill_manual(values = c("yellow", "tomato")) +
  annotation_raster(spiderImage, ymin = 0.6, ymax = 0.9, xmin = 35, xmax = 38) +
  theme_minimal()



# Beetles

prop.betLatPlot1<- prop_dataset %>%
  mutate(
    ObservationMethod = factor(ObservationMethod, levels = c("Visual", "Beat sheet")),
    beetle_prop = ifelse(ObservationMethod == "Beat sheet", -beetle_prop, beetle_prop)
  ) %>%
  ggplot(aes(x = Latitude, y = beetle_prop, fill = ObservationMethod)) +
  geom_area(alpha = 0.9, position = "identity", color = "black") +
  geom_point(data = prop_dataset %>% filter(ObservationMethod == "Visual"),
             aes(x = Latitude, y = 1.1, size = Trials),
             color = "yellow", shape = 21, fill = "yellow", alpha = 0.3) +
  geom_point(data = prop_dataset %>% filter(ObservationMethod == "Beat sheet"),
             aes(x = Latitude, y = -1.1,  size = Trials),
             shape = 21, fill = "tomato",color = "tomato", alpha = 0.3) +
  geom_hline(yintercept = c(0.5, 0, -0.5), linetype = "dashed", color = "gray40", alpha = 0.4) +
  scale_y_continuous(
    limits = c(-1.2, 1.2),
    breaks = seq(-1, 1, 0.25),
    labels = abs
  ) +
  scale_x_continuous(
    breaks = seq(floor(min(prop_dataset$Latitude)),
                 ceiling(max(prop_dataset$Latitude)),
                 by = 2)
  ) +
  labs(x = "Latitude", y = "Proportion") +
  scale_fill_manual(values = c("yellow", "tomato")) +
  annotation_raster(beetleImage, ymin = .7, ymax = .9, xmin = 35, xmax = 38)+
  theme_minimal()



# True Bugs

prop.bugLatPlot1 <-prop_dataset %>%
  mutate(
    ObservationMethod = factor(ObservationMethod, levels = c("Visual", "Beat sheet")),
    truebug_prop = ifelse(ObservationMethod == "Beat sheet", -truebug_prop, truebug_prop)
  ) %>%
  ggplot(aes(x = Latitude, y = truebug_prop, fill = ObservationMethod)) +
  geom_area(alpha = 0.9, position = "identity", color = "black") +
  geom_point(data = prop_dataset %>% filter(ObservationMethod == "Visual"),
             aes(x = Latitude, y = 1.1, size = Trials),
             color = "yellow", shape = 21, fill = "yellow", alpha = 0.3) +
  geom_point(data = prop_dataset %>% filter(ObservationMethod == "Beat sheet"),
             aes(x = Latitude, y = -1.1,  size = Trials),
             shape = 21, fill = "tomato",color = "tomato", alpha = 0.3) +
  geom_hline(yintercept = c(0.5, 0, -0.5), linetype = "dashed", color = "gray40", alpha = 0.4) +
  scale_y_continuous(
    limits = c(-1.2, 1.2),
    breaks = seq(-1, 1, 0.25),
    labels = abs
  ) +
  scale_x_continuous(
    breaks = seq(floor(min(prop_dataset$Latitude)),
                 ceiling(max(prop_dataset$Latitude)),
                 by = 2)
  ) +
  labs(x = "Latitude", y = "Proportion") +
  scale_fill_manual(values = c("yellow", "tomato")) +
  annotation_raster(truebugImage, ymin = .7, ymax = .94, xmin = 35, xmax = 38)+
  theme_minimal()


# True hopper

prop.hopLatPlot1 <- prop_dataset %>%
  mutate(
    ObservationMethod = factor(ObservationMethod, levels = c("Visual", "Beat sheet")),
    hopper_prop = ifelse(ObservationMethod == "Beat sheet", -hopper_prop, hopper_prop)
  ) %>%
  ggplot(aes(x = Latitude, y = hopper_prop, fill = ObservationMethod)) +
  geom_area(alpha = 0.9, position = "identity", color = "black") +
  geom_point(data = prop_dataset %>% filter(ObservationMethod == "Visual"),
             aes(x = Latitude, y = 1.1, size = Trials),
             color = "yellow", shape = 21, fill = "yellow", alpha = 0.3) +
  geom_point(data = prop_dataset %>% filter(ObservationMethod == "Beat sheet"),
             aes(x = Latitude, y = -1.1,  size = Trials),
             shape = 21, fill = "tomato",color = "tomato", alpha = 0.3) +
  geom_hline(yintercept = c(0.5, 0, -0.5), linetype = "dashed", color = "gray40", alpha = 0.4) +
  scale_y_continuous(
    limits = c(-1.2, 1.2),
    breaks = seq(-1, 1, 0.25),
    labels = abs
  ) +
  scale_x_continuous(
    breaks = seq(floor(min(prop_dataset$Latitude)),
                 ceiling(max(prop_dataset$Latitude)),
                 by = 2)
  ) +
  labs(x = "Latitude", y = "Proportion") +
  scale_fill_manual(values = c("yellow", "tomato")) +
  annotation_raster(hopperImage, ymin = .6, ymax = .94, xmin = 35, xmax = 38)+
  theme_minimal()


# Ants

prop.antLatPlot1 <- prop_dataset %>%
  mutate(
    ObservationMethod = factor(ObservationMethod, levels = c("Visual", "Beat sheet")),
    ant_prop = ifelse(ObservationMethod == "Beat sheet", -ant_prop, ant_prop)
  ) %>%
  ggplot(aes(x = Latitude, y = ant_prop, fill = ObservationMethod)) +
  geom_area(alpha = 0.9, position = "identity", color = "black") +
  geom_point(data = prop_dataset %>% filter(ObservationMethod == "Visual"),
             aes(x = Latitude, y = 1.1, size = Trials),
             color = "yellow", shape = 21, fill = "yellow", alpha = 0.3) +
  geom_point(data = prop_dataset %>% filter(ObservationMethod == "Beat sheet"),
             aes(x = Latitude, y = -1.1,  size = Trials),
             shape = 21, fill = "tomato",color = "tomato", alpha = 0.3) +
  geom_hline(yintercept = c(0.5, 0, -0.5), linetype = "dashed", color = "gray40", alpha = 0.4) +
  scale_y_continuous(
    limits = c(-1.2, 1.2),
    breaks = seq(-1, 1, 0.25),
    labels = abs
  ) +
  scale_x_continuous(
    breaks = seq(floor(min(prop_dataset$Latitude)),
                 ceiling(max(prop_dataset$Latitude)),
                 by = 2)
  ) +
  labs(x = "Latitude", y = "Proportion") +
  scale_fill_manual(values = c("yellow", "tomato")) +
  annotation_raster(antImage, ymin = .7, ymax = .9, xmin = 33, xmax = 36)+
  theme_minimal()


ggarrange(prop.catLatPlot1, prop.spiLatPlot1, prop.betLatPlot1,
          prop.bugLatPlot1, prop.hopLatPlot1, prop.antLatPlot1, 
          ncol=2, nrow=3, common.legend = TRUE, legend="bottom")

