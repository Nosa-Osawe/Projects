prop_FAGR = fullDataset %>%
  filter(julianday %in% julianWindow,
         WetLeaves == 0,
         !Name %in% c('Coweeta - BS', 'Coweeta - BB', 'Coweeta - RK'),
         sciName == "Fagus grandifolia") %>%
  group_by(Name, ID, ObservationMethod) %>%
  summarize(caterpillar = ifelse(sum(Group == 'caterpillar', na.rm = TRUE) > 0, 1, 0),
            spider = ifelse(sum(Group == 'spider', na.rm = TRUE) > 0, 1, 0),
            beetle = ifelse(sum(Group == 'beetle', na.rm = TRUE) > 0, 1, 0),
            truebug = ifelse(sum(Group == 'truebugs', na.rm = TRUE) > 0, 1, 0),
            hopper = ifelse(sum(Group == 'leafhopper', na.rm = TRUE) > 0, 1, 0),
            ant = ifelse(sum(Group == 'ant', na.rm = TRUE) > 0, 1, 0)) %>% 
  group_by(Name, ObservationMethod) %>% 
  summarise(caterpillar_prop = mean(caterpillar),
            spider_prop = mean(spider),
            beetle_prop = mean(beetle),
            truebug_prop = mean(truebug),
            hopper_prop  = mean(hopper),
            ant_prop = mean(ant),
            Trials = n())  


prop_FAGRdataset = left_join(prop_FAGR, sites, by = 'Name')



propFAGR.cat.Dev.Latitude = glm(caterpillar_prop ~ dev*Latitude + ObservationMethod, 
                            data = prop_FAGRdataset, weights = Trials,  family = "binomial")
summary(propFAGR.cat.Dev.Latitude)

propFAGR.spi.Dev.Latitude = glm(spider_prop ~ dev*Latitude + ObservationMethod, 
                            data = prop_FAGRdataset, weights = Trials,  family = "binomial")
summary(propFAGR.spi.Dev.Latitude)

propFAGR.bet.Dev.Latitude = glm(beetle_prop ~ dev*Latitude + ObservationMethod, 
                            data = prop_FAGRdataset, weights = Trials,  family = "binomial")
summary(propFAGR.bet.Dev.Latitude)

propFAGR.bug.Dev.Latitude = glm(truebug_prop ~ dev*Latitude + ObservationMethod, 
                            data = prop_FAGRdataset, weights = Trials,  family = "binomial")
summary(propFAGR.bug.Dev.Latitude)  

propFAGR.hop.Dev.Latitude = glm(hopper_prop ~ dev*Latitude + ObservationMethod, 
                            data = prop_FAGRdataset, weights = Trials,  family = "binomial")
summary(propFAGR.hop.Dev.Latitude)  

propFAGR.ant.Dev.Latitude = glm(ant_prop ~ dev*Latitude + ObservationMethod, 
                            data = prop_FAGRdataset, weights = Trials,  family = "binomial")
summary(propFAGR.ant.Dev.Latitude)  




# ---- viz the binomial GLMs

propFAGR.catDevPlot1<- interact_plot(propFAGR.cat.Dev.Latitude,
                                 pred = dev, modx = Latitude,
                                 y.label = "Prop. of surveys with caterpillars",
                                 x.lab = "% developed cover", cex.lab = 2, vary.lty = FALSE,
                                 colors = c('darkblue', 'blue', 'powderblue'
                                 ),
                                 line.thickness = 2) +
  geom_point(data = prop_FAGRdataset,
             aes(x = dev,
                 y = caterpillar_prop,
                 size = Trials,
                 shape = ObservationMethod,
                 fill = ObservationMethod),  
             color = "black",   
             alpha = 0.5,
  ) + 
  scale_shape_manual(values = c(21, 22)) +    
  scale_fill_manual(values = c("tomato", "yellow")) +   
  coord_cartesian(ylim = c(0, 0.4))+
  annotation_raster(catImage, ymin = .3, ymax = .35, xmin = 60, xmax = 100)



propFAGR.spiDevPlot1<- interact_plot(propFAGR.spi.Dev.Latitude,
                                 pred = dev, modx = Latitude,
                                 y.label = "Prop. of surveys with spiders",
                                 x.lab = "% developed cover", cex.lab = 2,
                                 vary.lty = FALSE,
                                 line.thickness = 2) +
  scale_color_gradient(low = "darkblue", high = "lightblue")+
  geom_point(data = prop_FAGRdataset,
             aes(x = dev,
                 y = spider_prop,
                 size = Trials,
                 shape = ObservationMethod,
                 fill = ObservationMethod),  
             color = "black",   
             alpha = 0.5,
  ) + 
  scale_shape_manual(values = c(21, 22)) +    
  scale_fill_manual(values = c("tomato", "yellow")) +   
 # coord_cartesian(ylim = c(0, 1))+
  annotation_raster(spiderImage, ymin = .6, ymax = .8, xmin = 20, xmax = 60)



propFAGR.betDevPlot1<- interact_plot(propFAGR.bet.Dev.Latitude,
                                 pred = dev, modx = Latitude,
                                 y.label = "Prop. of surveys with beetles",
                                 x.lab = "% developed cover", cex.lab = 2, vary.lty = FALSE,
                                  
                                 line.thickness = 2) +
  scale_color_gradient(low = "darkblue", high = "lightblue")+
  geom_point(data = prop_FAGRdataset,
             aes(x = dev,
                 y = beetle_prop,
                 size = Trials,
                 shape = ObservationMethod,
                 fill = ObservationMethod),  
             color = "black",   
             alpha = 0.5,
  ) + 
  scale_shape_manual(values = c(21, 22)) +    
  scale_fill_manual(values = c("tomato", "yellow")) +   
 # coord_cartesian(ylim = c(0, 0.9))+
  annotation_raster(beetleImage, ymin = .6, ymax = .75, xmin = 60, xmax = 100)



propFAGR.bugDevPlot1<- interact_plot(propFAGR.bug.Dev.Latitude,
                                 pred = dev, modx = Latitude,
                                 y.label = "Prop. of surveys with true bugs",
                                 x.lab = "% developed cover", cex.lab = 2, vary.lty = FALSE,
                                 line.thickness = 2) +
  scale_color_gradient(low = "darkblue", high = "lightblue")+
  geom_point(data = prop_FAGRdataset,
             aes(x = dev,
                 y = truebug_prop,
                 size = Trials,
                 shape = ObservationMethod,
                 fill = ObservationMethod),  
             color = "black",   
             alpha = 0.5,
  ) + 
  scale_shape_manual(values = c(21, 22)) +    
  scale_fill_manual(values = c("tomato", "yellow")) +   
  coord_cartesian(ylim = c(0, 0.65))+
  annotation_raster(truebugImage, ymin = .5, ymax = .6, xmin = 25, xmax = 65)




propFAGR.hopDevPlot1<- interact_plot(propFAGR.hop.Dev.Latitude,
                                 pred = dev, modx = Latitude,
                                 y.label = "Prop. of surveys with hopper",
                                 x.lab = "% developed cover", cex.lab = 2, vary.lty = FALSE,
                                 line.thickness = 2) +
  scale_color_gradient(low = "darkblue", high = "lightblue")+
  geom_point(data = prop_FAGRdataset,
             aes(x = dev,
                 y = hopper_prop,
                 size = Trials,
                 shape = ObservationMethod,
                 fill = ObservationMethod),  
             color = "black",   
             alpha = 0.5,
  ) + 
  scale_shape_manual(values = c(21, 22)) +    
  scale_fill_manual(values = c("tomato", "yellow")) +   
  coord_cartesian(ylim = c(0, 0.65))+
  annotation_raster(hopperImage, ymin = .5, ymax = .64, xmin = 20, xmax = 60)




propFAGR.antDevPlot1<- interact_plot(propFAGR.ant.Dev.Latitude,
                                 pred = dev, modx = Latitude,
                                 y.label = "Prop. of surveys with ants",
                                 x.lab = "% developed cover", cex.lab = 2, vary.lty = FALSE,
                                 colors = c('darkblue', 'blue', 'powderblue'
                                 ),
                                 line.thickness = 2) +
  geom_point(data = prop_FAGRdataset,
             aes(x = dev,
                 y = ant_prop,
                 size = Trials,
                 shape = ObservationMethod,
                 fill = ObservationMethod),  
             color = "black",   
             alpha = 0.5,
  ) + 
  scale_shape_manual(values = c(21, 22)) +    
  scale_fill_manual(values = c("tomato", "yellow")) +   
  coord_cartesian(ylim = c(0, 0.7))+
  annotation_raster(antImage, ymin = .6, ymax = .7, xmin = 20, xmax = 55)


ggarrange(propFAGR.catDevPlot1, propFAGR.spiDevPlot1, propFAGR.betDevPlot1,
          propFAGR.bugDevPlot1, propFAGR.hopDevPlot1, propFAGR.antDevPlot1, 
          ncol=3, nrow=2, common.legend = TRUE, legend="bottom")




propFAGR.cat.For.Latitude = glm(caterpillar_prop ~ forest*Latitude + ObservationMethod, 
                            data = prop_FAGRdataset, weights = Trials,  family = "binomial")
summary(propFAGR.cat.For.Latitude)


propFAGR.spi.For.Latitude = glm(spider_prop ~ forest*Latitude + ObservationMethod, 
                            data = prop_FAGRdataset, weights = Trials,  family = "binomial")
summary(propFAGR.spi.For.Latitude)

propFAGR.bet.For.Latitude = glm(beetle_prop ~ forest*Latitude + ObservationMethod, 
                            data = prop_FAGRdataset, weights = Trials,  family = "binomial")
summary(propFAGR.bet.For.Latitude)

propFAGR.bug.For.Latitude = glm(truebug_prop ~ forest*Latitude + ObservationMethod, 
                            data = prop_FAGRdataset, weights = Trials,  family = "binomial")
summary(propFAGR.bug.For.Latitude)  


propFAGR.hop.For.Latitude = glm(hopper_prop ~ forest*Latitude + ObservationMethod, 
                            data = prop_FAGRdataset, weights = Trials,  family = "binomial")
summary(propFAGR.hop.For.Latitude)  


propFAGR.ant.For.Latitude = glm(ant_prop ~ forest*Latitude + ObservationMethod, 
                            data = prop_FAGRdataset, weights = Trials,  family = "binomial")
summary(propFAGR.ant.For.Latitude)  





propFAGR.catForPlot1<- interact_plot(propFAGR.cat.For.Latitude,
                                     pred = forest, modx = Latitude,
                                     y.label = "Prop. of surveys with caterpillars",
                                     x.lab = "% developed cover", cex.lab = 2, vary.lty = FALSE,
                                     line.thickness = 2) +
  scale_color_gradient(low = "darkgreen", high = "lightgreen")+
  geom_point(data = prop_FAGRdataset,
             aes(x = forest,
                 y = caterpillar_prop,
                 size = Trials,
                 shape = ObservationMethod,
                 fill = ObservationMethod),  
             color = "black",   
             alpha = 0.5,
  ) + 
  scale_shape_manual(values = c(21, 22)) +    
  scale_fill_manual(values = c("tomato", "yellow")) +   
  coord_cartesian(ylim = c(0, 0.4))+
  annotation_raster(catImage, ymin = .29, ymax = .35, xmin = 33, xmax = 67)






propFAGR.spiForPlot1<- interact_plot(propFAGR.spi.For.Latitude,
                                     pred = forest, modx = Latitude,
                                     y.label = "Prop. of surveys with caterpillars",
                                     x.lab = "% developed cover", cex.lab = 2, vary.lty = FALSE,
                                     line.thickness = 2) +
  scale_color_gradient(low = "darkgreen", high = "lightgreen")+
  geom_point(data = prop_FAGRdataset,
             aes(x = forest,
                 y = spider_prop,
                 size = Trials,
                 shape = ObservationMethod,
                 fill = ObservationMethod),  
             color = "black",   
             alpha = 0.5,
  ) + 
  scale_shape_manual(values = c(21, 22)) +    
  scale_fill_manual(values = c("tomato", "yellow")) +   
  # coord_cartesian(ylim = c(0, 1))+
  annotation_raster(spiderImage, ymin = .7, ymax = .9, xmin = 60, xmax = 98)





propFAGR.betForPlot1<- interact_plot(propFAGR.bet.For.Latitude,
                                     pred = forest, modx = Latitude,
                                     y.label = "Prop. of surveys with beetles",
                                     x.lab = "% developed cover", cex.lab = 2, vary.lty = FALSE,
                                     line.thickness = 2) +
  scale_color_gradient(low = "darkgreen", high = "lightgreen")+
  geom_point(data = prop_FAGRdataset,
             aes(x = forest,
                 y = beetle_prop,
                 size = Trials,
                 shape = ObservationMethod,
                 fill = ObservationMethod),  
             color = "black",   
             alpha = 0.5,
  ) + 
  scale_shape_manual(values = c(21, 22)) +    
  scale_fill_manual(values = c("tomato", "yellow")) +   
  coord_cartesian(ylim = c(0, 0.9))+
  annotation_raster(beetleImage, ymin = .7, ymax = .85, xmin = 60, xmax = 90)




propFAGR.bugForPlot1<- interact_plot(propFAGR.bug.For.Latitude,
                                     pred = forest, modx = Latitude,
                                     y.label = "Prop. of surveys with true bugs",
                                     x.lab = "% developed cover", cex.lab = 2, vary.lty = FALSE,
                                     line.thickness = 2) +
  scale_color_gradient(low = "darkgreen", high = "lightgreen")+
  geom_point(data = prop_FAGRdataset,
             aes(x = forest,
                 y = truebug_prop,
                 size = Trials,
                 shape = ObservationMethod,
                 fill = ObservationMethod),  
             color = "black",   
             alpha = 0.5,
  ) + 
  scale_shape_manual(values = c(21, 22)) +    
  scale_fill_manual(values = c("tomato", "yellow")) +   
  coord_cartesian(ylim = c(0, 0.65))+
  annotation_raster(truebugImage, ymin = .5, ymax = .6, xmin = 45, xmax = 85)


propFAGR.hopForPlot1<- interact_plot(propFAGR.hop.For.Latitude,
                                     pred = forest, modx = Latitude,
                                     y.label = "Prop. of surveys with hopper",
                                     x.lab = "% developed cover", cex.lab = 2, vary.lty = FALSE,
                                     line.thickness = 2) +
  scale_color_gradient(low = "darkgreen", high = "lightgreen")+
  geom_point(data = prop_FAGRdataset,
             aes(x = forest,
                 y = hopper_prop,
                 size = Trials,
                 shape = ObservationMethod,
                 fill = ObservationMethod),  
             color = "black",   
             alpha = 0.5,
  ) + 
  scale_shape_manual(values = c(21, 22)) +    
  scale_fill_manual(values = c("tomato", "yellow")) +   
  coord_cartesian(ylim = c(0, 0.65))+
  annotation_raster(hopperImage, ymin = .5, ymax = .64, xmin = 20, xmax = 60)



propFAGR.antForPlot1<- interact_plot(propFAGR.ant.For.Latitude,
                                     pred = forest, modx = Latitude,
                                     y.label = "Prop. of surveys with ant",
                                     x.lab = "% developed cover", cex.lab = 2, vary.lty = FALSE,
                                     line.thickness = 2) +
  scale_color_gradient(low = "darkgreen", high = "lightgreen")+
  geom_point(data = prop_FAGRdataset,
             aes(x = forest,
                 y = ant_prop,
                 size = Trials,
                 shape = ObservationMethod,
                 fill = ObservationMethod),  
             color = "black",   
             alpha = 0.5,
  ) + 
  scale_shape_manual(values = c(21, 22)) +    
  scale_fill_manual(values = c("tomato", "yellow")) +   
  coord_cartesian(ylim = c(0, 0.7))+
  annotation_raster(antImage, ymin = .6, ymax = .7, xmin = 55, xmax = 85)



ggarrange(propFAGR.catForPlot1, propFAGR.spiForPlot1, propFAGR.betForPlot1,
          propFAGR.bugForPlot1, propFAGR.hopForPlot1, propFAGR.antForPlot1, 
          ncol=3, nrow=2, common.legend = TRUE, legend="bottom")




#--------- Prop_ACRU_________________________________

prop_ACRU = fullDataset %>%
  filter(julianday %in% julianWindow,
         WetLeaves == 0,
         !Name %in% c('Coweeta - BS', 'Coweeta - BB', 'Coweeta - RK'),
         sciName == "Acer rubrum") %>%
  group_by(Name, ID, ObservationMethod) %>%
  summarize(caterpillar = ifelse(sum(Group == 'caterpillar', na.rm = TRUE) > 0, 1, 0),
            spider = ifelse(sum(Group == 'spider', na.rm = TRUE) > 0, 1, 0),
            beetle = ifelse(sum(Group == 'beetle', na.rm = TRUE) > 0, 1, 0),
            truebug = ifelse(sum(Group == 'truebugs', na.rm = TRUE) > 0, 1, 0),
            hopper = ifelse(sum(Group == 'leafhopper', na.rm = TRUE) > 0, 1, 0),
            ant = ifelse(sum(Group == 'ant', na.rm = TRUE) > 0, 1, 0)) %>% 
  group_by(Name, ObservationMethod) %>% 
  summarise(caterpillar_prop = mean(caterpillar),
            spider_prop = mean(spider),
            beetle_prop = mean(beetle),
            truebug_prop = mean(truebug),
            hopper_prop  = mean(hopper),
            ant_prop = mean(ant),
            Trials = n())  

prop_ACRUdataset = left_join(prop_ACRU, sites, by = 'Name')





propACRU.cat.Dev.Latitude = glm(caterpillar_prop ~ dev*Latitude + ObservationMethod, 
                                data = prop_ACRUdataset, weights = Trials,  family = "binomial")
summary(propFAGR.cat.Dev.Latitude)

propACRU.spi.Dev.Latitude = glm(spider_prop ~ dev*Latitude + ObservationMethod, 
                                data = prop_ACRUdataset, weights = Trials,  family = "binomial")
summary(propFAGR.spi.Dev.Latitude)

propACRU.bet.Dev.Latitude = glm(beetle_prop ~ dev*Latitude + ObservationMethod, 
                                data = prop_ACRUdataset, weights = Trials,  family = "binomial")
summary(propFAGR.bet.Dev.Latitude)

propACRU.bug.Dev.Latitude = glm(truebug_prop ~ dev*Latitude + ObservationMethod, 
                                data = prop_ACRUdataset, weights = Trials,  family = "binomial")
summary(propFAGR.bug.Dev.Latitude)  

propACRU.hop.Dev.Latitude = glm(hopper_prop ~ dev*Latitude + ObservationMethod, 
                                data = prop_ACRUdataset, weights = Trials,  family = "binomial")
summary(propFAGR.hop.Dev.Latitude)  

propACRU.ant.Dev.Latitude = glm(ant_prop ~ dev*Latitude + ObservationMethod, 
                                data = prop_ACRUdataset, weights = Trials,  family = "binomial")
summary(propFAGR.ant.Dev.Latitude)  


propACRU.catDevPlot1<- interact_plot(propACRU.cat.Dev.Latitude,
                                     pred = dev, modx = Latitude,
                                     y.label = "Prop. of surveys with caterpillars",
                                     x.lab = "% developed cover", cex.lab = 2,
                                     vary.lty = FALSE,
                                     line.thickness = 2) +
  scale_color_gradient(low = "darkblue", high = "lightblue")+
  geom_point(data = prop_ACRUdataset,
             aes(x = dev,
                 y = caterpillar_prop,
                 size = Trials,
                 shape = ObservationMethod,
                 fill = ObservationMethod),  
             color = "black",   
             alpha = 0.5,
  ) + 
  scale_shape_manual(values = c(21, 22)) +    
  scale_fill_manual(values = c("tomato", "yellow")) +   
  annotation_raster(catImage, ymin = .3, ymax = .35, xmin = 60, xmax = 100)



propACRU.spiDevPlot1<- interact_plot(propACRU.spi.Dev.Latitude,
                                     pred = dev, modx = Latitude,
                                     y.label = "Prop. of surveys with spiders",
                                     x.lab = "% developed cover", cex.lab = 2,
                                     vary.lty = FALSE,
                                     line.thickness = 2) +
  scale_color_gradient(low = "darkblue", high = "lightblue")+
  geom_point(data = prop_ACRUdataset,
             aes(x = dev,
                 y = spider_prop,
                 size = Trials,
                 shape = ObservationMethod,
                 fill = ObservationMethod),  
             color = "black",   
             alpha = 0.5,
  ) + 
  scale_shape_manual(values = c(21, 22)) +    
  scale_fill_manual(values = c("tomato", "yellow")) +   
  # coord_cartesian(ylim = c(0, 1))+
  annotation_raster(spiderImage, ymin = .6, ymax = .8, xmin = 20, xmax = 60)



propACRU.betDevPlot1<- interact_plot(propACRU.bet.Dev.Latitude,
                                     pred = dev, modx = Latitude,
                                     y.label = "Prop. of surveys with beetles",
                                     x.lab = "% developed cover", cex.lab = 2, vary.lty = FALSE,
                                     
                                     line.thickness = 2) +
  scale_color_gradient(low = "darkblue", high = "lightblue")+
  geom_point(data = prop_ACRUdataset,
             aes(x = dev,
                 y = beetle_prop,
                 size = Trials,
                 shape = ObservationMethod,
                 fill = ObservationMethod),  
             color = "black",   
             alpha = 0.5,
  ) + 
  scale_shape_manual(values = c(21, 22)) +    
  scale_fill_manual(values = c("tomato", "yellow")) +   
   coord_cartesian(ylim = c(0, 1))+
  annotation_raster(beetleImage, ymin = .6, ymax = .75, xmin = 60, xmax = 100)



propACRU.bugDevPlot1<- interact_plot(propACRU.bug.Dev.Latitude,
                                     pred = dev, modx = Latitude,
                                     y.label = "Prop. of surveys with true bugs",
                                     x.lab = "% developed cover", cex.lab = 2, vary.lty = FALSE,
                                     line.thickness = 2) +
  scale_color_gradient(low = "darkblue", high = "lightblue")+
  geom_point(data = prop_ACRUdataset,
             aes(x = dev,
                 y = truebug_prop,
                 size = Trials,
                 shape = ObservationMethod,
                 fill = ObservationMethod),  
             color = "black",   
             alpha = 0.5,
  ) + 
  scale_shape_manual(values = c(21, 22)) +    
  scale_fill_manual(values = c("tomato", "yellow")) +   
  coord_cartesian(ylim = c(0, 0.65))+
  annotation_raster(truebugImage, ymin = .5, ymax = .6, xmin = 25, xmax = 65)




propACRU.hopDevPlot1<- interact_plot(propACRU.hop.Dev.Latitude,
                                     pred = dev, modx = Latitude,
                                     y.label = "Prop. of surveys with hopper",
                                     x.lab = "% developed cover", cex.lab = 2, vary.lty = FALSE,
                                     line.thickness = 2) +
  scale_color_gradient(low = "darkblue", high = "lightblue")+
  geom_point(data = prop_ACRUdataset,
             aes(x = dev,
                 y = hopper_prop,
                 size = Trials,
                 shape = ObservationMethod,
                 fill = ObservationMethod),  
             color = "black",   
             alpha = 0.5,
  ) + 
  scale_shape_manual(values = c(21, 22)) +    
  scale_fill_manual(values = c("tomato", "yellow")) +   
  coord_cartesian(ylim = c(0, 0.65))+
  annotation_raster(hopperImage, ymin = .5, ymax = .64, xmin = 20, xmax = 60)




propACRU.antDevPlot1<- interact_plot(propACRU.ant.Dev.Latitude,
                                     pred = dev, modx = Latitude,
                                     y.label = "Prop. of surveys with ants",
                                     x.lab = "% developed cover", cex.lab = 2, vary.lty = FALSE,
                                     colors = c('darkblue', 'blue', 'powderblue'
                                     ),
                                     line.thickness = 2) +
  geom_point(data = prop_ACRUdataset,
             aes(x = dev,
                 y = ant_prop,
                 size = Trials,
                 shape = ObservationMethod,
                 fill = ObservationMethod),  
             color = "black",   
             alpha = 0.5,
  ) + 
  scale_shape_manual(values = c(21, 22)) +    
  scale_fill_manual(values = c("tomato", "yellow")) +   
  coord_cartesian(ylim = c(0, 1))+
  annotation_raster(antImage, ymin = .6, ymax = .8, xmin = 20, xmax = 55)


ggarrange(propACRU.catDevPlot1, propACRU.spiDevPlot1, propACRU.betDevPlot1,
          propACRU.bugDevPlot1, propACRU.hopDevPlot1, propACRU.antDevPlot1, 
          ncol=3, nrow=2, common.legend = TRUE, legend="bottom")



######------- -Quercus rubra ----------------------------------------------------------

prop_QURU = fullDataset %>%
  filter(julianday %in% julianWindow,
         WetLeaves == 0,
         !Name %in% c('Coweeta - BS', 'Coweeta - BB', 'Coweeta - RK'),
         sciName == "Quercus rubra") %>%
  group_by(Name, ID, ObservationMethod) %>%
  summarize(caterpillar = ifelse(sum(Group == 'caterpillar', na.rm = TRUE) > 0, 1, 0),
            spider = ifelse(sum(Group == 'spider', na.rm = TRUE) > 0, 1, 0),
            beetle = ifelse(sum(Group == 'beetle', na.rm = TRUE) > 0, 1, 0),
            truebug = ifelse(sum(Group == 'truebugs', na.rm = TRUE) > 0, 1, 0),
            hopper = ifelse(sum(Group == 'leafhopper', na.rm = TRUE) > 0, 1, 0),
            ant = ifelse(sum(Group == 'ant', na.rm = TRUE) > 0, 1, 0)) %>% 
  group_by(Name, ObservationMethod) %>% 
  summarise(caterpillar_prop = mean(caterpillar),
            spider_prop = mean(spider),
            beetle_prop = mean(beetle),
            truebug_prop = mean(truebug),
            hopper_prop  = mean(hopper),
            ant_prop = mean(ant),
            Trials = n())  


prop_QURUdataset = left_join(prop_QURU, sites, by = 'Name')



propQURU.cat.Dev.Latitude = glm(caterpillar_prop ~ dev*Latitude + ObservationMethod, 
                                data = prop_QURUdataset, weights = Trials,  family = "binomial")
summary(propQURU.cat.Dev.Latitude)

propQURU.spi.Dev.Latitude = glm(spider_prop ~ dev*Latitude + ObservationMethod, 
                                data = prop_QURUdataset, weights = Trials,  family = "binomial")
summary(propQURU.spi.Dev.Latitude)

propQURU.bet.Dev.Latitude = glm(beetle_prop ~ dev*Latitude + ObservationMethod, 
                                data = prop_QURUdataset, weights = Trials,  family = "binomial")
summary(propQURU.bet.Dev.Latitude)

propQURU.bug.Dev.Latitude = glm(truebug_prop ~ dev*Latitude + ObservationMethod, 
                                data = prop_QURUdataset, weights = Trials,  family = "binomial")
summary(propQURU.bug.Dev.Latitude)  

propQURU.hop.Dev.Latitude = glm(hopper_prop ~ dev*Latitude + ObservationMethod, 
                                data = prop_QURUdataset, weights = Trials,  family = "binomial")
summary(propQURU.hop.Dev.Latitude)  

propQURU.ant.Dev.Latitude = glm(ant_prop ~ dev*Latitude + ObservationMethod, 
                                data = prop_QURUdataset, weights = Trials,  family = "binomial")
summary(propQURU.ant.Dev.Latitude)  




# ---- viz the binomial GLMs

propQURU.catDevPlot1<- interact_plot(propQURU.cat.Dev.Latitude,
                                     pred = dev, modx = Latitude,
                                     y.label = "Prop. of surveys with caterpillars",
                                     x.lab = "% developed cover", cex.lab = 2, vary.lty = FALSE,
                                     colors = c('darkblue', 'blue', 'powderblue'
                                     ),
                                     line.thickness = 2) +
  geom_point(data = prop_QURUdataset,
             aes(x = dev,
                 y = caterpillar_prop,
                 size = Trials,
                 shape = ObservationMethod,
                 fill = ObservationMethod),  
             color = "black",   
             alpha = 0.5,
  ) + 
  scale_shape_manual(values = c(21, 22)) +    
  scale_fill_manual(values = c("tomato", "yellow")) +   
 # coord_cartesian(ylim = c(0, 0.4))+
  annotation_raster(catImage, ymin = .3, ymax = .35, xmin = 60, xmax = 100)



propQURU.spiDevPlot1<- interact_plot(propQURU.spi.Dev.Latitude,
                                     pred = dev, modx = Latitude,
                                     y.label = "Prop. of surveys with spiders",
                                     x.lab = "% developed cover", cex.lab = 2,
                                     vary.lty = FALSE,
                                     line.thickness = 2) +
  scale_color_gradient(low = "darkblue", high = "lightblue")+
  geom_point(data = prop_QURUdataset,
             aes(x = dev,
                 y = spider_prop,
                 size = Trials,
                 shape = ObservationMethod,
                 fill = ObservationMethod),  
             color = "black",   
             alpha = 0.5,
  ) + 
  scale_shape_manual(values = c(21, 22)) +    
  scale_fill_manual(values = c("tomato", "yellow")) +   
  # coord_cartesian(ylim = c(0, 1))+
  annotation_raster(spiderImage, ymin = .6, ymax = .8, xmin = 20, xmax = 60)



propQURU.betDevPlot1<- interact_plot(propQURU.bet.Dev.Latitude,
                                     pred = dev, modx = Latitude,
                                     y.label = "Prop. of surveys with beetles",
                                     x.lab = "% developed cover", cex.lab = 2, vary.lty = FALSE,
                                     
                                     line.thickness = 2) +
  scale_color_gradient(low = "darkblue", high = "lightblue")+
  geom_point(data = prop_QURUdataset,
             aes(x = dev,
                 y = beetle_prop,
                 size = Trials,
                 shape = ObservationMethod,
                 fill = ObservationMethod),  
             color = "black",   
             alpha = 0.5,
  ) + 
  scale_shape_manual(values = c(21, 22)) +    
  scale_fill_manual(values = c("tomato", "yellow")) +   
  # coord_cartesian(ylim = c(0, 0.9))+
  annotation_raster(beetleImage, ymin = .6, ymax = .75, xmin = 60, xmax = 100)



propQURU.bugDevPlot1<- interact_plot(propQURU.bug.Dev.Latitude,
                                     pred = dev, modx = Latitude,
                                     y.label = "Prop. of surveys with true bugs",
                                     x.lab = "% developed cover", cex.lab = 2, vary.lty = FALSE,
                                     line.thickness = 2) +
  scale_color_gradient(low = "darkblue", high = "lightblue")+
  geom_point(data = prop_QURUdataset,
             aes(x = dev,
                 y = truebug_prop,
                 size = Trials,
                 shape = ObservationMethod,
                 fill = ObservationMethod),  
             color = "black",   
             alpha = 0.5,
  ) + 
  scale_shape_manual(values = c(21, 22)) +    
  scale_fill_manual(values = c("tomato", "yellow")) +   
  coord_cartesian(ylim = c(0, 0.65))+
  annotation_raster(truebugImage, ymin = .5, ymax = .6, xmin = 25, xmax = 65)




propQURU.hopDevPlot1<- interact_plot(propQURU.hop.Dev.Latitude,
                                     pred = dev, modx = Latitude,
                                     y.label = "Prop. of surveys with hopper",
                                     x.lab = "% developed cover", cex.lab = 2, vary.lty = FALSE,
                                     line.thickness = 2) +
  scale_color_gradient(low = "darkblue", high = "lightblue")+
  geom_point(data = prop_QURUdataset,
             aes(x = dev,
                 y = hopper_prop,
                 size = Trials,
                 shape = ObservationMethod,
                 fill = ObservationMethod),  
             color = "black",   
             alpha = 0.5,
  ) + 
  scale_shape_manual(values = c(21, 22)) +    
  scale_fill_manual(values = c("tomato", "yellow")) +   
  coord_cartesian(ylim = c(0, 0.65))+
  annotation_raster(hopperImage, ymin = .5, ymax = .64, xmin = 20, xmax = 60)




propQURU.antDevPlot1<- interact_plot(propQURU.ant.Dev.Latitude,
                                     pred = dev, modx = Latitude,
                                     y.label = "Prop. of surveys with ants",
                                     x.lab = "% developed cover", cex.lab = 2, vary.lty = FALSE,
                                     colors = c('darkblue', 'blue', 'powderblue'
                                     ),
                                     line.thickness = 2) +
  geom_point(data = prop_QURUdataset,
             aes(x = dev,
                 y = ant_prop,
                 size = Trials,
                 shape = ObservationMethod,
                 fill = ObservationMethod),  
             color = "black",   
             alpha = 0.5,
  ) + 
  scale_shape_manual(values = c(21, 22)) +    
  scale_fill_manual(values = c("tomato", "yellow")) +   
  coord_cartesian(ylim = c(0, 1))+
  annotation_raster(antImage, ymin = .6, ymax = .8, xmin = 20, xmax = 55)


ggarrange(propQURU.catDevPlot1, propQURU.spiDevPlot1, propQURU.betDevPlot1,
          propQURU.bugDevPlot1, propQURU.hopDevPlot1, propQURU.antDevPlot1, 
          ncol=3, nrow=2, common.legend = TRUE, legend="bottom")





# -- - --   - - - - -    Prunus serotina ---------------------------------

prop_PRSE = fullDataset %>%
  filter(julianday %in% julianWindow,
         WetLeaves == 0,
         !Name %in% c('Coweeta - BS', 'Coweeta - BB', 'Coweeta - RK'),
         sciName == "Prunus serotina") %>%
  group_by(Name, ID, ObservationMethod) %>%
  summarize(caterpillar = ifelse(sum(Group == 'caterpillar', na.rm = TRUE) > 0, 1, 0),
            spider = ifelse(sum(Group == 'spider', na.rm = TRUE) > 0, 1, 0),
            beetle = ifelse(sum(Group == 'beetle', na.rm = TRUE) > 0, 1, 0),
            truebug = ifelse(sum(Group == 'truebugs', na.rm = TRUE) > 0, 1, 0),
            hopper = ifelse(sum(Group == 'leafhopper', na.rm = TRUE) > 0, 1, 0),
            ant = ifelse(sum(Group == 'ant', na.rm = TRUE) > 0, 1, 0)) %>% 
  group_by(Name, ObservationMethod) %>% 
  summarise(caterpillar_prop = mean(caterpillar),
            spider_prop = mean(spider),
            beetle_prop = mean(beetle),
            truebug_prop = mean(truebug),
            hopper_prop  = mean(hopper),
            ant_prop = mean(ant),
            Trials = n())  


prop_PRSEdataset = left_join(prop_PRSE, sites, by = 'Name')



propPRSE.cat.Dev.Latitude = glm(caterpillar_prop ~ dev*Latitude + ObservationMethod, 
                                data = prop_PRSEdataset, weights = Trials,  family = "binomial")
summary(propPRSE.cat.Dev.Latitude)

propPRSE.spi.Dev.Latitude = glm(spider_prop ~ dev*Latitude + ObservationMethod, 
                                data = prop_PRSEdataset, weights = Trials,  family = "binomial")
summary(propPRSE.spi.Dev.Latitude)

propPRSE.bet.Dev.Latitude = glm(beetle_prop ~ dev*Latitude + ObservationMethod, 
                                data = prop_PRSEdataset, weights = Trials,  family = "binomial")
summary(propPRSE.bet.Dev.Latitude)

propPRSE.bug.Dev.Latitude = glm(truebug_prop ~ dev*Latitude + ObservationMethod, 
                                data = prop_PRSEdataset, weights = Trials,  family = "binomial")
summary(propPRSE.bug.Dev.Latitude)  

propPRSE.hop.Dev.Latitude = glm(hopper_prop ~ dev*Latitude + ObservationMethod, 
                                data = prop_PRSEdataset, weights = Trials,  family = "binomial")
summary(propPRSE.hop.Dev.Latitude)  

propPRSE.ant.Dev.Latitude = glm(ant_prop ~ dev*Latitude + ObservationMethod, 
                                data = prop_PRSEdataset, weights = Trials,  family = "binomial")
summary(propPRSE.ant.Dev.Latitude)  




# ---- viz the binomial GLMs

propPRSE.catDevPlot1<- interact_plot(propPRSE.cat.Dev.Latitude,
                                     pred = dev, modx = Latitude,
                                     y.label = "Prop. of surveys with caterpillars",
                                     x.lab = "% developed cover", cex.lab = 2, vary.lty = FALSE,
                                     colors = c('darkblue', 'blue', 'powderblue'
                                     ),
                                     line.thickness = 2) +
  geom_point(data = prop_PRSEdataset,
             aes(x = dev,
                 y = caterpillar_prop,
                 size = Trials,
                 shape = ObservationMethod,
                 fill = ObservationMethod),  
             color = "black",   
             alpha = 0.5,
  ) + 
  scale_shape_manual(values = c(21, 22)) +    
  scale_fill_manual(values = c("tomato", "yellow")) +   
  # coord_cartesian(ylim = c(0, 0.4))+
  annotation_raster(catImage, ymin = .3, ymax = .35, xmin = 60, xmax = 100)



propPRSE.spiDevPlot1<- interact_plot(propPRSE.spi.Dev.Latitude,
                                     pred = dev, modx = Latitude,
                                     y.label = "Prop. of surveys with spiders",
                                     x.lab = "% developed cover", cex.lab = 2,
                                     vary.lty = FALSE,
                                     line.thickness = 2) +
  scale_color_gradient(low = "darkblue", high = "lightblue")+
  geom_point(data = prop_PRSEdataset,
             aes(x = dev,
                 y = spider_prop,
                 size = Trials,
                 shape = ObservationMethod,
                 fill = ObservationMethod),  
             color = "black",   
             alpha = 0.5,
  ) + 
  scale_shape_manual(values = c(21, 22)) +    
  scale_fill_manual(values = c("tomato", "yellow")) +   
  # coord_cartesian(ylim = c(0, 1))+
  annotation_raster(spiderImage, ymin = .6, ymax = .8, xmin = 20, xmax = 60)



propPRSE.betDevPlot1<- interact_plot(propPRSE.bet.Dev.Latitude,
                                     pred = dev, modx = Latitude,
                                     y.label = "Prop. of surveys with beetles",
                                     x.lab = "% developed cover", cex.lab = 2, vary.lty = FALSE,
                                     
                                     line.thickness = 2) +
  scale_color_gradient(low = "darkblue", high = "lightblue")+
  geom_point(data = prop_PRSEdataset,
             aes(x = dev,
                 y = beetle_prop,
                 size = Trials,
                 shape = ObservationMethod,
                 fill = ObservationMethod),  
             color = "black",   
             alpha = 0.5,
  ) + 
  scale_shape_manual(values = c(21, 22)) +    
  scale_fill_manual(values = c("tomato", "yellow")) +   
  # coord_cartesian(ylim = c(0, 0.9))+
  annotation_raster(beetleImage, ymin = .6, ymax = .75, xmin = 60, xmax = 100)



propPRSE.bugDevPlot1<- interact_plot(propPRSE.bug.Dev.Latitude,
                                     pred = dev, modx = Latitude,
                                     y.label = "Prop. of surveys with true bugs",
                                     x.lab = "% developed cover", cex.lab = 2, vary.lty = FALSE,
                                     line.thickness = 2) +
  scale_color_gradient(low = "darkblue", high = "lightblue")+
  geom_point(data = prop_PRSEdataset,
             aes(x = dev,
                 y = truebug_prop,
                 size = Trials,
                 shape = ObservationMethod,
                 fill = ObservationMethod),  
             color = "black",   
             alpha = 0.5,
  ) + 
  scale_shape_manual(values = c(21, 22)) +    
  scale_fill_manual(values = c("tomato", "yellow")) +   
  coord_cartesian(ylim = c(0, 0.65))+
  annotation_raster(truebugImage, ymin = .5, ymax = .6, xmin = 25, xmax = 65)




propPRSE.hopDevPlot1<- interact_plot(propPRSE.hop.Dev.Latitude,
                                     pred = dev, modx = Latitude,
                                     y.label = "Prop. of surveys with hopper",
                                     x.lab = "% developed cover", cex.lab = 2, vary.lty = FALSE,
                                     line.thickness = 2) +
  scale_color_gradient(low = "darkblue", high = "lightblue")+
  geom_point(data = prop_PRSEdataset,
             aes(x = dev,
                 y = hopper_prop,
                 size = Trials,
                 shape = ObservationMethod,
                 fill = ObservationMethod),  
             color = "black",   
             alpha = 0.5,
  ) + 
  scale_shape_manual(values = c(21, 22)) +    
  scale_fill_manual(values = c("tomato", "yellow")) +   
  coord_cartesian(ylim = c(0, 0.65))+
  annotation_raster(hopperImage, ymin = .5, ymax = .64, xmin = 20, xmax = 60)




propPRSE.antDevPlot1<- interact_plot(propPRSE.ant.Dev.Latitude,
                                     pred = dev, modx = Latitude,
                                     y.label = "Prop. of surveys with ants",
                                     x.lab = "% developed cover", cex.lab = 2, vary.lty = FALSE,
                                     colors = c('darkblue', 'blue', 'powderblue'
                                     ),
                                     line.thickness = 2) +
  geom_point(data = prop_PRSEdataset,
             aes(x = dev,
                 y = ant_prop,
                 size = Trials,
                 shape = ObservationMethod,
                 fill = ObservationMethod),  
             color = "black",   
             alpha = 0.5,
  ) + 
  scale_shape_manual(values = c(21, 22)) +    
  scale_fill_manual(values = c("tomato", "yellow")) +   
  coord_cartesian(ylim = c(0, 1))+
  annotation_raster(antImage, ymin = .6, ymax = .8, xmin = 20, xmax = 55)


ggarrange(propPRSE.catDevPlot1, propPRSE.spiDevPlot1, propPRSE.betDevPlot1,
          propPRSE.bugDevPlot1, propPRSE.hopDevPlot1, propPRSE.antDevPlot1, 
          ncol=3, nrow=2, common.legend = TRUE, legend="bottom")





# --- ---Nyssa sylvatica - -------------


prop_NYSY = fullDataset %>%
  filter(julianday %in% julianWindow,
         WetLeaves == 0,
         !Name %in% c('Coweeta - BS', 'Coweeta - BB', 'Coweeta - RK'),
         sciName == "Nyssa sylvatica") %>%
  group_by(Name, ID, ObservationMethod) %>%
  summarize(caterpillar = ifelse(sum(Group == 'caterpillar', na.rm = TRUE) > 0, 1, 0),
            spider = ifelse(sum(Group == 'spider', na.rm = TRUE) > 0, 1, 0),
            beetle = ifelse(sum(Group == 'beetle', na.rm = TRUE) > 0, 1, 0),
            truebug = ifelse(sum(Group == 'truebugs', na.rm = TRUE) > 0, 1, 0),
            hopper = ifelse(sum(Group == 'leafhopper', na.rm = TRUE) > 0, 1, 0),
            ant = ifelse(sum(Group == 'ant', na.rm = TRUE) > 0, 1, 0)) %>% 
  group_by(Name, ObservationMethod) %>% 
  summarise(caterpillar_prop = mean(caterpillar),
            spider_prop = mean(spider),
            beetle_prop = mean(beetle),
            truebug_prop = mean(truebug),
            hopper_prop  = mean(hopper),
            ant_prop = mean(ant),
            Trials = n())  


prop_NYSYdataset = left_join(prop_NYSY, sites, by = 'Name')



propNYSY.cat.Dev.Latitude = glm(caterpillar_prop ~ dev*Latitude + ObservationMethod, 
                                data = prop_NYSYdataset, weights = Trials,  family = "binomial")
summary(propNYSY.cat.Dev.Latitude)

propNYSY.spi.Dev.Latitude = glm(spider_prop ~ dev*Latitude + ObservationMethod, 
                                data = prop_NYSYdataset, weights = Trials,  family = "binomial")
summary(propNYSY.spi.Dev.Latitude)

propNYSY.bet.Dev.Latitude = glm(beetle_prop ~ dev*Latitude + ObservationMethod, 
                                data = prop_NYSYdataset, weights = Trials,  family = "binomial")
summary(propNYSY.bet.Dev.Latitude)

propNYSY.bug.Dev.Latitude = glm(truebug_prop ~ dev*Latitude + ObservationMethod, 
                                data = prop_NYSYdataset, weights = Trials,  family = "binomial")
summary(propNYSY.bug.Dev.Latitude)  

propNYSY.hop.Dev.Latitude = glm(hopper_prop ~ dev*Latitude + ObservationMethod, 
                                data = prop_NYSYdataset, weights = Trials,  family = "binomial")
summary(propNYSY.hop.Dev.Latitude)  

propNYSY.ant.Dev.Latitude = glm(ant_prop ~ dev*Latitude + ObservationMethod, 
                                data = prop_NYSYdataset, weights = Trials,  family = "binomial")
summary(propNYSY.ant.Dev.Latitude)  




# ---- viz the binomial GLMs

propNYSY.catDevPlot1<- interact_plot(propNYSY.cat.Dev.Latitude,
                                     pred = dev, modx = Latitude,
                                     y.label = "Prop. of surveys with caterpillars",
                                     x.lab = "% developed cover", cex.lab = 2, vary.lty = FALSE,
                                     colors = c('darkblue', 'blue', 'powderblue'
                                     ),
                                     line.thickness = 2) +
  geom_point(data = prop_NYSYdataset,
             aes(x = dev,
                 y = caterpillar_prop,
                 size = Trials,
                 shape = ObservationMethod,
                 fill = ObservationMethod),  
             color = "black",   
             alpha = 0.5,
  ) + 
  scale_shape_manual(values = c(21, 22)) +    
  scale_fill_manual(values = c("tomato", "yellow")) +   
  coord_cartesian(ylim = c(0, 1))+
  annotation_raster(catImage, ymin = .3, ymax = .35, xmin = 60, xmax = 100)



propNYSY.spiDevPlot1<- interact_plot(propNYSY.spi.Dev.Latitude,
                                     pred = dev, modx = Latitude,
                                     y.label = "Prop. of surveys with spiders",
                                     x.lab = "% developed cover", cex.lab = 2,
                                     vary.lty = FALSE,
                                     line.thickness = 2) +
  scale_color_gradient(low = "darkblue", high = "lightblue")+
  geom_point(data = prop_NYSYdataset,
             aes(x = dev,
                 y = spider_prop,
                 size = Trials,
                 shape = ObservationMethod,
                 fill = ObservationMethod),  
             color = "black",   
             alpha = 0.5,
  ) + 
  scale_shape_manual(values = c(21, 22)) +    
  scale_fill_manual(values = c("tomato", "yellow")) +   
  coord_cartesian(ylim = c(0, 1))+
  annotation_raster(spiderImage, ymin = .6, ymax = .8, xmin = 20, xmax = 60)



propNYSY.betDevPlot1<- interact_plot(propNYSY.bet.Dev.Latitude,
                                     pred = dev, modx = Latitude,
                                     y.label = "Prop. of surveys with beetles",
                                     x.lab = "% developed cover", cex.lab = 2, vary.lty = FALSE,
                                     
                                     line.thickness = 2) +
  scale_color_gradient(low = "darkblue", high = "lightblue")+
  geom_point(data = prop_NYSYdataset,
             aes(x = dev,
                 y = beetle_prop,
                 size = Trials,
                 shape = ObservationMethod,
                 fill = ObservationMethod),  
             color = "black",   
             alpha = 0.5,
  ) + 
  scale_shape_manual(values = c(21, 22)) +    
  scale_fill_manual(values = c("tomato", "yellow")) +   
  coord_cartesian(ylim = c(0, 1))+
  annotation_raster(beetleImage, ymin = .6, ymax = .75, xmin = 60, xmax = 100)



propNYSY.bugDevPlot1<- interact_plot(propNYSY.bug.Dev.Latitude,
                                     pred = dev, modx = Latitude,
                                     y.label = "Prop. of surveys with true bugs",
                                     x.lab = "% developed cover", cex.lab = 2, vary.lty = FALSE,
                                     line.thickness = 2) +
  scale_color_gradient(low = "darkblue", high = "lightblue")+
  geom_point(data = prop_NYSYdataset,
             aes(x = dev,
                 y = truebug_prop,
                 size = Trials,
                 shape = ObservationMethod,
                 fill = ObservationMethod),  
             color = "black",   
             alpha = 0.5,
  ) + 
  scale_shape_manual(values = c(21, 22)) +    
  scale_fill_manual(values = c("tomato", "yellow")) +   
  coord_cartesian(ylim = c(0, 0.65))+
  annotation_raster(truebugImage, ymin = .5, ymax = .6, xmin = 25, xmax = 65)




propNYSY.hopDevPlot1<- interact_plot(propNYSY.hop.Dev.Latitude,
                                     pred = dev, modx = Latitude,
                                     y.label = "Prop. of surveys with hopper",
                                     x.lab = "% developed cover", cex.lab = 2, vary.lty = FALSE,
                                     line.thickness = 2) +
  scale_color_gradient(low = "darkblue", high = "lightblue")+
  geom_point(data = prop_NYSYdataset,
             aes(x = dev,
                 y = hopper_prop,
                 size = Trials,
                 shape = ObservationMethod,
                 fill = ObservationMethod),  
             color = "black",   
             alpha = 0.5,
  ) + 
  scale_shape_manual(values = c(21, 22)) +    
  scale_fill_manual(values = c("tomato", "yellow")) +   
  coord_cartesian(ylim = c(0, 0.65))+
  annotation_raster(hopperImage, ymin = .5, ymax = .64, xmin = 20, xmax = 60)




propNYSY.antDevPlot1<- interact_plot(propNYSY.ant.Dev.Latitude,
                                     pred = dev, modx = Latitude,
                                     y.label = "Prop. of surveys with ants",
                                     x.lab = "% developed cover", cex.lab = 2, vary.lty = FALSE,
                                     colors = c('darkblue', 'blue', 'powderblue'
                                     ),
                                     line.thickness = 2) +
  geom_point(data = prop_NYSYdataset,
             aes(x = dev,
                 y = ant_prop,
                 size = Trials,
                 shape = ObservationMethod,
                 fill = ObservationMethod),  
             color = "black",   
             alpha = 0.5,
  ) + 
  scale_shape_manual(values = c(21, 22)) +    
  scale_fill_manual(values = c("tomato", "yellow")) +   
  coord_cartesian(ylim = c(0, 1))+
  annotation_raster(antImage, ymin = .6, ymax = .8, xmin = 20, xmax = 55)


ggarrange(propNYSY.catDevPlot1, propNYSY.spiDevPlot1, propNYSY.betDevPlot1,
          propNYSY.bugDevPlot1, propNYSY.hopDevPlot1, propNYSY.antDevPlot1, 
          ncol=3, nrow=2, common.legend = TRUE, legend="bottom")

