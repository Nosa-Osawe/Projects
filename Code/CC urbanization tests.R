library(effectsize)
library(performance)

fullDataset = read.csv("https://raw.githubusercontent.com/hurlbertlab/caterpillars-analysis-public/refs/heads/master/data/fullDataset_2025-06-17.csv")

# write.csv(fullDataset, "C:\\Users\\DELL\\Desktop\\ccdata.csv")
# or locally...
fullDataset  = read.csv("C:\\Users\\DELL\\Desktop\\ccdata.csv")




minSurveys = 50
julianWindow = 152:213
fullDataset %>%
  filter(julianday %in% julianWindow,
         Longitude > -100,
         WetLeaves == 0) %>%
  group_by(Name, ObservationMethod) %>%
  summarize(nSurvs = n_distinct(ID)) %>%
  filter(nSurvs >= minSurveys) %>% 
  arrange(desc(nSurvs)) %>% 
  select(Name) %>% 
  summarise(count = n()) %>% 
  arrange(!desc(count)) %>% 
  View()

fullDataset %>%
  filter(Name %in% goodSites$Name,
         julianday %in% julianWindow,
         WetLeaves == 0,
         !Name %in% c('Coweeta - BS', 'Coweeta - BB', 'Coweeta - RK'))

  group_by(Name, ObservationMethod) %>%
  summarize(nSurvs = n_distinct(ID)) %>%
  filter(nSurvs >= minSurveys) %>%
  arrange(desc(nSurvs))

fullDataset %>%
  filter(julianday %in% julianWindow,
         WetLeaves == 0,
         !Name %in% c('Coweeta - BS', 'Coweeta - BB', 'Coweeta - RK')) %>% 
  group_by(sciName) %>% 
  summarise(count= n()) %>%
  arrange(desc(count)) %>% 
  print(n=25)

fullDataset %>% 
  head(3) %>% 
  View()

dataset %>% 
  group_by(Name, ObservationMethod) %>%
  summarize(nSurvs = n_distinct(ID)) %>%
  arrange(desc(nSurvs)) %>% 
  View()  # Some sites here have nSurvs (number of surveys) less than 50

length(unique(dataset$Name))

goodData %>% 
  group_by(ObservationMethod, Name) %>% 
  summarise(count = n(),
            names.site = n_distinct(Name)) %>% 
  arrange(count) %>% 
  View()

goodData %>% 
  group_by(ObservationMethod) %>% 
  summarise(min.cat = across(where(is.numeric), min),
            max.cat = across(where(is.numeric),max)) 



fullDataset %>%
  filter(Name %in% goodSites$Name,
         julianday %in% julianWindow,
         WetLeaves == 0,
         !Name %in% c('Coweeta - BS', 'Coweeta - BB', 'Coweeta - RK'),
         Group%in%  c('caterpillar', 'spider', 'ant', 'leafhopper', 'beetle', 'truebugs')
         ) %>% 
  group_by(Name, ObservationMethod) %>%
  summarize(caterpillar = sum(Group == 'caterpillar'),
            spider = sum(Group == 'spider'),
            beetle = sum(Group == 'beetle'),
            truebug = sum(Group == 'truebugs'),
            hopper = sum(Group == 'leafhopper'),
            ant = sum(Group == 'ant'),
            distinc.ID = n_distinct(ID)) %>% 
  arrange(desc(distinc.ID)) %>% 
  View() # number of distinct survey IDs for each sites (by observation methods)
        #  Where there is at least one observation of arthropods. 


fullDataset %>%
  filter(Name %in% goodSites$Name,
         julianday %in% julianWindow,
         WetLeaves == 0,
         !Name %in% c('Coweeta - BS', 'Coweeta - BB', 'Coweeta - RK')) %>% 
  group_by(Name, ID, ObservationMethod) %>%
  summarize(caterpillar = ifelse(sum(Group == 'caterpillar', na.rm = TRUE) > 0, 1, 0),
            spider = ifelse(sum(Group == 'spider', na.rm = TRUE) > 0, 1, 0),
            beetle = ifelse(sum(Group == 'beetle', na.rm = TRUE) > 0, 1, 0),
            truebug = ifelse(sum(Group == 'truebugs', na.rm = TRUE) > 0, 1, 0),
            hopper = ifelse(sum(Group == 'leafhopper', na.rm = TRUE) > 0, 1, 0),
            ant = ifelse(sum(Group == 'ant', na.rm = TRUE) > 0, 1, 0)) %>% 
  View()

 
 prop_fullDataset<- fullDataset %>%
  filter(Name %in% goodSites$Name,
         julianday %in% julianWindow,
         WetLeaves == 0,
         !Name %in% c('Coweeta - BS', 'Coweeta - BB', 'Coweeta - RK')) %>% 
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
 
 
 prop_dataset = left_join(prop_fullDataset, sites, by = 'Name')
 
 datasetBS = left_join(goodDataBS, sites, by = 'Name')
 
 datasetVis = left_join(goodDataVis, sites, by = 'Name')
 
 datasetACRU = left_join(goodDataACRU, sites, by = 'Name')
 

 
 
 #--------- --- lets make Binomial GLM -------------------------
 
 prop.cat.Dev.Latitude = glm(caterpillar_prop ~ dev*Latitude + ObservationMethod, 
                        data = prop_dataset, weights = Trials,  family = "binomial")
 summary(prop.cat.Dev.Latitude)
 # compare to: summary(cat.Dev.Latitude)
 
 
 prop.spi.Dev.Latitude = glm(spider_prop ~ dev*Latitude + ObservationMethod, 
                             data = prop_dataset, weights = Trials,  family = "binomial")
 summary(prop.spi.Dev.Latitude)
 
 prop.bet.Dev.Latitude = glm(beetle_prop ~ dev*Latitude + ObservationMethod, 
                             data = prop_dataset, weights = Trials,  family = "binomial")
 summary(prop.bet.Dev.Latitude)
 
 prop.bug.Dev.Latitude = glm(truebug_prop ~ dev*Latitude + ObservationMethod, 
                             data = prop_dataset, weights = Trials,  family = "binomial")
 summary(prop.bug.Dev.Latitude) # no interaction effect
 
 
 prop.hop.Dev.Latitude = glm(hopper_prop ~ dev*Latitude + ObservationMethod, 
                             data = prop_dataset, weights = Trials,  family = "binomial")
 summary(prop.hop.Dev.Latitude)  
 

 
 prop.ant.Dev.Latitude = glm(ant_prop ~ dev*Latitude + ObservationMethod, 
                             data = prop_dataset, weights = Trials,  family = "binomial")
 summary(prop.ant.Dev.Latitude)  
 
 
 # ---- viz the binomial GLMs
 
 prop.catDevPlot1<- interact_plot(prop.cat.Dev.Latitude,
               pred = dev, modx = Latitude,
               y.label = "Prop. of surveys with caterpillars",
               x.lab = "% developed cover", cex.lab = 2, vary.lty = FALSE,
               colors = c('darkblue', 'blue', 'powderblue'
                          ),
               line.thickness = 2) +
   geom_point(data = prop_dataset,
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
 
 
 
 
 prop.spiDevPlot1<- interact_plot(prop.spi.Dev.Latitude,
                                  pred = dev, modx = Latitude,
                                  y.label = "Prop. of surveys with spiders",
                                  x.lab = "% developed cover", cex.lab = 2, vary.lty = FALSE,
                                  colors = c('darkblue', 'blue', 'powderblue'
                                  ),
                                  line.thickness = 2) +
   geom_point(data = prop_dataset,
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
   coord_cartesian(ylim = c(0, 0.9))+
   annotation_raster(spiderImage, ymin = .6, ymax = .8, xmin = 20, xmax = 60)
 
 
 
 
 
 prop.betDevPlot1<- interact_plot(prop.bet.Dev.Latitude,
                                  pred = dev, modx = Latitude,
                                  y.label = "Prop. of surveys with beetles",
                                  x.lab = "% developed cover", cex.lab = 2, vary.lty = FALSE,
                                  colors = c('darkblue', 'blue', 'powderblue'
                                  ),
                                  line.thickness = 2) +
   geom_point(data = prop_dataset,
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
   coord_cartesian(ylim = c(0, 0.9))+
   annotation_raster(beetleImage, ymin = .6, ymax = .75, xmin = 60, xmax = 100)
 
 
 
 
 
 prop.bugDevPlot1<- interact_plot(prop.bug.Dev.Latitude,
                                  pred = dev, modx = Latitude,
                                  y.label = "Prop. of surveys with true bugs",
                                  x.lab = "% developed cover", cex.lab = 2, vary.lty = FALSE,
                                  colors = c('darkblue', 'blue', 'powderblue'
                                  ),
                                  line.thickness = 2) +
   geom_point(data = prop_dataset,
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
 
 
 
 
 
 prop.hopDevPlot1<- interact_plot(prop.hop.Dev.Latitude,
                                  pred = dev, modx = Latitude,
                                  y.label = "Prop. of surveys with hopper",
                                  x.lab = "% developed cover", cex.lab = 2, vary.lty = FALSE,
                                  colors = c('darkblue', 'blue', 'powderblue'
                                  ),
                                  line.thickness = 2) +
   geom_point(data = prop_dataset,
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
 
 
 
 prop.antDevPlot1<- interact_plot(prop.ant.Dev.Latitude,
                                  pred = dev, modx = Latitude,
                                  y.label = "Prop. of surveys with ants",
                                  x.lab = "% developed cover", cex.lab = 2, vary.lty = FALSE,
                                  colors = c('darkblue', 'blue', 'powderblue'
                                  ),
                                  line.thickness = 2) +
   geom_point(data = prop_dataset,
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
 
 
 ggarrange(prop.catDevPlot1, prop.spiDevPlot1, prop.betDevPlot1,
           prop.bugDevPlot1, prop.hopDevPlot1, prop.antDevPlot1, 
           ncol=3, nrow=2, common.legend = TRUE, legend="bottom")
 
 
 
 
 
 # Check what is going on with forest covers

 
 
 
 prop.cat.For.Latitude = glm(caterpillar_prop ~ forest*Latitude + ObservationMethod, 
                             data = prop_dataset, weights = Trials,  family = "binomial")
 summary(prop.cat.For.Latitude)
 
 
 prop.spi.For.Latitude = glm(spider_prop ~ forest*Latitude + ObservationMethod, 
                             data = prop_dataset, weights = Trials,  family = "binomial")
 summary(prop.spi.For.Latitude)
 
 prop.bet.For.Latitude = glm(beetle_prop ~ forest*Latitude + ObservationMethod, 
                             data = prop_dataset, weights = Trials,  family = "binomial")
 summary(prop.bet.For.Latitude)
 
 prop.bug.For.Latitude = glm(truebug_prop ~ forest*Latitude + ObservationMethod, 
                             data = prop_dataset, weights = Trials,  family = "binomial")
 summary(prop.bug.For.Latitude)  
 
 
 prop.hop.For.Latitude = glm(hopper_prop ~ forest*Latitude + ObservationMethod, 
                             data = prop_dataset, weights = Trials,  family = "binomial")
 summary(prop.hop.For.Latitude)  
 
 
 prop.ant.For.Latitude = glm(ant_prop ~ forest*Latitude + ObservationMethod, 
                             data = prop_dataset, weights = Trials,  family = "binomial")
 summary(prop.ant.For.Latitude)  
 
 
 
 
 # PLOTS
 
 
 
 
 prop.CatForPlot1 <- interact_plot(prop.cat.For.Latitude,
               pred = forest, modx = Latitude,
               y.label = "Prop. of surveys with caterpillars",
               x.lab = "% forest cover",   
               cex.lab = 2,
               vary.lty = FALSE,
               line.thickness = 2) +
   scale_color_gradient(low = "darkgreen", high = "lightgreen")+
   geom_point(data = prop_dataset,
              aes(x = forest,
                  y = caterpillar_prop,
                  size = Trials,
                  shape = ObservationMethod,
                  fill = ObservationMethod),
              color = "black",
              alpha = 0.5) +
   scale_shape_manual(values = c(21, 22)) +
   scale_fill_manual(values = c("tomato", "yellow")) +
   coord_cartesian(ylim = c(0, 0.4)) +
   annotation_raster(catImage, ymin = .3, ymax = .35, xmin = 60, xmax = 100)
   
 
 
 
 
 
 
 prop.SpiForPlot1<- interact_plot(prop.spi.For.Latitude,
                                  pred = forest, modx = Latitude,
                                  y.label = "Prop. of surveys with spiders",
                                  x.lab = "% forest cover",   
                                  cex.lab = 2,
                                  vary.lty = FALSE,
                                  line.thickness = 2) +
   scale_color_gradient(low = "darkgreen", high = "lightgreen")+
   geom_point(data = prop_dataset,
              aes(x = forest,
                  y = spider_prop,
                  size = Trials,
                  shape = ObservationMethod,
                  fill = ObservationMethod),
              color = "black",
              alpha = 0.5) +
   scale_shape_manual(values = c(21, 22)) +
   scale_fill_manual(values = c("tomato", "yellow")) +
   coord_cartesian(ylim = c(0, 0.8)) +
   annotation_raster(spiderImage, ymin = .6, ymax = .8, xmin = 60, xmax = 95)
 
 
 
 prop.BetForPlot1<- interact_plot(prop.bet.For.Latitude,
                                  pred = forest, modx = Latitude,
                                  y.label = "Prop. of surveys with beetles",
                                  x.lab = "% forest cover",   
                                  cex.lab = 2,
                                  vary.lty = FALSE,
                                  line.thickness = 2) +
   scale_color_gradient(low = "darkgreen", high = "lightgreen")+
   geom_point(data = prop_dataset,
              aes(x = forest,
                  y = beetle_prop,
                  size = Trials,
                  shape = ObservationMethod,
                  fill = ObservationMethod),
              color = "black",
              alpha = 0.5) +
   scale_shape_manual(values = c(21, 22)) +
   scale_fill_manual(values = c("tomato", "yellow")) +
   coord_cartesian(ylim = c(0, 0.9))+
   annotation_raster(beetleImage, ymin = .6, ymax = .75, xmin = 60, xmax = 90)
 
 
 
 prop.BugForPlot1<- interact_plot(prop.bug.For.Latitude,
                                  pred = forest, modx = Latitude,
                                  y.label = "Prop. of surveys with true bugs",
                                  x.lab = "% forest cover",   
                                  cex.lab = 2,
                                  vary.lty = FALSE,
                                  line.thickness = 2) +
   scale_color_gradient(low = "darkgreen", high = "lightgreen")+
   geom_point(data = prop_dataset,
              aes(x = forest,
                  y = truebug_prop,
                  size = Trials,
                  shape = ObservationMethod,
                  fill = ObservationMethod),
              color = "black",
              alpha = 0.5) +
   scale_shape_manual(values = c(21, 22)) +
   scale_fill_manual(values = c("tomato", "yellow")) +
   coord_cartesian(ylim = c(0, 0.65))+
   annotation_raster(truebugImage, ymin = .5, ymax = .6, xmin = 25, xmax = 65)
 
 
 
 
 
 
 prop.HopForPlot1<- interact_plot(prop.hop.For.Latitude,
                                  pred = forest, modx = Latitude,
                                  y.label = "Prop. of surveys with hopper",
                                  x.lab = "% forest cover",   
                                  cex.lab = 2,
                                  vary.lty = FALSE,
                                  line.thickness = 2) +
   scale_color_gradient(low = "darkgreen", high = "lightgreen")+
   geom_point(data = prop_dataset,
              aes(x = forest,
                  y = hopper_prop,
                  size = Trials,
                  shape = ObservationMethod,
                  fill = ObservationMethod),
              color = "black",
              alpha = 0.5) +
   scale_shape_manual(values = c(21, 22)) +
   scale_fill_manual(values = c("tomato", "yellow")) +
   coord_cartesian(ylim = c(0, 0.65))+
   annotation_raster(hopperImage, ymin = .5, ymax = .64, xmin = 20, xmax = 60)
   
 
 
 
 
 
 
 prop.AntForPlot1<- interact_plot(prop.ant.For.Latitude,
                                  pred = forest, modx = Latitude,
                                  y.label = "Prop. of surveys with ants",
                                  x.lab = "% forest cover",   
                                  cex.lab = 2,
                                  vary.lty = FALSE,
                                  line.thickness = 2) +
   scale_color_gradient(low = "darkgreen", high = "lightgreen")+
   geom_point(data = prop_dataset,
              aes(x = forest,
                  y = ant_prop,
                  size = Trials,
                  shape = ObservationMethod,
                  fill = ObservationMethod),
              color = "black",
              alpha = 0.5) +
   scale_shape_manual(values = c(21, 22)) +
   scale_fill_manual(values = c("tomato", "yellow")) +
   coord_cartesian(ylim = c(0, 0.7))+
   annotation_raster(antImage, ymin = .6, ymax = .7, xmin = 55, xmax = 85)
 
 
 
 ggarrange(prop.CatForPlot1, prop.SpiForPlot1, prop.BetForPlot1,
           prop.BugForPlot1, prop.HopForPlot1, prop.AntForPlot1, 
           ncol=3, nrow=2, common.legend = TRUE, legend="bottom")
 
 
 
# -------------------------  Fagus grandifolia------------------------
goodDataFAGR = fullDataset %>%
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
            ant = ifelse(sum(Group == 'ant', na.rm = TRUE) > 0, 1, 0))


datasetFAGR = left_join(goodDataFAGR, sites, by = 'Name')

minSurveysFAGR = 10

siteSummaryFAGR = datasetFAGR %>%
  group_by(Name, Region, Longitude, Latitude, dev, forest) %>%
  summarize(nSurvs = n_distinct(ID),
            nSurvsBS = n_distinct(ID[ObservationMethod == 'Beat sheet']),
            nSurvsVis = n_distinct(ID[ObservationMethod == 'Visual']),
            propCat = n_distinct(ID[caterpillar == 1])/nSurvs,
            propBeet = n_distinct(ID[beetle == 1])/nSurvs,
            propTruebug = n_distinct(ID[truebug == 1])/nSurvs,
            propspider = n_distinct(ID[spider == 1])/nSurvs,
            propHopper = n_distinct(ID[hopper == 1])/nSurvs,
            propAnt = n_distinct(ID[ant == 1])/nSurvs) %>%
  filter(nSurvs >= minSurveysFAGR)

FAGRdata = datasetFAGR %>%
  filter(Name %in% siteSummaryFAGR$Name)



# GLMs
cat.Dev.Lat.FAGR = glm(caterpillar ~ dev + Latitude + dev*Latitude + ObservationMethod, 
                       data = FAGRdata, family = "binomial")

spi.Dev.Lat.FAGR = glm(spider ~ dev + Latitude + dev*Latitude + ObservationMethod, 
                       data = FAGRdata, family = "binomial")

beet.Dev.Lat.FAGR = glm(beetle ~ dev + Latitude + dev*Latitude + ObservationMethod, 
                        data = FAGRdata, family = "binomial")

hop.Dev.Lat.FAGR = glm(hopper ~ dev + Latitude + dev*Latitude + ObservationMethod, 
                       data = FAGRdata, family = "binomial")

bug.Dev.Lat.FAGR = glm(truebug ~ dev + Latitude + dev*Latitude + ObservationMethod, 
                       data = FAGRdata, family = "binomial")

ant.Dev.Lat.FAGR = glm(ant ~ dev + Latitude + dev*Latitude + ObservationMethod, 
                       data = FAGRdata, family = "binomial")

 

####################3

model_performance(cat.Dev.Lat.FAGR)
xx <-simulate_residuals(cat.Dev.Lat.FAGR)

residuals(xx)
hist(xx)
 
######################
# Forest cover models
cat.For.Lat.FAGR = glm(caterpillar ~ forest + Latitude + forest*Latitude + ObservationMethod, 
                       data = FAGRdata, family = "binomial")

spi.For.Lat.FAGR = glm(spider ~ forest + Latitude + forest*Latitude + ObservationMethod, 
                       data = FAGRdata, family = "binomial")

beet.For.Lat.FAGR = glm(beetle ~ forest + Latitude + forest*Latitude + ObservationMethod, 
                        data = FAGRdata, family = "binomial")

hop.For.Lat.FAGR = glm(hopper ~ forest + Latitude + forest*Latitude + ObservationMethod, 
                       data = FAGRdata, family = "binomial")

bug.For.Lat.FAGR = glm(truebug ~ forest + Latitude + forest*Latitude + ObservationMethod, 
                       data = FAGRdata, family = "binomial")

ant.For.Lat.FAGR = glm(ant ~ forest + Latitude + forest*Latitude + ObservationMethod, 
                       data = FAGRdata, family = "binomial")



# GLM output

devOutputFAGR = data.frame(rbind(summary(cat.Dev.Lat.FAGR)$coefficients, 
                                 summary(spi.Dev.Lat.FAGR)$coefficients, 
                                 summary(beet.Dev.Lat.FAGR)$coefficients,
                                 summary(hop.Dev.Lat.FAGR)$coefficients, 
                                 summary(bug.Dev.Lat.FAGR)$coefficients,
                                 summary(ant.Dev.Lat.FAGR)$coefficients))
devOutputFAGR$term = rep(c('Intercept', 'dev', 'Latitude', 'dev*Latitude', 'Method'), times = 6)
devOutputFAGR$Group = rep(c('caterpillar', 'spider', 'beetle', 'leafhopper', 'truebugs', 'ant'), each = 5)


forOutputFAGR = data.frame(rbind(summary(cat.For.Lat.FAGR)$coefficients, 
                                 summary(spi.For.Lat.FAGR)$coefficients, 
                                 summary(beet.For.Lat.FAGR)$coefficients,
                                 summary(hop.For.Lat.FAGR)$coefficients, 
                                 summary(bug.For.Lat.FAGR)$coefficients,
                                 summary(ant.For.Lat.FAGR)$coefficients))
forOutputFAGR$term = rep(c('Intercept', 'forest', 'Latitude', 'forest*Latitude', 'Method'), times = 6)
forOutputFAGR$Group = rep(c('caterpillar', 'spider', 'beetle', 'leafhopper', 'truebugs', 'ant'), each = 5)


# Interaction plots
catDevPlotFAGR = interact_plot(cat.Dev.Lat.FAGR, pred = dev, modx = Latitude,
                               y.label = "Prop. of surveys with caterpillars",
                               x.lab = "% developed cover", cex.lab = 2, vary.lty = FALSE,
                               colors = c('darkblue', 'blue', 'powderblue'), line.thickness = 2) +
  annotation_raster(catImage, ymin = .025, ymax = .04, xmin = 0, xmax = 40)

beetDevPlotFAGR = interact_plot(beet.Dev.Lat.FAGR, pred = dev, modx = Latitude,
                                y.label = "Prop. of surveys with beetles",
                                x.lab = "% developed cover", cex.lab = 2, vary.lty = FALSE,
                                colors = c('darkblue', 'blue', 'powderblue'), line.thickness = 2) +
  annotation_raster(beetleImage, ymin = .18, ymax = .23, xmin = 10, xmax = 40) 

bugDevPlotFAGR = interact_plot(bug.Dev.Lat.FAGR, pred = dev, modx = Latitude,
                               y.label = "Prop. of surveys with true bugs",
                               x.lab = "% developed cover", cex.lab = 2, vary.lty = FALSE,
                               colors = c('darkblue', 'blue', 'powderblue'), line.thickness = 2) +
  annotation_raster(truebugImage, ymin = .04, ymax = .057, xmin = 40, xmax = 80)

spiDevPlotFAGR = interact_plot(spi.Dev.Lat.FAGR, pred = dev, modx = Latitude,
                               y.label = "Prop. of surveys with spiders",
                               x.lab = "% developed cover", cex.lab = 2, vary.lty = FALSE,
                               colors = c('darkblue', 'blue', 'powderblue'), line.thickness = 2) +
  annotation_raster(spiderImage, ymin = .25, ymax = .32, xmin = 60, xmax = 100)

hopDevPlotFAGR = interact_plot(hop.Dev.Lat.FAGR, pred = dev, modx = Latitude,
                               y.label = "Prop. of surveys with hoppers",
                               x.lab = "% developed cover", cex.lab = 2, vary.lty = FALSE,
                               colors = c('darkblue', 'blue', 'powderblue'), line.thickness = 2) +
  annotation_raster(hopperImage, ymin = .05, ymax = .09, xmin = 60, xmax = 100)

antDevPlotFAGR = interact_plot(ant.Dev.Lat.FAGR, pred = dev, modx = Latitude,
                               y.label = "Prop. of surveys with ants",
                               x.lab = "% developed cover", cex.lab = 2, vary.lty = FALSE,
                               colors = c('darkblue', 'blue', 'powderblue'), line.thickness = 2) +
  annotation_raster(antImage, ymin = .075, ymax = .1, xmin = 5, xmax = 40)

ggarrange(catDevPlotFAGR, spiDevPlotFAGR, beetDevPlotFAGR, bugDevPlotFAGR, hopDevPlotFAGR, antDevPlotFAGR, 
          ncol=3, nrow=2, common.legend = TRUE, legend="bottom")






catForPlotFAGR = interact_plot(cat.For.Lat.FAGR, pred = forest, modx = Latitude,
                               y.label = "Prop. of surveys with caterpillars",
                               x.lab = "% Forest cover", cex.lab = 2, vary.lty = FALSE,
                               colors = c('darkgreen', 'green', 'lightgreen'), line.thickness = 2) +
  annotation_raster(catImage, ymin = .075, ymax = .095, xmin = 33, xmax = 67)

beetForPlotFAGR = interact_plot(beet.For.Lat.FAGR, pred = forest, modx = Latitude,
                                y.label = "Prop. of surveys with beetles",
                                x.lab = "% Forest cover", cex.lab = 2, vary.lty = FALSE,
                                colors = c('darkgreen', 'green', 'lightgreen'), line.thickness = 2) +
  annotation_raster(beetleImage, ymin = .16, ymax = .205, xmin = 50, xmax = 85)

bugForPlotFAGR = interact_plot(bug.For.Lat.FAGR, pred = forest, modx = Latitude,
                               y.label = "Prop. of surveys with true bugs",
                               x.lab = "% Forest cover", cex.lab = 2, vary.lty = FALSE,
                               colors = c('darkgreen', 'green', 'lightgreen'), line.thickness = 2) +
  annotation_raster(truebugImage, ymin = .04, ymax = .05, xmin = 10, xmax = 50)

spiForPlotFAGR = interact_plot(spi.For.Lat.FAGR, pred = forest, modx = Latitude,
                               y.label = "Prop. of surveys with spiders",
                               x.lab = "% Forest cover", cex.lab = 2, vary.lty = FALSE,
                               colors = c('darkgreen', 'green', 'lightgreen'), line.thickness = 2) +
  annotation_raster(spiderImage, ymin = .31, ymax = .4, xmin = 0, xmax = 40)

hopForPlotFAGR = interact_plot(hop.For.Lat.FAGR, pred = forest, modx = Latitude,
                               y.label = "Prop. of surveys with hoppers",
                               x.lab = "% Forest cover", cex.lab = 2, vary.lty = FALSE,
                               colors = c('darkgreen', 'green', 'lightgreen'), line.thickness = 2) +
  annotation_raster(hopperImage, ymin = .05, ymax = .1, xmin = 0, xmax = 40)

antForPlotFAGR = interact_plot(ant.For.Lat.FAGR, pred = forest, modx = Latitude,
                               y.label = "Prop. of surveys with ants",
                               x.lab = "% Forest cover", cex.lab = 2, vary.lty = FALSE,
                               colors = c('darkgreen', 'green', 'lightgreen'), line.thickness = 2) +
  annotation_raster(antImage, ymin = .085, ymax = .11, xmin = 5, xmax = 45)

ggarrange(catForPlotFAGR, spiForPlotFAGR, beetForPlotFAGR, bugForPlotFAGR, hopForPlotFAGR, antForPlotFAGR, 
          ncol=3, nrow=2, common.legend = TRUE, legend="bottom")


#############################################################################################

# -------------------------  Acer saccharum ------------------------

goodDataACSA = fullDataset %>%
  filter(julianday %in% julianWindow,
         WetLeaves == 0,
         !Name %in% c('Coweeta - BS', 'Coweeta - BB', 'Coweeta - RK'),
         sciName == "Acer saccharum") %>%
  group_by(Name, ID, ObservationMethod) %>%
  summarize(caterpillar = ifelse(sum(Group == 'caterpillar', na.rm = TRUE) > 0, 1, 0),
            spider = ifelse(sum(Group == 'spider', na.rm = TRUE) > 0, 1, 0),
            beetle = ifelse(sum(Group == 'beetle', na.rm = TRUE) > 0, 1, 0),
            truebug = ifelse(sum(Group == 'truebugs', na.rm = TRUE) > 0, 1, 0),
            hopper = ifelse(sum(Group == 'leafhopper', na.rm = TRUE) > 0, 1, 0),
            ant = ifelse(sum(Group == 'ant', na.rm = TRUE) > 0, 1, 0))


datasetACSA = left_join(goodDataACSA, sites, by = 'Name')

minSurveysACSA = 10

siteSummaryACSA = datasetACSA %>%
  group_by(Name, Region, Longitude, Latitude, dev, forest) %>%
  summarize(nSurvs = n_distinct(ID),
            nSurvsBS = n_distinct(ID[ObservationMethod == 'Beat sheet']),
            nSurvsVis = n_distinct(ID[ObservationMethod == 'Visual']),
            propCat = n_distinct(ID[caterpillar == 1])/nSurvs,
            propBeet = n_distinct(ID[beetle == 1])/nSurvs,
            propTruebug = n_distinct(ID[truebug == 1])/nSurvs,
            propspider = n_distinct(ID[spider == 1])/nSurvs,
            propHopper = n_distinct(ID[hopper == 1])/nSurvs,
            propAnt = n_distinct(ID[ant == 1])/nSurvs) %>%
  filter(nSurvs >= minSurveysACSA)

ACSAdata = datasetACSA %>%
  filter(Name %in% siteSummaryACSA$Name)



# GLMs
cat.Dev.Lat.ACSA = glm(caterpillar ~ dev + Latitude + dev*Latitude + ObservationMethod, 
                       data = ACSAdata, family = "binomial")

spi.Dev.Lat.ACSA = glm(spider ~ dev + Latitude + dev*Latitude + ObservationMethod, 
                       data = ACSAdata, family = "binomial")

beet.Dev.Lat.ACSA = glm(beetle ~ dev + Latitude + dev*Latitude + ObservationMethod, 
                        data = ACSAdata, family = "binomial")

hop.Dev.Lat.ACSA = glm(hopper ~ dev + Latitude + dev*Latitude + ObservationMethod, 
                       data = ACSAdata, family = "binomial")

bug.Dev.Lat.ACSA = glm(truebug ~ dev + Latitude + dev*Latitude + ObservationMethod, 
                       data = ACSAdata, family = "binomial")

ant.Dev.Lat.ACSA = glm(ant ~ dev + Latitude + dev*Latitude + ObservationMethod, 
                       data = ACSAdata, family = "binomial")



# Forest cover models
cat.For.Lat.ACSA = glm(caterpillar ~ forest + Latitude + forest*Latitude + ObservationMethod, 
                       data = ACSAdata, family = "binomial")

spi.For.Lat.ACSA = glm(spider ~ forest + Latitude + forest*Latitude + ObservationMethod, 
                       data = ACSAdata, family = "binomial")

beet.For.Lat.ACSA = glm(beetle ~ forest + Latitude + forest*Latitude + ObservationMethod, 
                        data = ACSAdata, family = "binomial")

hop.For.Lat.ACSA = glm(hopper ~ forest + Latitude + forest*Latitude + ObservationMethod, 
                       data = ACSAdata, family = "binomial")

bug.For.Lat.ACSA = glm(truebug ~ forest + Latitude + forest*Latitude + ObservationMethod, 
                       data = ACSAdata, family = "binomial")

ant.For.Lat.ACSA = glm(ant ~ forest + Latitude + forest*Latitude + ObservationMethod, 
                       data = ACSAdata, family = "binomial")



# GLM output

devOutputACSA = data.frame(rbind(summary(cat.Dev.Lat.ACSA)$coefficients, 
                                 summary(spi.Dev.Lat.ACSA)$coefficients, 
                                 summary(beet.Dev.Lat.ACSA)$coefficients,
                                 summary(hop.Dev.Lat.ACSA)$coefficients, 
                                 summary(bug.Dev.Lat.ACSA)$coefficients,
                                 summary(ant.Dev.Lat.ACSA)$coefficients))
devOutputACSA$term = rep(c('Intercept', 'dev', 'Latitude', 'dev*Latitude', 'Method'), times = 6)
devOutputACSA$Group = rep(c('caterpillar', 'spider', 'beetle', 'leafhopper', 'truebugs', 'ant'), each = 5)


forOutputACSA = data.frame(rbind(summary(cat.For.Lat.ACSA)$coefficients, 
                                 summary(spi.For.Lat.ACSA)$coefficients, 
                                 summary(beet.For.Lat.ACSA)$coefficients,
                                 summary(hop.For.Lat.ACSA)$coefficients, 
                                 summary(bug.For.Lat.ACSA)$coefficients,
                                 summary(ant.For.Lat.ACSA)$coefficients))
forOutputACSA$term = rep(c('Intercept', 'forest', 'Latitude', 'forest*Latitude', 'Method'), times = 6)
forOutputACSA$Group = rep(c('caterpillar', 'spider', 'beetle', 'leafhopper', 'truebugs', 'ant'), each = 5)


# Interaction plots
catDevPlotACSA = interact_plot(cat.Dev.Lat.ACSA, pred = dev, modx = Latitude,
                               y.label = "Prop. of surveys with caterpillars",
                               x.lab = "% developed cover", cex.lab = 2, vary.lty = FALSE,
                               colors = c('darkblue', 'blue', 'powderblue'), line.thickness = 2) +
  annotation_raster(catImage, ymin = .025, ymax = .04, xmin = 0, xmax = 40)

beetDevPlotACSA = interact_plot(beet.Dev.Lat.ACSA, pred = dev, modx = Latitude,
                                y.label = "Prop. of surveys with beetles",
                                x.lab = "% developed cover", cex.lab = 2, vary.lty = FALSE,
                                colors = c('darkblue', 'blue', 'powderblue'), line.thickness = 2) +
  annotation_raster(beetleImage, ymin = .20, ymax = .24, xmin = 30, xmax = 60) 

bugDevPlotACSA = interact_plot(bug.Dev.Lat.ACSA, pred = dev, modx = Latitude,
                               y.label = "Prop. of surveys with true bugs",
                               x.lab = "% developed cover", cex.lab = 2, vary.lty = FALSE,
                               colors = c('darkblue', 'blue', 'powderblue'), line.thickness = 2) +
  annotation_raster(truebugImage, ymin = .0405, ymax = .045, xmin = 10, xmax = 38)

spiDevPlotACSA = interact_plot(spi.Dev.Lat.ACSA, pred = dev, modx = Latitude,
                               y.label = "Prop. of surveys with spiders",
                               x.lab = "% developed cover", cex.lab = 2, vary.lty = FALSE,
                               colors = c('darkblue', 'blue', 'powderblue'), line.thickness = 2) +
  annotation_raster(spiderImage, ymin = .25, ymax = .32, xmin = 60, xmax = 100)

hopDevPlotACSA = interact_plot(hop.Dev.Lat.ACSA, pred = dev, modx = Latitude,
                               y.label = "Prop. of surveys with hoppers",
                               x.lab = "% developed cover", cex.lab = 2, vary.lty = FALSE,
                               colors = c('darkblue', 'blue', 'powderblue'), line.thickness = 2) +
  annotation_raster(hopperImage, ymin = .06, ymax = .09, xmin = 5, xmax = 50)

antDevPlotACSA = interact_plot(ant.Dev.Lat.ACSA, pred = dev, modx = Latitude,
                               y.label = "Prop. of surveys with ants",
                               x.lab = "% developed cover", cex.lab = 2, vary.lty = FALSE,
                               colors = c('darkblue', 'blue', 'powderblue'), line.thickness = 2) +
  annotation_raster(antImage, ymin = .075, ymax = .1, xmin = 5, xmax = 40)

ggarrange(catDevPlotACSA, spiDevPlotACSA, beetDevPlotACSA, bugDevPlotACSA, hopDevPlotACSA, antDevPlotACSA, 
          ncol=3, nrow=2, common.legend = TRUE, legend="bottom")






catForPlotACSA = interact_plot(cat.For.Lat.ACSA, pred = forest, modx = Latitude,
                               y.label = "Prop. of surveys with caterpillars",
                               x.lab = "% Forest cover", cex.lab = 2, vary.lty = FALSE,
                               colors = c('darkgreen', 'green', 'lightgreen'), line.thickness = 2) +
  annotation_raster(catImage, ymin = .065, ymax = .085, xmin = 20, xmax = 57)

beetForPlotACSA = interact_plot(beet.For.Lat.ACSA, pred = forest, modx = Latitude,
                                y.label = "Prop. of surveys with beetles",
                                x.lab = "% Forest cover", cex.lab = 2, vary.lty = FALSE,
                                colors = c('darkgreen', 'green', 'lightgreen'), line.thickness = 2) +
  annotation_raster(beetleImage, ymin = .12, ymax = .15, xmin = 10, xmax = 45)

bugForPlotACSA = interact_plot(bug.For.Lat.ACSA, pred = forest, modx = Latitude,
                               y.label = "Prop. of surveys with true bugs",
                               x.lab = "% Forest cover", cex.lab = 2, vary.lty = FALSE,
                               colors = c('darkgreen', 'green', 'lightgreen'), line.thickness = 2) +
  annotation_raster(truebugImage, ymin = .03, ymax = .035, xmin = 10, xmax = 50)

spiForPlotACSA = interact_plot(spi.For.Lat.ACSA, pred = forest, modx = Latitude,
                               y.label = "Prop. of surveys with spiders",
                               x.lab = "% Forest cover", cex.lab = 2, vary.lty = FALSE,
                               colors = c('darkgreen', 'green', 'lightgreen'), line.thickness = 2) +
  annotation_raster(spiderImage, ymin = .25, ymax = .32, xmin = 0, xmax = 40)

hopForPlotACSA = interact_plot(hop.For.Lat.ACSA, pred = forest, modx = Latitude,
                               y.label = "Prop. of surveys with hoppers",
                               x.lab = "% Forest cover", cex.lab = 2, vary.lty = FALSE,
                               colors = c('darkgreen', 'green', 'lightgreen'), line.thickness = 2) +
  annotation_raster(hopperImage, ymin = .07, ymax = .1, xmin = 40, xmax = 80)

antForPlotACSA = interact_plot(ant.For.Lat.ACSA, pred = forest, modx = Latitude,
                               y.label = "Prop. of surveys with ants",
                               x.lab = "% Forest cover", cex.lab = 2, vary.lty = FALSE,
                               colors = c('darkgreen', 'green', 'lightgreen'), line.thickness = 2) +
  annotation_raster(antImage, ymin = .18, ymax = .24, xmin = 70, xmax = 99)

ggarrange(catForPlotACSA, spiForPlotACSA, beetForPlotACSA, bugForPlotACSA, hopForPlotACSA, antForPlotACSA, 
          ncol=3, nrow=2, common.legend = TRUE, legend="bottom")




##########################################################################################


#--------------------------------- Acer negundo ------------------------------------
#- ------------------------Acer negundo ---------------------------------------------

goodDataACNE = fullDataset %>%
  filter(julianday %in% julianWindow,
         WetLeaves == 0,
         !Name %in% c('Coweeta - BS', 'Coweeta - BB', 'Coweeta - RK'),
         sciName == "Acer negundo") %>%
  group_by(Name, ID, ObservationMethod) %>%
  summarize(caterpillar = ifelse(sum(Group == 'caterpillar', na.rm = TRUE) > 0, 1, 0),
            spider = ifelse(sum(Group == 'spider', na.rm = TRUE) > 0, 1, 0),
            beetle = ifelse(sum(Group == 'beetle', na.rm = TRUE) > 0, 1, 0),
            truebug = ifelse(sum(Group == 'truebugs', na.rm = TRUE) > 0, 1, 0),
            hopper = ifelse(sum(Group == 'leafhopper', na.rm = TRUE) > 0, 1, 0),
            ant = ifelse(sum(Group == 'ant', na.rm = TRUE) > 0, 1, 0))


datasetACNE = left_join(goodDataACNE, sites, by = 'Name')

minSurveysACNE = 10

siteSummaryACNE = datasetACNE %>%
  group_by(Name, Region, Longitude, Latitude, dev, forest) %>%
  summarize(nSurvs = n_distinct(ID),
            nSurvsBS = n_distinct(ID[ObservationMethod == 'Beat sheet']),
            nSurvsVis = n_distinct(ID[ObservationMethod == 'Visual']),
            propCat = n_distinct(ID[caterpillar == 1])/nSurvs,
            propBeet = n_distinct(ID[beetle == 1])/nSurvs,
            propTruebug = n_distinct(ID[truebug == 1])/nSurvs,
            propspider = n_distinct(ID[spider == 1])/nSurvs,
            propHopper = n_distinct(ID[hopper == 1])/nSurvs,
            propAnt = n_distinct(ID[ant == 1])/nSurvs) %>%
  filter(nSurvs >= minSurveysACNE)

ACNEdata = datasetACNE %>%
  filter(Name %in% siteSummaryACNE$Name)



# GLMs
cat.Dev.Lat.ACNE = glm(caterpillar ~ dev + Latitude + dev*Latitude + ObservationMethod, 
                       data = ACNEdata, family = "binomial")

spi.Dev.Lat.ACNE = glm(spider ~ dev + Latitude + dev*Latitude + ObservationMethod, 
                       data = ACNEdata, family = "binomial")

beet.Dev.Lat.ACNE = glm(beetle ~ dev + Latitude + dev*Latitude + ObservationMethod, 
                        data = ACNEdata, family = "binomial")

hop.Dev.Lat.ACNE = glm(hopper ~ dev + Latitude + dev*Latitude + ObservationMethod, 
                       data = ACNEdata, family = "binomial")

bug.Dev.Lat.ACNE = glm(truebug ~ dev + Latitude + dev*Latitude + ObservationMethod, 
                       data = ACNEdata, family = "binomial")

ant.Dev.Lat.ACNE = glm(ant ~ dev + Latitude + dev*Latitude + ObservationMethod, 
                       data = ACNEdata, family = "binomial")



# Forest cover models
cat.For.Lat.ACNE = glm(caterpillar ~ forest + Latitude + forest*Latitude + ObservationMethod, 
                       data = ACNEdata, family = "binomial")

spi.For.Lat.ACNE = glm(spider ~ forest + Latitude + forest*Latitude + ObservationMethod, 
                       data = ACNEdata, family = "binomial")

beet.For.Lat.ACNE = glm(beetle ~ forest + Latitude + forest*Latitude + ObservationMethod, 
                        data = ACNEdata, family = "binomial")

hop.For.Lat.ACNE = glm(hopper ~ forest + Latitude + forest*Latitude + ObservationMethod, 
                       data = ACNEdata, family = "binomial")

bug.For.Lat.ACNE = glm(truebug ~ forest + Latitude + forest*Latitude + ObservationMethod, 
                       data = ACNEdata, family = "binomial")

ant.For.Lat.ACNE = glm(ant ~ forest + Latitude + forest*Latitude + ObservationMethod, 
                       data = ACNEdata, family = "binomial")



# GLM output

devOutputACNE = data.frame(rbind(summary(cat.Dev.Lat.ACNE)$coefficients, 
                                 summary(spi.Dev.Lat.ACNE)$coefficients, 
                                 summary(beet.Dev.Lat.ACNE)$coefficients,
                                 summary(hop.Dev.Lat.ACNE)$coefficients, 
                                 summary(bug.Dev.Lat.ACNE)$coefficients,
                                 summary(ant.Dev.Lat.ACNE)$coefficients))
devOutputACNE$term = rep(c('Intercept', 'dev', 'Latitude', 'dev*Latitude', 'Method'), times = 6)
devOutputACNE$Group = rep(c('caterpillar', 'spider', 'beetle', 'leafhopper', 'truebugs', 'ant'), each = 5)


forOutputACNE = data.frame(rbind(summary(cat.For.Lat.ACNE)$coefficients, 
                                 summary(spi.For.Lat.ACNE)$coefficients, 
                                 summary(beet.For.Lat.ACNE)$coefficients,
                                 summary(hop.For.Lat.ACNE)$coefficients, 
                                 summary(bug.For.Lat.ACNE)$coefficients,
                                 summary(ant.For.Lat.ACNE)$coefficients))
forOutputACNE$term = rep(c('Intercept', 'forest', 'Latitude', 'forest*Latitude', 'Method'), times = 6)
forOutputACNE$Group = rep(c('caterpillar', 'spider', 'beetle', 'leafhopper', 'truebugs', 'ant'), each = 5)


# Interaction plots
catDevPlotACNE = interact_plot(cat.Dev.Lat.ACNE, pred = dev, modx = Latitude,
                               y.label = "Prop. of surveys with caterpillars",
                               x.lab = "% developed cover", cex.lab = 2, vary.lty = FALSE,
                               colors = c('darkblue', 'blue', 'powderblue'), line.thickness = 2) +
  annotation_raster(catImage, ymin = .35, ymax = .4, xmin = 70, xmax = 99)

beetDevPlotACNE = interact_plot(beet.Dev.Lat.ACNE, pred = dev, modx = Latitude,
                                y.label = "Prop. of surveys with beetles",
                                x.lab = "% developed cover", cex.lab = 2, vary.lty = FALSE,
                                colors = c('darkblue', 'blue', 'powderblue'), line.thickness = 2) +
  annotation_raster(beetleImage, ymin = .4, ymax = .45, xmin = 75, xmax = 100) 

bugDevPlotACNE = interact_plot(bug.Dev.Lat.ACNE, pred = dev, modx = Latitude,
                               y.label = "Prop. of surveys with true bugs",
                               x.lab = "% developed cover", cex.lab = 2, vary.lty = FALSE,
                               colors = c('darkblue', 'blue', 'powderblue'), line.thickness = 2) +
  annotation_raster(truebugImage, ymin = .15, ymax = .19, xmin = 70, xmax = 99)

spiDevPlotACNE = interact_plot(spi.Dev.Lat.ACNE, pred = dev, modx = Latitude,
                               y.label = "Prop. of surveys with spiders",
                               x.lab = "% developed cover", cex.lab = 2, vary.lty = FALSE,
                               colors = c('darkblue', 'blue', 'powderblue'), line.thickness = 2) +
  annotation_raster(spiderImage, ymin = .3, ymax = .36, xmin = 60, xmax = 100)

hopDevPlotACNE = interact_plot(hop.Dev.Lat.ACNE, pred = dev, modx = Latitude,
                               y.label = "Prop. of surveys with hoppers",
                               x.lab = "% developed cover", cex.lab = 2, vary.lty = FALSE,
                               colors = c('darkblue', 'blue', 'powderblue'), line.thickness = 2) +
  annotation_raster(hopperImage, ymin = .4, ymax = .5, xmin = 65, xmax = 99)

antDevPlotACNE = interact_plot(ant.Dev.Lat.ACNE, pred = dev, modx = Latitude,
                               y.label = "Prop. of surveys with ants",
                               x.lab = "% developed cover", cex.lab = 2, vary.lty = FALSE,
                               colors = c('darkblue', 'blue', 'powderblue'), line.thickness = 2) +
  annotation_raster(antImage, ymin = .12, ymax = .15, xmin = 5, xmax = 40)

ggarrange(catDevPlotACNE, spiDevPlotACNE, beetDevPlotACNE, bugDevPlotACNE, hopDevPlotACNE, antDevPlotACNE, 
          ncol=3, nrow=2, common.legend = TRUE, legend="bottom")






catForPlotACNE = interact_plot(cat.For.Lat.ACNE, pred = forest, modx = Latitude,
                               y.label = "Prop. of surveys with caterpillars",
                               x.lab = "% Forest cover", cex.lab = 2, vary.lty = FALSE,
                               colors = c('darkgreen', 'green', 'lightgreen'), line.thickness = 2) +
  annotation_raster(catImage, ymin = .25, ymax = .3, xmin = 20, xmax = 50)

beetForPlotACNE = interact_plot(beet.For.Lat.ACNE, pred = forest, modx = Latitude,
                                y.label = "Prop. of surveys with beetles",
                                x.lab = "% Forest cover", cex.lab = 2, vary.lty = FALSE,
                                colors = c('darkgreen', 'green', 'lightgreen'), line.thickness = 2) +
  annotation_raster(beetleImage, ymin = .35, ymax = .4, xmin = 60, xmax = 80)

bugForPlotACNE = interact_plot(bug.For.Lat.ACNE, pred = forest, modx = Latitude,
                               y.label = "Prop. of surveys with true bugs",
                               x.lab = "% Forest cover", cex.lab = 2, vary.lty = FALSE,
                               colors = c('darkgreen', 'green', 'lightgreen'), line.thickness = 2) +
  annotation_raster(truebugImage, ymin = .09, ymax = .11, xmin = 10, xmax = 50)

spiForPlotACNE = interact_plot(spi.For.Lat.ACNE, pred = forest, modx = Latitude,
                               y.label = "Prop. of surveys with spiders",
                               x.lab = "% Forest cover", cex.lab = 2, vary.lty = FALSE,
                               colors = c('darkgreen', 'green', 'lightgreen'), line.thickness = 2) +
  annotation_raster(spiderImage, ymin = .35, ymax = .42, xmin = 0, xmax = 40)

hopForPlotACNE = interact_plot(hop.For.Lat.ACNE, pred = forest, modx = Latitude,
                               y.label = "Prop. of surveys with hoppers",
                               x.lab = "% Forest cover", cex.lab = 2, vary.lty = FALSE,
                               colors = c('darkgreen', 'green', 'lightgreen'), line.thickness = 2) +
  annotation_raster(hopperImage, ymin = .35, ymax = .43, xmin = 10, xmax = 50)

antForPlotACNE = interact_plot(ant.For.Lat.ACNE, pred = forest, modx = Latitude,
                               y.label = "Prop. of surveys with ants",
                               x.lab = "% Forest cover", cex.lab = 2, vary.lty = FALSE,
                               colors = c('darkgreen', 'green', 'lightgreen'), line.thickness = 2) +
  annotation_raster(antImage, ymin = .34, ymax = .44, xmin = 10, xmax = 40)

ggarrange(catForPlotACNE, spiForPlotACNE, beetForPlotACNE, bugForPlotACNE, hopForPlotACNE, antForPlotACNE, 
          ncol=3, nrow=2, common.legend = TRUE, legend="bottom")








