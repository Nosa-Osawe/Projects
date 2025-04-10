require(vegan)
require(tidyverse)
library(MASS)
require(corrplot)

env <- read.csv("C:\\Users\\DELL\\Documents\\Git in R\\Projects\\Data\\doubsenv.csv", 
                row.names = 1)
env <- env[-8, ]  ## Site number 8 contains no species, so we remove row 8 (site 8) 

spe <- read.csv("C:\\Users\\DELL\\Documents\\Git in R\\Projects\\Data\\doubsspe.csv", 
                row.names = 1)
spe <- spe[-8, ]  # Remove corresponding abiotic data for site 8 (because removed from fish data). 

ab <- table(unlist(spe)) # # Count number of species frequencies in each abundance class
ab   # 50% of our dataset consists of zeros

# Apply Hellinger transformation to correct for the double zero problem
spe.hel <- decostand(spe, method = "hellinger")

# check for collinearity in the env data; make corr matrix

corrplot(cor(env), method = c("color"), type = "lower",
         tl.cex = 0.7, tl.col = "black", addCoef.col='black' )



# Scale and center env. variables
env.z <- decostand(env, method = "standardize")

# Run an RDA


# will remove 'das', which was correlated with many other variables:
env.z <- subset(env.z, select = -das)
# Model the effect of all environmental variables on fish  community composition
spe.rda <- rda(spe.hel ~ ., data = env.z)
summary(spe.rda)

# Forward selection of variables:
fwd.sel <- ordiR2step(rda(spe.hel ~ 1, data = env.z), # lower model limit (simple!)
                      scope = formula(spe.rda), # upper model limit (the "full" model)
                      direction = "forward",
                      R2scope = TRUE, # can't surpass the "full" model's R2
                      pstep = 1000,
                      trace = FALSE) # change to TRUE to see the selection process!

# Check the new model with forward-selected variables
fwd.sel$call


# Write our new model
spe.rda.signif <- rda(spe.hel ~ alt + oxy + dbo, data = env.z)
# check the adjusted R2 (corrected for the number of
# explanatory variables)
RsquareAdj(spe.rda.signif)

#test the significance of each variable
anova.cca(spe.rda.signif, step = 1000, by = "term")
anova.cca(spe.rda.signif, step = 1000, by = "axis")

# Type 1 scaling
ordiplot(spe.rda.signif, scaling = 1, type = "text")
# Type 2 scaling
ordiplot(spe.rda.signif, scaling = 2, type = "text")



## extract % explained by the first 2 axes
perc <- round(100*(summary(spe.rda.signif)$cont$importance[2, 1:2]), 2)

## extract scores - these are coordinates in the RDA space
sc_si <- scores(spe.rda.signif, display="sites", choices=c(1,2), scaling=1)
sc_sp <- scores(spe.rda.signif, display="species", choices=c(1,2), scaling=1)
sc_bp <- scores(spe.rda.signif, display="bp", choices=c(1, 2), scaling=1)

# I can convert to data frame and make ggplot2, right:
sc_si <- as.data.frame(sc_si)
sc_sp <- as.data.frame(sc_sp)
sc_bp <- as.data.frame(sc_bp)

# Syntax for the partial RDA:
spe.partial.rda <- rda(spe.hel ~ pH + dur + pho + nit + amm + oxy + dbo + 
                         # these are the effects we are interested in
                         Condition(alt + pen + deb), 
                       # these are the covariates
                       data = env.z)

summary(spe.partial.rda)


# Extract the model's adjusted R2
RsquareAdj(spe.partial.rda)$adj.r.squared

# Test whether the model is statistically significant
anova.cca(spe.partial.rda, step = 999)

ordiplot(spe.partial.rda, scaling = 2, 
         main = "Doubs River partial RDA - Scaling 2")


# Variance partitioninig for RDA

# Subset environmental data into topography variables and
# chemistry variables
env.topo <- subset(env.z, select = c(alt, pen, deb))
env.chem <- subset(env.z, select = c(pH, dur, pho, nit, amm,
                                     oxy, dbo))


# Partition the variation in fish community composition
spe.part.all <- varpart(spe.hel, env.chem, env.topo)
spe.part.all$part  # access results!

# plot the variation partitioning Venn diagram
plot(spe.part.all,
     Xnames = c("Chem", "Topo"), # name the partitions
     bg = c("seagreen3", "mediumpurple"), alpha = 80, # colour the circles
     digits = 2, # only show 2 digits
     cex = 1.5)

# The shared fraction [b] does not represent an interaction effect of 
# the two explanatory matrices. 


# SIGNIFICANT TESTING
# [a+b] Chemistry without controlling for topography
anova.cca(rda(spe.hel, env.chem))

# [b+c] Topography without controlling for chemistry
anova.cca(rda(spe.hel, env.topo))

# [a] Chemistry alone
anova.cca(rda(spe.hel, env.chem, env.topo))


# [c] Topography alone
anova.cca(rda(spe.hel, env.topo, env.chem))


spa <- read.csv("C:\\Users\\DELL\\Documents\\Git in R\\Projects\\Data\\doubsspa.csv", 
                row.names = 1)

spa$site <- 1:nrow(spa)  # add site numbers
spa <- spa[-8, ]  # remove site 8

# group sites based on latitude
spa <- spa %>%
  mutate(group = case_when(
    y < 82              ~ "1",
    y > 82 & y < 156    ~ "2",
    y > 156             ~ "3"
  ))

# Plot of the latitude
ggplot(data = spa) +
  geom_point(aes(x = x, 
                 y = y, 
                 colour = as.factor(group)), 
             size = 4) +
  labs(color = "Groups", 
       x = "Longitude", 
       y = "Latitude") +
  scale_color_manual(values = c("#3b5896", "#e3548c", "#ffa600")) +
  theme_classic() + # formatting the plot to make it pretty
  theme(axis.title = element_text(size = 18),
        axis.text = element_text(size = 16),
        legend.title = element_text(size = 20),
        legend.text = element_text(size = 18))


# run the LDA grouping sites into latitude groups based on
# env data
LDA <- lda(env, spa$group)
# predict the groupings
lda.plotdf <- data.frame(group = spa$group, lda = predict(LDA)$x)

# Plot the newly reorganised sites according to the LDA
ggplot(lda.plotdf) +
  geom_point(aes(x = lda.LD1, 
                 y = lda.LD2, 
                 col = factor(group)), 
             size = 4) +
  labs(color = "Groups") +
  scale_color_manual(values = c("#3b5896", "#e3548c", "#ffa600")) +
  theme_classic() + # formatting the plot to make it pretty
  theme(axis.title = element_text(size = 18),
        axis.text = element_text(size = 16),
        legend.title = element_text(size = 20),
        legend.text = element_text(size = 18))

# EVALUATING GROUPING ACCURACY
# Classification of the objects based on the LDA
spe.class <- predict(LDA)$class

# Posterior probabilities that the objects belong to those
# groups
spe.post <- predict(LDA)$posterior

# Table of prior vs. predicted classifications
(spe.table <- table(spa$group, spe.class))


# Proportion of corrected classification
diag(prop.table(spe.table, 1))



