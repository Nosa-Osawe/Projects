require(vegan)
require(tidyverse)

env <- read.csv("C:\\Users\\DELL\\Documents\\Git in R\\Projects\\Data\\doubsenv.csv", 
                row.names = 1)
env <- env[-8, ]  ## Site number 8 contains no species, so we remove row 8 (site 8) 

spe <- read.csv("C:\\Users\\DELL\\Documents\\Git in R\\Projects\\Data\\doubsspe.csv", 
                row.names = 1)
spe <- spe[-8, ]  # Remove corresponding abiotic data for site 8 (because removed from fish data). 

ab <- table(unlist(spe)) # # Count number of species frequencies in each abundance class
ab   # 50% of our dataset consists of zeros


