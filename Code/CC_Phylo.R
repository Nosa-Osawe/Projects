library(pheatmap)
library(V.PhyloMaker)
library(ape)
library(tidyverse)
library(readxl)
library(vegan)
library(phytools)


# This is a list of plant with the most branch surveys as at June 21, 2025
cc_species <- data.frame(
  species = c("Fagus_grandifolia",
              "Liquidambar_styraciflua",
              "Acer_saccharum",
              "Acer_rubrum",
              "Acer_negundo",
              "Diospyros_virginiana",
              "Carpinus_caroliniana",
              "Prunus_serotina",
              "Quercus_alba",
              "Quercus_rubra",
              NA,
              "Nyssa_sylvatica",
              "Lindera_benzoin",
              "Rhododendron_maximum",
              "Acer_leucoderme",
              "Hamamelis_virginiana",
              "Fraxinus_americana",
              "Cornus_florida",
              "Carya_cordiformis",
              "Oxydendrum_arboreum",
              "Betula_papyrifera",
              "Ostrya_virginiana",
              NA,
              "Cercis_canadensis"),
  genus = c("Fagus", "Liquidambar", "Acer", "Acer", "Acer", 
            "Diospyros", "Carpinus", "Prunus", "Quercus", "Quercus",
            "Lindera", "Nyssa", "Lindera", "Rhododendron", "Acer",
            "Hamamelis", "Fraxinus", "Cornus", "Carya", "Oxydendrum",
            "Betula", "Ostrya", "Prunus", "Cercis"),
  family = c("Fagaceae", "Altingiaceae", "Sapindaceae", "Sapindaceae", "Sapindaceae",
             "Ebenaceae", "Betulaceae", "Rosaceae", "Fagaceae", "Fagaceae",
             "Lauraceae", "Nyssaceae", "Lauraceae", "Ericaceae", "Sapindaceae",
             "Hamamelidaceae", "Oleaceae", "Cornaceae", "Juglandaceae", "Ericaceae",
             "Betulaceae", "Betulaceae", "Rosaceae", "Fabaceae")
)

# view(cc_species)



cc_tree <-  phylo.maker(cc_species, output.tree = TRUE, output.sp.list = TRUE)

plot(cc_tree$scenario.3, show.tip.label = TRUE)




