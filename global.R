#app.R ####
library(shiny)
library(shinyjs)
library(shinyWidgets)
#ui.R ####
library(plotly)

#server.R ####
#library(raster)
library(tidyverse)#dplyr...
library(ape) #ancestral reconstruciton, tree import
library(treeio) # tree visualization
library(castor) #ancestral reconstruction function asr_max_parsimony
library(broom) #function glance
library(ggfortify) # tree visualization
library(reactlog) #not needed, just for analytics
library(ggtree) # tree visualization
library(reticulate) # use python for treeTime
library(corrplot)#correlation plot
library(GGally) #some more correlation plots
library(memoise) #caching plots to speed up the performance

#Tree.R ####
library(RColorBrewer) #tree visualization

