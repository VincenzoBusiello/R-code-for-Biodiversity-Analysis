# The purpose of this code is to monitor the progress of two types of vegetation over the years 
# in order to determine whether there has been any environmental change due to anthropogenic causes 
# and/or climate change. 


library(terra) # used for spatial data life raster or vector data
library(imageRy) # handling raster images
library(gglot2)# for graphs
library(viridis) # alternative color palettes 
library(patchwork) # for multiple grapsh on a single plot


# setting the working directory 
setwd("C:/SEexam/Batt Reef")

# importing images 
br1 <- rast("Breef04.tiff")
br2 <- rast("Breef03.tiff")
br3 <- rast("Breef02.tiff")
br4 <- rast("Breef08.tiff")

# creating a single images whit 4 bands, as follows
# 1) Red
# 2) Blue
# 3) Green
# 4) NearInfraRed - NIR
br <- c(br1, br2, br3, br4)

# printing the true color image
im.plotRGB(br, 1,2,3)
