# The purpose of this code is to monitor the progress of two types of vegetation over the years 
# in order to determine whether there has been any environmental.


library(terra) # used for spatial data life raster or vector data
library(imageRy) # handling raster images
library(gglot2)# for graphs
library(viridis) # alternative color palettes 
library(patchwork) # for multiple grapsh on a single plot


# setting the working directory 
setwd("C:/SEexam/francia1")

# checking if there are all the needed files in the selected directory

files <- list.files()
for (i in files) 
    {
      print (i)
        if (i == tail(files, n=1))
          {
            break
          }    
    }

# creating a single object whit 4 images, which every one is a specific band, as follows:
# 1) Red
# 2) Blue
# 3) Green
# 4) NIR

images <- c("fb4.tiff", "fb3.tiff", "fb2.tiff", "fb8.tiff")

fb <- vector("list", length(images))

for (j in seq_along(images))
  {
    fb[[j]] <- rast(images[j])
  }
## at this point, fb is a list of data. 

## let's transform it in a spat raster! 
fb_r <- rast(fb)
fb_r
im.plotRGB(fb_r, 1,2,3)
