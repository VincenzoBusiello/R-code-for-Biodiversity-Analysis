install.packages("rasterdiv")  
library(rasterdiv) # used for calculating diversity on numerical matrices
library(imageRy)
library(viridis)
library(terra)

im.list()

sent <- im.import("sentinel.png") # importing the image sentinel.png

im.plotRGB(sent, 1,2,3) # visualizing sent object with bads colored in the follow mode:
                        # first band ->   NIR   -> Red
                        # second band ->  RED   -> Green
                        # third band ->   GREEN -> Blue 

im.plotRGB(sent, 3,2,1) # visualizing sent object with bads colored in the follow mode:
                        # first band ->   GREEN -> Red
                        # second band ->  RED   -> Green
                        # third band ->   NIR   -> Blue 

nir <- sent[[1]] # the first sent band saved on object nir
sd3 <- focal(nir, matrix(1/9,3,3), "sd") # the focal() function allows to calculate standard
                                           # deviation of the object used as an argument
                                           # using a moving window of 3x3

sd7 <- focal(nir, matrix(1/49,7,7), "sd") # the focal() function allows to calculate standard
                                           # deviation of the object used as an argument
                                           # using a moving window of 7x7

par(mfrow=c(2,1))
plot(sd3)
plot(sd7)

