# Code for remote sensing data handling and analysis
# reflectance: amount of light based on incident light. 
# the radiation we see is narrow, from blue to red, the visible, there are additional lenghts we can use like infra-red or UV

install.packages("devtools") 
install.packages("terra")

# installing the imageRy package

install_github("ducciorocchini/imageRy")
library(imageRy)
library(terra)

im.list() #report of all the data in the function

# data in the package, provided for sentinels data of the dolomites
# sentinel datas are coming from programm like Copernicus that provides data from all planet 
# bands are how to see the lengths in certain wave -> Sentinel-2 bands name of the satellite
# among the data we have sentinel.dolomites.b2 (b2 = band 2 -> the blue -> we're working at wavelenght 490 nm)

b2 <- im.import("sentinel.dolomites.b2.tif") #importing the data outside the package 

cl <- colorRampPalette(c("black","grey","light grey")) (100) 
plot (b2, col = cl) 

# passive sensor: sunlight passes the atmosphere, passes an object and comes back to the satellite
# Incident radiant flux = energy coming from an object
# part of this radiation is reflect and it is reflected radiant flux
# the reflectance is ranging from 0 to 1

# exercise: import b3 (green) and plot it with the previous palette

b3 <- im.import("sentinel.dolomites.b3.tif")
plot (b3, col = cl) 

# now we use band 4 -> 665 nm -> red wavelength

b4 <- im.import("sentinel.dolomites.b4.tif")
plot(b4, col=cl)

# we do the same for band 8 -> 842 nm -> visible and near infrared (VNIR)

# let's import the NIR (near infrared) band

b8 <- im.import("sentinel.dolomites.b8.tif")
plot(b8, col=cl)

# we can plot in a multiframe all the images together

par(mfrow=c(2,2))
plot(b2, col=cl)
plot(b3, col=cl)
plot(b4, col=cl)
plot(b8, col=cl)

# now we can stack them in the same image.

plot(b2, col=cl)
plot(b3, col=cl)
plot(b4, col=cl)
plot(b8, col=cl)
sentstack <- c(b2,b3,b4,b8)
plot(sentstack, col=cl) # in the plot but they are overlapped

dev.off()
# now we want to plot one layer 

plot(sentstack[[1]], col=cl) # 1 because the first band we put was the number 2 (b2)
plot(sentstack[[4]], col=cl) # 4 is the b8

# multiframe with different color palette

par(mfrow=c(2,2))
clb <- colorRampPalette(c("darkblue","blue","light blue")) (100)
plot(b2, col = clb)

# exercise: apply the same concept to the green band with band 3 

clg <- colorRampPalette(c("darkgreen","green","light green")) (100)
plot(b3, col=clg)

# we do the same with b4 with red

clr <- colorRampPalette(c("dark red", "red", "pink")) (100)
plot(b4, col=clr)

# plotting the NIR band (b8)

cln <- colorRampPalette(c("brown", "orange", "yellow")) (100)
plot(b8, col= cln)

# there is the possibility to mount all bands together to form new colours
# RGB = red, green and blue -> scheme that every single device uses to show colours
# this will lead to "natural color imagery" = how our eyes see landscapes

im.plotRGB(sentstack, r=3, g=2, b=1) # sentstack is the image we made before, also red = 3 = b4 / green = 2 = b3 / blue = 1 = b2.

# now we want to use near infrared. RGB has 3 components but we have 4 bands,facciamo quindi scorrere i numeri così otteniamo il "false colors"

im.plotRGB(sentstack, r = 4, g = 3, b = 2) # false color image -> all vegetation becomes red because it's reflecting

day2

library(terra)
terra 1.7.83
library(imageRy)
im.list()

# recall b2, b3, b4, b8

b2 <- im.import("sentinel.dolomites.b2.tif")
b3 <- im.import("sentinel.dolomites.b3.tif")
b4 <- im.import("sentinel.dolomites.b4.tif")
b8 <- im.import("sentinel.dolomites.b8.tif")
sentstack <- c(b2,b3,b4,b8)
plot(sentstack)

# why we want to stack? vegetation is now represented by infrared beause vegetation reflects a lot in NIR
# it's common to put NIR on top of red band of RGB

im.plotRGB(sentstack, r=3, g=2, b=1) # so to add addittional information that our eyes cannot see 

im.plotRGB(sentstack, r=3, g=4, b=2) # let's try to put NIR on top of green band obtaining another false colour image 

# a NIR on top of blue band

im.plotRGB(sentstack, r=3, g=2, b=4)

examples: 
im.plotRGB(sentstack, r=3, g=2, b=1) # natural color image
im.plotRGB(sentstack, r=4, g=3, b=2) # false color image
im.plotRGB(sentstack, r=3, g=4, b=2) # false color image
im.plotRGB(sentstack, r=3, g=2, b=4) # false color image
