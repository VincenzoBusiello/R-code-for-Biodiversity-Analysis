library(imageRy)
library(terra)
library(viridis)

im.list()

sent <- im.import("sentinel.png")

pairs(sent)
# to see the correlation amongst all of the bands (of sentinel image) in a plot 

# perform PCA on sent using function im.pca 
# keeps the variability reducing the information

sentpc <- im.pca(sent) 
sentpc
plot(sentpc) 

# there is the variability explained by the principal component in the pc1
# we extract pc1 (more variability = more information)

pc1 <- sentpc[[1]] # 88,59% of variability 
pc1 

pc1sd <- focal(pc1, matrix(1/9, 3, 3), fun=sd) # focal to calculate standard deviation and shows areas with more variability
plot(pc1sd) 

viridisc <- colorRampPalette(viridis(7))(255) # for colorblind people viridis 
plot(pc1, col=viridisc) 


# calculating standard deviation on top of pc1
pc1sd3 <- focal(pc1, matrix(1/9, 3, 3), fun=sd)
plot(pc1sd3, col=viridisc)

pc1sd7 <- focal(pc1, matrix(1/49, 7, 7), fun=sd) # different dimension to calculate sd
plot(pc1sd7, col=viridisc)

par(mfrow=c(2,3))
im.plotRGB(sent, 2, 1, 3)

plot(sd3, col=viridisc)
plot(sd7, col=viridisc)
plot(pc1, col=viridisc)
plot(pc1sd3, col=viridisc)
plot(pc1sd7, col=viridisc)

# stack all the standard deviation layers

sdstack <- c(sd3, sd7, pc1sd3, pc1sd7)
names(sdstack) <- c("sd3", "sd7", "pc1sd3", "pc1sd7")
plot(sdstack, col=viridisc)
