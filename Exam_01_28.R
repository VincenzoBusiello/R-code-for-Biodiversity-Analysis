library(terra)
library(imageRy)
library(gglot2)
library(viridis)
library(patchwork)

setwd("C:/SEexam/Batt Reef")

br1 <- rast("Breef04.tiff")
br2 <- rast("Breef03.tiff")
br3 <- rast("Breef02.tiff")
br4 <- rast("Breef08.tiff")

br <- c(br1, br2, br3, br4)

im.plotRGB(br, 1,2,3)

bb <- br[[1]]
gb <- br[[2]]

BGR <- bb/gb

mBGR <- br[[4]] - BGR
pBGR <- br[[4]] + BGR
NBGR <- mBGR/pBGR
