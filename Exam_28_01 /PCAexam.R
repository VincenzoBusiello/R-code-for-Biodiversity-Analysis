

# calculating pca

pca20 <- im.pca(rbn_20)

## pca values: 
### pca1 = 11.54
### pca2 = 3.63
### pca3 = 0.90
### pca4 = 0.50

totpca20 <- sum(11.54,3.63,0.90,0.50)
11.54*100/totpca20 # 69.64 of variability explained by the 1st axis
3.63*100/totpca20 # 21.91 of variability explained by the 2nd axis
0.90*100/totpca20 # 5.43 of variability explained by the 3rd axis
0.50*100/totpca20 # 3.02 of variability explained by the 4th axis

# let's combine PCA1 and PCA2 because they explain 91,5% of the total variability
pcrbn20 <- pca20[[1]] + pca20[[2]]
sd_rbn_20 <- focal(pcrbn20, matrix(1/9,3,3), fun=sd)


### same for 2023 and 2025

pca23 <- im.pca(rbn_23)
totpca23 <- sum(6.84,4.48,1.23,0.70)
6.84*100/totpca23 # 51.62 of variability explained by the 1st axis
4.48*100/totpca23 # 33.81 of variability explained by the 2nd axis
1.23*100/totpca23 # 9.28 of variability explained by the 3rd axis
0.70*100/totpca23 # 5.28 of variability explained by the 4th axis

# let's combine PCA1 and PCA2 because they explain 85.43% of the total variability
pcrbn23 <- pca23[[1]] + pca23[[2]]


pca25 <- im.pca(rbn_25)
totpca25 <- sum(13.35, 5.61, 1.27, 0.66)
13.35*100/totpca25 # 63.91 of variability explained by the 1st axis
5.61*100/totpca25 # 26.85 of variability explained by the 2nd axis
1.27*100/totpca25 # 6.08 of variability explained by the 3rd axis
0.66*100/totpca25 # 3.16 of variability explained by the 4th axis

# let's combine PCA1 and PCA2 because they explain 90.76% of the total variability
pcrbn25 <- pca25[[1]] + pca25[[2]]


par(mfrow=c(1,3))
plot(pcrbn20, col=viridis(100), main="Spatial variation 2020")
plot(pcrbn23, col=viridis(100), main="Spatial variation 2020")
plot(pcrbn25, col=viridis(100), main="Spatial variation 2020")
