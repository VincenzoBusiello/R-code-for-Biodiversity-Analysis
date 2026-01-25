
# trying Water-Adjusted Vegetation Index - WAVI
L = 0.7

rbn_20_dif = rbn_20[[4]] - rbn_20[[3]]
rbn_20_sum = rbn_20[[4]] + rbn_20[[3]]
rbn_20_WAVI = (1+L)*(rbn_20_dif / (rbn_20_sum + L))

rbn_23_dif = rbn_23[[4]] - rbn_23[[3]]
rbn_23_sum = rbn_23[[4]] + rbn_23[[3]]
rbn_23_WAVI = (1+L)*(rbn_23_dif / (rbn_23_sum + L))

rbn_25_dif = rbn_25[[4]] - rbn_25[[3]]
rbn_25_sum = rbn_25[[4]] + rbn_25[[3]]
rbn_25_WAVI = (1+L)*(rbn_25_dif / (rbn_25_sum + L))


par(mfrow=c(3,2))
plot(rbn_20_WAVI, col = viridis(100))
im.plotRGB(rbn_20, 1,2,3)
plot(rbn_23_WAVI, col = viridis(100))
im.plotRGB(rbn_23, 1,2,3)
plot(rbn_25_WAVI, col = viridis(100))
im.plotRGB(rbn_25, 1,2,3)

par(mfrow=c(1,3))
cl_rbn_20_WAVI <- im.classify(rbn_20_WAVI, num_cluster=4)
cl_rbn_23_WAVI <- im.classify(rbn_23_WAVI, num_cluster=4)
cl_rbn_25_WAVI <- im.classify(rbn_25_WAVI, num_cluster=4)
