# The purpose of this code is to monitor the progress of two types of vegetation over the years 
# in order to determine whether there has been any environmental.


library(terra) # used for spatial data life raster or vector data
library(imageRy) # handling raster images
library(ggplot2)# for graphs
library(viridis) # alternative color palettes 
library(patchwork) # for multiple grapsh on a single plot

### 1st Case Study - Bourgneuf Bay, France - Zostera noltei

# setting the working directory 
setwd("C:/SEexam/bourgneuf")

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



i_20 <- c("bn20_4.tiff", "bn20_3.tiff", "bn20_2.tiff", "bn20_8.tiff")
i_23 <- c("bn23_4.tiff", "bn23_3.tiff", "bn23_2.tiff", "bn23_8.tiff")
i_25 <- c("bn25_4.tiff", "bn25_3.tiff", "bn25_2.tiff", "bn25_8.tiff")


bn_20 <- vector("list", length(i_20))
bn_23 <- vector("list", length(i_23))
bn_25 <- vector("list", length(i_25))



# for loops for the creation of objects with 4 spectral bands, one each year. 

## September 2020
for (j in seq_along(i_20))
  {
    bn_20[[j]] <- rast(i_20[j])
  }
## let's transform it in a spat raster! 
rbn_20 <- rast(bn_20)
rbn_20


## September 2023
for (j in seq_along(i_23))
  {
    bn_23[[j]] <- rast(i_23[j])
  }
rbn_23 <- rast(bn_23)
rbn_23


## September 2025
for (j in seq_along(i_25))
  {
    bn_25[[j]] <- rast(i_25[j])
  }
rbn_25 <- rast(bn_25)
rbn_25

par(mfrow=c(3,2))
# plotting the rbn_20 in true color
im.plotRGB(rbn_20, 1,2,3)
# plotting the rbn_20 whit NIR band in red
im.plotRGB(rbn_20, 4,2,3)

# plotting the rbn_23 in true color
im.plotRGB(rbn_23, 1,2,3)
# plotting the rbn_23 whit NIR band in red
im.plotRGB(rbn_23, 4,2,3)

# plotting the rbn_25 in true color
im.plotRGB(rbn_25, 1,2,3)
# plotting the rbn_25 whit NIR band in red
im.plotRGB(rbn_25, 4,2,3)

dev.off()

# calculating NDAVI bn20
rbn_20_dif = rbn_20[[4]] - rbn_20[[3]]
rbn_20_sum = rbn_20[[4]] + rbn_20[[3]]
rbn_20_NDAVI = rbn_20_dif / rbn_20_sum

# calculating NDAVI bn23
rbn_23_dif = rbn_23[[4]] - rbn_23[[3]]
rbn_23_sum = rbn_23[[4]] + rbn_23[[3]]
rbn_23_NDAVI = rbn_23_dif / rbn_23_sum

# calculating NDAVI bn25
rbn_25_dif = rbn_25[[4]] - rbn_25[[3]]
rbn_25_sum = rbn_25[[4]] + rbn_25[[3]]
rbn_25_NDAVI = rbn_25_dif / rbn_25_sum

## visualising each true color image next to NDAVI image (standard and viridis palette)
par(mfrow=c(3,3))
im.plotRGB(rbn_20, 1,2,3)
plot(rbn_20_NDAVI)
plot(rbn_20_NDAVI, col=viridis(100))
im.plotRGB(rbn_23, 1,2,3)
plot(rbn_23_NDAVI)
plot(rbn_23_NDAVI, col=viridis(100))
im.plotRGB(rbn_25, 1,2,3)
plot(rbn_25_NDAVI)
plot(rbn_25_NDAVI, col=viridis(100))

# classification
par(mfrow=c(2,3))
cl_rbn_20_NDAVI <- im.classify(rbn_20_NDAVI, num_cluster=3)
cl_rbn_23_NDAVI <- im.classify(rbn_23_NDAVI, num_cluster=3)
cl_rbn_25_NDAVI <- im.classify(rbn_25_NDAVI, num_cluster=3)
plot(rbn_20_NDAVI, col=viridis(100))
plot(rbn_23_NDAVI, col=viridis(100))
plot(rbn_25_NDAVI, col=viridis(100))


#percentages of classes found in 2020
t_bn20 <- ncell(cl_rbn_20_NDAVI)
f_bn20 <- freq(cl_rbn_20_NDAVI)
p_bn20 = f_bn20 / t_bn20
perc_bn20 = p_bn20*100
perc_bn20

#percentages of classes found in 2020
t_bn23 <- ncell(cl_rbn_23_NDAVI)
f_bn23 <- freq(cl_rbn_23_NDAVI)
p_bn23 = f_bn23 / t_bn23
perc_bn23 = p_bn23*100
perc_bn23

#percentages of classes found in 2020
t_bn25 <- ncell(cl_rbn_25_NDAVI)
f_bn25 <- freq(cl_rbn_25_NDAVI)
p_bn25 = f_bn25 / t_bn25
perc_bn25 = p_bn25*100
perc_bn25

            ### 1st classification: 
#1) water                 -->    35.957301 %
#2) aquatic vegetation    -->    61.005755 %
#3) land vegetation       -->     3.036944 %

            ### 2nd classification: 
#1) aquatic vegetation    -->    29.590609 %
#2) water                 -->    65.540797 %
#3) land vegetation       -->     4.868594 %

            ### 3rd classification: 
#1) water                 -->    58.79595 %
#2) land vegetation       -->     4.22179 %
#3) aquatic vegetation    -->    36.98226 %

years <- c("2020", "2023", "2025")
a_veg <- c(61.01, 29.59, 36.98) # percentages of aquatic vegetation
l_veg <- c(3.04, 4.87, 4.22) # percentages of land vegetation
water <- c(35.96, 65.54, 58.80) # percentages of water


tab <- data.frame(years, water, a_veg, l_veg)
View(tab)

pa_veg <- ggplot(tab, aes(x=years, y=a_veg, color=years)) + 
geom_histogram(outlier.colour="red", outlier.shape=8, outlier.size=4)+ 
ylim(c(0,100)) + 
xlab("Year") +
ylab("Aquatic vegetation (%)") +
ggtitle("Aquatic vegetation in the 2020s")

pl_veg <- ggplot(tab, aes(x=years, y=l_veg, color=years)) + 
geom_boxplot(outlier.colour="red", outlier.shape=8, outlier.size=4)+ 
ylim(c(0,100)) + 
xlab("Year") +
ylab("Land vegetation (%)") +
ggtitle("Land vegetation in the 2020s")

pwater <- ggplot(tab, aes(x=years, y=water, color=years)) + 
geom_boxplot(outlier.colour="red", outlier.shape=8, outlier.size=4)+ 
ylim(c(0,100)) + 
xlab("Year") +
ylab("Water extension (%)") +
ggtitle("Water extension in the 2020s")

pa_veg + pl_veg + pwater

pwater <- ggplot(tab, aes(x=years, y=water, color=water)) + 
            geom_bar(stat="identity", fill=years) +
            ylim(c(0,100)) + 
            xlab("Year") +
            ylab("Water extension (%)") +
            ggtitle("Water extension in the 2020s") +
            scale_colour_viridis_d(option = "plasma")

pa_veg <- ggplot(tab, aes(x=years, y=a_veg, color=years)) + geom_bar(stat="identity", fill="white")
pl_veg <- ggplot(tab, aes(x=years, y=l_veg, color=years)) + geom_bar(stat="identity", fill="white")

pwater + pa_veg + pl_veg
