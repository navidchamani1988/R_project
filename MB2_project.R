### supervised classification : Random Forest, Maximum Likelihood and combining raster brick with NDVI) 
###Source of data: Landsat 8 imagery
###Case study; Marivan, Iran
###Author: Navid Chamani


##Set up workspace
#Libraries
library(sp)
library(rgdal)
library(raster)
library(maptools)
library(RStoolbox)
library(randomForest)
library(GISTools)

##Set working directory
setwd("D:/R_project/landsat8")
##Prepare raster
red <- raster("D:/R_project/landsat8/LC08_L1TP_168035_20170822_20170911_01_T1_B4.TIF")
NIR <- raster("D:/R_project/landsat8/LC08_L1TP_168035_20170822_20170911_01_T1_B5.TIF")
blue <- raster("D:/R_project/landsat8/LC08_L1TP_168035_20170822_20170911_01_T1_B2.TIF")
green <- raster("D:/R_project/landsat8/LC08_L1TP_168035_20170822_20170911_01_T1_B3.TIF")

##loading study area
study_area<- shapefile("D:/R_project/study area/study area.shp")
plot(study_area)

##check coordinate reference system
crs(study_area)
crs(red)
crs(NIR)
crs(blue)
crs(green)

##Crop each band
c <- readOGR(dsn="D:/R_project/study area/study area.shp", layer="study area")
cRed <- crop(red, c)
cNIR <- crop(NIR, c)
cBlue <- crop(blue, c)
cGreen <- crop(green, c)

#Check plot
plot(cRed)
plot(cNIR)
plot(cBlue)
plot(cGreen)

##Stack and write raster
img_stack<- stack(cRed, cNIR, cBlue, cGreen)
img_stack
img_brick<- brick(cRed, cNIR, cBlue, cGreen)
img_brick
writeRaster(img_brick, datatype='FLT4S', filename = 'img', format='GTiff', overwrite='T')

##mask raster
plotRGB(img_brick, r=1, g=4, b=3, stretch='lin')
plot(study_area, add=T, col="red")
Marivan<- mask(img_brick, study_area)
plotRGB(Marivan, r=1, g=4, b=3, stretch='lin')
writeRaster(Marivan, dataType='FLT4S', filename='Marivan', format='GTiff', overwrite='TRUE')

#Check raster
extent(Marivan)
raster(Marivan)
hist(Marivan)
pairs(Marivan)

##load vector data
trainingClasses <- readOGR(dsn="D:/R_project/landsat8/trainingdata/trainingdata.shp", layer="trainingdata")
plot(trainingClasses, add=TRUE, col="red")


#shape file meta data
summary(trainingClasses)
class(trainingClasses)
length(trainingClasses)
crs(trainingClasses)
extent(trainingClasses)
trainingClasses

#shape file's attributes
trainingClasses@data
length(trainingClasses@data)
names(trainingClasses@data)
trainingClasses@data$class
levels(trainingClasses@data$class)

## create a color palette of 6 colors
classPallete<- c("grey", "yellow","red", "brown", "green", "blue")
classPallete

# create a vector of colors 
colors<- c("grey", "yellow", "red", "brown", "green", "blue")[trainingClasses$class]
colors

#class color 
colors2 <- c("grey", "yellow", "red", "purple", "green", "blue")

##plot raster and vector data
plotRGB(Marivan, r=1, g=4, b=3, stretch='lin')
plot(trainingClasses, col= colors, add=TRUE)

##random forest classification
sc<- superClass(Marivan, trainData=trainingClasses, responseCol='class')
x11()
plot(sc$map, col=colors2, main= "Land Cover Classification Map, Marivan 2017, Random Forest")
legend("bottomleft",
       legend=levels(trainingClasses$class),
       fill=colors2)


##Maximum likelihood classification
mlc<- superClass(Marivan, trainData=trainingClasses, responseCol='class', model = 'mlc')
plot(mlc$map, col=colors2, main= "Land Cover Classification Map, Marivan 2017, MaximumLikelihood")
legend("bottomleft",
       legend=levels(trainingClasses$class),
       fill=colors2)


##NDVI
NDVI <- (cNIR-cRed)/(cNIR+cRed)

#Check NDVI
raster(NDVI)
plot(NDVI)

##mask NDVI
NDVI_mask<- mask(NDVI, study_area)
plot(NDVI_mask)

#Write raster
writeRaster(NDVI_mask, datatype='FLT4S', filename = 'NDVI_mask', format='GTiff', overwrite='T')

#Combine NDVI and raster brick
setwd("D:/R_project/landsat8/tif")
list<-list.files(path=".",pattern='.tif$',full.names =TRUE)
NDVI_stack<- stack(list)
NDVI_stack

##plot raster
plotRGB(NDVI_stack, stretch='lin')
writeRaster(NDVI_stack, datatype='FLT4S', 
            filename = 'NDVI_stack',
            format='GTiff', overwrite='T')



##classification
NDVI_sc<-superClass(NDVI_stack, trainData=trainingClasses, responseCol='class')
plot(NDVI_sc$map, col=colors2, main="Land Cover Classification Map, Marivan 2017,NDVI")
legend("bottomleft",legend=levels(trainingClasses$class),
       fill=colors2) 

##validation
#random forest validation
sc_val<- superClass(Marivan, trainData=trainingClasses, responseCol='class', trainPartition =0.7)
sc_val

#maximum likelihood validation
mlc_val<- superClass(Marivan, trainData=trainingClasses, responseCol='class', trainPartition =0.7, model = 'mlc')
mlc_val

#NDVI validation
NDVI_val<- superClass(NDVI_stack, trainData=trainingClasses, responseCol='class', trainPartition =0.7)
NDVI_val






