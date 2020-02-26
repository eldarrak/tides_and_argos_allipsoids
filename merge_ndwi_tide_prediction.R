
#With this code you can estimate bathymetry based on NDWI values in your satellite imagery brick
#and tide level prediction. You have to do it separately for Flow and Ebb phases, 
#since the patterns of water flow differ between the phases.
#The objects Ebb and Flow are eventually made
#For tis you will need to load 
#1)cross_gam function by Eldar
#2)your raster brick, 
#3)dataframe with the list of images, with time
#4)dataframe with predicted tides (it should also contain tide phase column)

library(raster)
library(rootSolve)
library(mgcv)

load("tide.RData")
#tide<-Bubaque_2016_2020
tide<-tide[order(tide$Time),]

load("Image_time_upd_27012020.RData")#object "image_time"

#in the image_time dataframe, make a column of tide levels from the tide prediction
image_time$tidelevel<-approx(x=tide$Time, y=tide$z.m., xout=image_time$Imagetime)$y

#Make a column of tide phase (I use the 'quick-and-dirty' approach, but we will come up with a better one, right?)
library(data.table)
tide$tide_next_point<-shift(tide$z.m., n=1, fill=NA, type="lead")
image_time$tide_next_point<-approx(x=tide$Time, y=tide$tide_next_point, xout=image_time$Imagetime)$y

# image_time$tide_phase<-NA
# image_time$tide_phase[image_time$tide_next_point-image_time$tidelevel>0]<-"flow"
# image_time$tide_phase[image_time$tide_next_point-image_time$tidelevel<0]<-"ebbb"
# image_time$tide_phase[image_time$tide_next_point-image_time$tidelevel==0]<-"extremum"

#Because cross_gam does not use extremums, I assign extremums to ebbb 
image_time$tide_phase<-ifelse(image_time$tide_next_point>image_time$tidelevel,"flow","ebbb")

#remove NAs
image_time<-image_time[!is.na(image_time$tidelevel),]
#define maximal tide level for the data, and recalculate tide levels into meters to maximum
Max_tide<-max(image_time$tidelevel)
image_time$meters_to_max<-Max_tide-image_time$tidelevel

#give tide levels at flow negative values
image_time$meters_to_max[image_time$tide_phase=='flow']<- -image_time$meters_to_max[image_time$tide_phase=='flow']

#load your raster brick
brk<-brick('brk_29_01_2020.grd')

#this is how to calculate Ebb part. Repeat the same for Flow
Ebb<-calc(brk, fun=function(x) cross_gam_wrapper(x, image_time$meters_to_max, mode=c('ebbb')))
#writeRaster(Ebb1,filename="ebbb1.grd",overwrite=TRUE)

Res1<-Ebb1
#remove pixels that are never (water) or always (land) exposed 
Res1[Res1>=99]<-NA
Res1[Res1<=-99]<-NA
#par(mfrow=c(1,2))
image(Res1, col=topo.colors(128))

hist(Res1)
