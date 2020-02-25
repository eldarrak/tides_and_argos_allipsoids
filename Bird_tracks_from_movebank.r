
#install.packages('move','RgoogleMaps','maptools','data.table','RColorBrewer')

#You will need to load a tidal prediction
load("tide.RData")# object 'Bubaque_2016_2020'

library(RgoogleMaps)
  Sys.setenv(GOOGLE_MAPS_API_KEY = "AIzaSyAVRQ1V7yrzUixLGj5s0EPD2YgY9opQuvM")

library('move')

Login<-movebankLogin("XXXXX","XXXXX")

#Download one individual track using animal name:
# bird <- getMovebankData(study="Shorebirds_East_Atlantic_Flyway_Theunis_Piersma", animalName="bird_GB2018_66219", login=Login)
#OR
#Download ALL BIRDS from the study (in this case, not only bar-tailed godwits)
#here I download all the tracks starting from 01.01.2018
#removeDuplicatedTimestamps is now set to TRUE, but maybe we have to disuss what would be the best way to deal with those duplicates
bird<- getMovebankData(study="Shorebirds_East_Atlantic_Flyway_Theunis_Piersma", login=Login,removeDuplicatedTimestamps=TRUE,timestamp_start="20180101000000000")
bird<-subset(bird,argos_lc!=0 & argos_lc!='A'&  argos_lc!='B' &  argos_lc!='Z' )
#OR just remove severe outliers
bird<-bird[bird$argos_semi_major<=30000,]

#Append TIDES to the bird tracks
tide<-Bubaque_2016_2020
bird$tide<-approx(x=tide$Time,y=tide$z.m.,xout=bird$timestamp)$y

#Assign DAY and NIGHT using sunset and sunrise times for the location 
#maybe we should better use dawn and dask times, which is also possible with this package
#but it did not work for me, so need to investigate why

library(maptools)

#location Bijagos
Point<-cbind(lon=-15.87, lat=11.2)
gb2<-data.frame(jdate=as.numeric(format(timeseq, format="%j")),sunrise=sunriset(Point, timeseq, direction="sunrise",POSIXct.out=TRUE),sunset=sunriset(Point, timeseq, direction="sunset",POSIXct.out=TRUE))
bird$sunrise<-sunriset(Point, bird$timestamp, direction="sunrise",POSIXct.out=TRUE)$time
bird$sunset<-sunriset(Point, bird$timestamp, direction="sunset",POSIXct.out=TRUE)$time
bird$daytime<-ifelse((bird$timestamp >bird$sunrise & bird$timestamp <bird$sunset),"day","night")

#EBB and VLOED
#here I use a criterion which does not work very well
#so the tide_phase can be derived directly from the dataframe with tides when the issue will have been solved
library(data.table)
tide$tide_next_point<-shift(tide$z.m., n=1, fill=NA, type="lead")
bird$tide_next_point<-approx(x=tide$Time, y=tide$tide_next_point, xout=bird$timestamp)$y
head(bird)
bird$tide_phase<-NA
bird$tide_phase[bird$tide_next_point-bird$tide>0]<-"vloed"
bird$tide_phase[bird$tide_next_point-bird$tide<0]<-"eb"
bird$tide_phase[bird$tide_next_point-bird$tide==0]<-"extremum"


#These are arbitrarily assigned TIDE LEVELS for plotting in colours
#Just to see the pattern
#You can use "Tide levels for the animation" from the bottom of this script
#There, tidal curve is broken in 12-minute intervals (which is more practical for animation but probably not the best solution for analysis of movement patterns)
#It is also something to reflect about: what would be the criteria for an 
#optimal tide-level assignment

Breaks<-round(seq(-2.4, 2.5, by=0.1), 1)

#Values<-runif(50, -0.7, 0.7)
which(is.na(bird$tide))
bird<-bird[!is.na(bird$tide),]
bird$tide_level<-sapply(bird$tide[], FUN=function(x) which(Breaks==round(x, 1)))

library(RColorBrewer)
blues<-c("#F7FBFF", "#F7FBFF", "#F7FBFF", "#F7FBFF", "#DEEBF7", "#DEEBF7", "#DEEBF7", "#DEEBF7", "#DEEBF7", "#C6DBEF", "#C6DBEF", "#C6DBEF", "#C6DBEF", "#C6DBEF", "#C6DBEF", "#9ECAE1", "#9ECAE1", "#9ECAE1", "#9ECAE1","#9ECAE1", "#9ECAE1", "#6BAED6", "#6BAED6", "#6BAED6", "#6BAED6", "#6BAED6", "#6BAED6", "#4292C6", "#4292C6", "#4292C6","#4292C6", "#4292C6", "#4292C6", "#2171B5", "#2171B5", "#2171B5", "#2171B5", "#2171B5","#2171B5", "#08519C", "#08519C", "#08519C", "#08519C", "#08519C", "#08306B", "#08306B", "#08306B", "#08306B")

reds<-c("#FFFFCC", "#FFFFCC", "#FFFFCC", "#FFFFCC", "#FFEDA0","#FFEDA0", "#FFEDA0", "#FFEDA0", "#FFEDA0","#FFEDA0", "#FED976", "#FED976", "#FED976", "#FED976", "#FED976", "#FEB24C", "#FEB24C", "#FEB24C", "#FEB24C",
"#FEB24C", "#FEB24C", "#FD8D3C", "#FD8D3C", "#FD8D3C", "#FD8D3C", "#FD8D3C", "#FD8D3C", "#FC4E2A", "#FC4E2A", "#FC4E2A","#FC4E2A", "#FC4E2A", "#FC4E2A", "#E31A1C", "#E31A1C", "#E31A1C", "#E31A1C", "#E31A1C", "#E31A1C", "#BD0026","#BD0026", "#BD0026", "#BD0026", "#BD0026", "#800026", "#800026", "#800026", "#800026")


a<-GetMap(center=c(lat=11.2, lon=-15.9),  zoom=9, maptype='satellite')
PlotOnStaticMap(a)
bird_points<-LatLon2XY.centered(a, lat=bird$location_lat, lon=bird$location_long)
points(bird_points[[1]], bird_points[[2]] ,col="red",pch=".",cex=2)

#Bar-tailed Godwits and Whimbrels
bird.df<-as.data.frame(bird)
bird.df.bird<-bird.df[substr(bird.df$ local_identifier,1,4)=="bird",]
bird.df.whim<-bird.df[substr(bird.df$ local_identifier,1,4)=="Whim",]

#bird.df<-bird.df[bird.df$argos_error_radius<500,]

#day vs night & depth
par(mfrow=c(1,2))
a<-GetMap(center=c(lat=11.2, lon=-15.9),  zoom=10, maptype='satellite')
PlotOnStaticMap(a)
#bird points
neb<-LatLon2XY.centered(a, lat=bird.df.bird[bird.df.bird$daytime=="day",]$location_lat, lon=bird.df.bird[bird.df.bird$daytime=="day",]$location_long)
points(neb[[1]], neb[[2]] ,col=reds[bird.df.bird[bird.df.bird$daytime=="day",]$tide_level],pch=".",cex=2)
nvloed<-LatLon2XY.centered(a, lat=bird.df.bird[bird.df.bird$daytime=="night",]$location_lat, lon=bird.df.bird[bird.df.bird$daytime=="night",]$location_long)
points(nvloed[[1]], nvloed[[2]] ,col=blues[bird.df.bird[bird.df.bird$daytime=="night",]$tide_level],pch=".",cex=2)
#Whim points
PlotOnStaticMap(a)
neb<-LatLon2XY.centered(a, lat=bird.df.whim[bird.df.whim$daytime=="day",]$location_lat, lon=bird.df.whim[bird.df.whim$daytime=="day",]$location_long)
points(neb[[1]], neb[[2]] ,col=reds[bird.df.whim[bird.df.whim$daytime=="day",]$tide_level],pch=".",cex=2)
nvloed<-LatLon2XY.centered(a, lat=bird.df.whim[bird.df.whim$daytime=="night",]$location_lat, lon=bird.df.whim[bird.df.whim$daytime=="night",]$location_long)
points(nvloed[[1]], nvloed[[2]] ,col=blues[bird.df.whim[bird.df.whim$daytime=="night",]$tide_level],pch=".",cex=2)

#all spp
a<-GetMap(center=c(lat=11.2, lon=-15.9),  zoom=9, maptype='satellite')
PlotOnStaticMap(a)
neb<-LatLon2XY.centered(a, lat=bird.df[bird.df$daytime=="day",]$location_lat, lon=bird.df[bird.df$daytime=="day",]$location_long)
points(neb[[1]], neb[[2]] ,col=reds[bird.df[bird.df$daytime=="day",]$tide_level],pch=".",cex=2)
nvloed<-LatLon2XY.centered(a, lat=bird.df[bird.df$daytime=="night",]$location_lat, lon=bird.df[bird.df$daytime=="night",]$location_long)
points(nvloed[[1]], nvloed[[2]] ,col=blues[bird.df[bird.df$daytime=="night",]$tide_level],pch=".",cex=2)

#ebb and vloed
#all spp
a<-GetMap(center=c(lat=11.18, lon=-15.906),  zoom=10, maptype='satellite')
PlotOnStaticMap(a)
neb<-LatLon2XY.centered(a, lat=bird.df[bird.df$tide_phase=="eb",]$location_lat, lon=bird.df[bird.df$tide_phase=="eb",]$location_long)
points(neb[[1]], neb[[2]] ,col=reds[bird.df[bird.df$tide_phase=="eb",]$tide_level],pch=".",cex=2)
nvloed<-LatLon2XY.centered(a, lat=bird.df[bird.df$tide_phase=="vloed",]$location_lat, lon=bird.df[bird.df$tide_phase=="vloed",]$location_long)
points(nvloed[[1]], nvloed[[2]] ,col=blues[bird.df[bird.df$tide_phase=="vloed",]$tide_level],pch=".",cex=2)


###TIDE LEVELS FOR ANIMATION
#choose a low tide point to start
lowtides<- tide[tide$tidephase=="extremum" & tide$z.m.<0,]
hist(lowtides$z.m.)
lowtidesmin2<-lowtides[lowtides$z.m.<(-2),]
lowtidesmin2$Date<-as.POSIXct(round(lowtidesmin2$Time,"days"))
startdatelow<-lowtidesmin2[lowtidesmin2$Date==lowtidesmin2$Date[1],]
starttimelow<-startdatelow[which.min(startdatelow$z.m.),]$Time
head(tide[tide$Time>starttimelow,])

timeseq<-seq(as.POSIXct("2016-02-09 04:23:00", tz="GMT"), as.POSIXct("2016-02-09 16:34:00", tz="GMT"), by=60*12)
tidelevels<-as.data.frame(timeseq)
tidelevels$tide<-approx(x=tide$Time, y=tide$z.m., xout=tidelevels$timeseq)$y
#tidelevels$next_time<-shift(tidelevels$timeseq, n=1, fill=NA, type="lead")
#tidelevels$tide_next<-approx(x=tide$Time, y=tide$z.m., xout=tidelevels$next_time)$y
tidelevels$tide_next<-shift(tidelevels$tide, n=1, fill=NA, type="lead")
tidelevels$tidelevel<-1:nrow(tidelevels)
tidelevels$tidephase<-ifelse(as.numeric(tidelevels$timeseq)<as.numeric(as.POSIXct("2016-02-09 10:33:00", tz="GMT")),"vloed","eb")

tidelevels$tide[1]<-(-Inf)
tidelevels$tide_next[nrow(tidelevels)]<-(-Inf)

tidelevels$meantide<-NA
tidelevels$meantide[1]<-(-2.1)
tidelevels$meantide[nrow(tidelevels)]<-(-2.1)
for(i in 2:nrow(tidelevels)-1){
tidelevels$meantide[i]<-mean(c(tidelevels$tide[i],tidelevels$tide_next[i]))
}
#setwd('D:\\job\\Sciense\\Guinea_Bissau\\Tides\\Exposure\\Follow_up_2020')
#save(tidelevels,file="tidelevels.RData")

#TIDE LEVELS TO BIRD TRACKS

bird.df<-as.data.frame(bird)
bird.df$tide_level_new<-NA

for(i in 1:nrow(bird.df)){
cur_res<-tidelevels[apply(cbind(tidelevels$tide, tidelevels$tide_next), 1, min) <=bird.df$tide[i]
 & apply(cbind(tidelevels$tide, tidelevels$tide_next), 1, max)>bird.df$tide[i]
 & tidelevels$tidephase == bird.df$tide_phase[i],]$tidelevel
if (length(cur_res)>0) bird.df$tide_level_new[i]<-cur_res
 }

table(bird.df$tide_level_new)

