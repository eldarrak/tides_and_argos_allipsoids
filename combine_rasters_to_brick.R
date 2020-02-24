
# setwd('d:\\maps\\Google_earth_engine\\GB\\whole_area_scale_10')

library(raster)
# we assume that files start from local_tidal_level_
# and that they are in a proper oder..
# this should be improved as we should sort by the tidal level and tidal phase. 

Files<-list.files(pattern='local_tidal_level_*')
File_numbers<-as.numeric(sapply(strsplit(Files, '_'), '[[', i=4))

Files<-Files[order(File_numbers)]

library(raster) # load the raster package
brk <- do.call(brick, lapply(Files, raster))

brk <- dropLayer(brk, 13)
brk <- dropLayer(brk, 4)

writeRaster(brk, 'brk_XXX.grd', overwrite=TRUE)

