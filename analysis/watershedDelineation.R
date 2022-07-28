setwd("C:/Users/echildress/Documents/mapShen")
library(mapShen)
library(rgrass7)
use_sp()
#read raster image
dem <- raster::raster("data/gis/dem/w001001.adf")
#plot(r)
# r
sites<-aqData("sites")
utmX<-sites[SiteID=="1F030",UTMX_E]
utmY<-sites[SiteID=="1F030",UTMY_N]

system('"C:/Program Files/GRASS GIS 7.8/grass78.bat"')

#Initiate the GRASS GIS within R studio
initGRASS(gisBase='C:/Program Files/GRASS GIS 7.8', gisDbase='C:/Users/echildress/Documents/grassdata',
          location="newLocation3",mapset="PERMANENT", override = TRUE)



#create spatial polygons to crop the raster
buffer<-5000
plot(dem)
ext<- raster::extent(utmX-buffer,utmX+buffer,utmY-buffer,utmY+buffer)
pol <- as(ext, "SpatialPolygons")
plot(pol,add=T)
polCrop<-raster::crop(dem, pol)
ext2 <- raster::extent(polCrop)
r2 <- raster(ext2,nrow=nrow(polCrop)/2, ncol=ncol(polCrop)/2)

#resample the cell size if necessary
new_c <- raster::resample(polCrop, r2, method="ngb")
rast <- as(new_c, "SpatialGridDataFrame")

demSp<-as(dem,"SpatialGridDataFrame")

crs(rast)<-crs(dem)
writeRAST(demSp,"rast_img", flags = c("overwrite"))
execGRASS("r.info", map = "rast_img")

#set the region based on the mapset
execGRASS("g.region", raster = "rast_img")
out_raster <- readRAST("rast_img")
str(out_raster)
plot(out_raster)

#watershed delineation
# execGRASS("r.watershed", flags="overwrite",
#           parameters=list(elevation="rast_img", threshold=50000,
#                           drainage= "fdir",stream="upstream", basin="rbasin"))

execGRASS("r.watershed", flags="overwrite",
          parameters=list(elevation="rast_img", threshold=500,
                          drainage="flow_dir_grid"))
#
# execGRASS('r.thin', flags='overwrite',
#           parameters =  list(input='upstream',
#                              output='riv_thin'))
# execGRASS("r.to.vect", flags='overwrite',
#           parameters = list(input="riv_thin",
#                             output="streams", type="line"))
# execGRASS('r.to.vect', flags='overwrite',
#           parameters =  list(input='rbasin',
#                              output='catchments', type="area"))
# execGRASS('v.out.ogr', flags=c('overwrite'),
#           parameters=list(input='catchments',
#                           output="area.shp",type="area",
#                           format="ESRI_Shapefile"))
# execGRASS('v.out.ogr',flags=c('overwrite'),parameters=list(input='streams',
#                                                            output="streams.shp",type="line",format="ESRI_Shapefile"))
#
# shapefile("streams.shp") -> s
# shapefile("area.shp") -> a
# plot(demSp)
# plot(a, add=T)
# plot(s, add=T, col="red")

execGRASS("r.water.outlet",
          parameters=list(drainage="flow_dir_grid",
                          basin="basin1",
                          easting=utmX,
                          northing=utmY))
