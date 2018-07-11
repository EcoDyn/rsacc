library(raster)

# real use data
map <- raster("/mnt/Dados/Imagery/thais-validacao/CLAS_CED_SX260_2016_09_25_MOS_GEO_5cm.tif")
val <- shapefile("/mnt/Dados/Imagery/thais-validacao/amostras_validacao.shp")
val_field <- 'class_code' # name of SPDF column holding class labels

# simulated rasters

set.seed(123)
r1 <- raster(nrows=100, ncols=100, xmn=0, xmx=10, vals = as.integer(runif(10000,1,9)))
r2 <- raster(nrows=100, ncols=100, xmn=0, xmx=10, vals = as.integer(runif(10000,1,9)))

# rasterizing the polygon data
rasval <- raster::rasterize(val, map, field = as.numeric(val@data[,val_field]))

#crosstabulation of the simulated data
ctab1 <- crosstab(r1,r2,useNA=F)
class(ctab1)
str(ctab1)
head(ctab1)

#crosstabulation of the real data
ctab2 <- raster::crosstab(map,rasval)
class(ctab2)
str(ctab2)
head(ctab2)

#with long = T we get the same result
ctab3 <- crosstab(r1,r2,long=T)
class(ctab3)
str(ctab3)
head(ctab3)


#crosstabulation of the real data
ctab2 <- raster::crosstab(map,rasval)
class(ctab2)
str(ctab2)
head(ctab2)

# raster properties
map
rasval
r1
r2
