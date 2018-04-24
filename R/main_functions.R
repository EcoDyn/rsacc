### Accuracy assessment of remote sensing classifications

### Input data are 'map' and 'val', which would be raster or spatial objects
### read in usinf the raster package

### Dependencies

library(raster)
library(rgdal)
library(sp)
### Read in test data

map_ras <- raster("sample_data/LandsatClassification.tif")
map_poly <- shapefile("sample_data/LandsatClassification/LandsatClassification.shp")

val_ras <- raster("sample_data/site_447_classified.tif")
val_poly <- shapefile("sample_data/site_447_classified_poly.shp")
val_points <- shapefile("sample_data/site_447_classified_points.shp")


#### Function to check the type and validity of the files

check_inputs <- function(map,val,reproj=FALSE){
    print(paste("Map data is a", class(map)))
    print(paste("Reference data is a", class(val)))
    if (projection(map) != projection(val)){
        print(paste("Map projection: ",projection(map)))
        print(paste("Reference projection: ",projection(val)))
        if (reproj == TRUE){
           return(FALSE)
        } else {
            print("Error! Map projections are not equal. Use reproj=TRUE.")
            return(FALSE)
        }
    } else {
        print("Projections are the same.")
        return(TRUE)
    }
}




### Build confusion table
conf_mat <- function(map, val, field, reproj=FALSE){
    check <- check_inputs(map,val,reproj = reproj)
    if (check == FALSE & reproj == TRUE){
        print("Reprojecting validation to match map.")
        if (inherits(val,'Spatial')){
            val <- spTransform(val,CRSobj = projection(map))
            print(projection(val))
        }
        if (inherits(val,'Raster')){
        val <- projectRaster(val,crs = projection(map))
        print(projection(val))
        }
    }
    # raster vs raster
    if (class(map) == "RasterLayer" & class(val) == "RasterLayer"){
        map_val <- resample(map,val)
        cmat <- crosstab(map_val,val)
    }
    if (class(map) == "RasterLayer" & inherits(val,"SpatialPolygons")) {
        rasval <- rasterize(val, map, field = field)
        cmat <-  crosstab(map,rasval)
    }
    if (class(map) == "RasterLayer" & inherits(val,"SpatialPoints")) {
        mapvals <- extract(map,val,sp=T)
        cmat <-  table(rasval[,field],mapvals)
    }
    return(cmat)
}


    # raster vs poly
    # raster vs points
    # poly vs poly
    # poly vs points


