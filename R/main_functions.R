### Accuracy assessment of remote sensing classifications

### Input data are 'map' and 'val', which would be raster or spatial objects
### read in usinf the raster package

### Dependencies

library(raster)
library(rgdal)
library(sp)
library(rgeos)
library(maptools)

### test data

### Test data is comprised of a Landsat classification of a region
### in Sao Paulo, Brazil, made using Google Earth Engine.
###
### Training and validation data was extracted from the
### Global Land Cover Validation Reference Dataset:
###
### https://landcover.usgs.gov/glc/
### /sample_data/readme_Global_30m_Land_Cover_ReferenceDatatset.txt
###

### Classes are:
### 0 = No Data
### 1 = Tree
### 2 = Water
### 3 = Barren
### 4 = Other Vegetation

### Raster version of the classified region, in GeoTIFF format:
map_ras <- raster("sample_data/LandsatClassification.tif")

### The polygonized version of the above dataset:
map_poly <- shapefile("sample_data/LandsatClassification/LandsatClassification.shp")

### This is the validation scene obtained as above.
val_ras <- raster("sample_data/site_447_classified.tif")

### Validation scene converted to polygons
val_poly <- shapefile("sample_data/site_447_classified_poly.shp")

### Random points extracted from the validation scene
val_points <- shapefile("sample_data/site_447_classified_points.shp")


#### Function to check the type and validity of the files
#### Parameters are:
#### map = classification results: raster or spatial polygons
#### val = reference data: raster, spatial polygons or spatial points
#### reproj = should the validation be reprojected to match the classification?

check_inputs <- function(map,val,reproj=FALSE){
    # Returns object classes so user knows what she is working with
    print(paste("Map data is a", class(map)))
    print(paste("Reference data is a", class(val)))

    # Inform user of map and val projections and test if they
    # are the same. Throw error if reproj = FALSE

    if (projection(map) != projection(val)){
        print(paste("Map projection: ",projection(map)))
        print(paste("Reference projection: ",projection(val)))
        if (reproj == TRUE){
           return(FALSE)
        } else {
            return(FALSE)
            stop("Error! Map projections are not equal. Use reproj=TRUE.")
        }
    } else {
        print(paste("Projections are the same: ",projection(map)))
        return(TRUE)
    }
}

### Function to build the confusion matrix
#### Parameters are:
#### map = classification results: raster or spatial polygons
#### val = reference data: raster, spatial polygons or spatial points
#### reproj = should the validation be reprojected to match the classification?

conf_mat <- function(map, val, field, reproj=FALSE){

    # Reprojects val data to match map
    check <- check_inputs(map,val,reproj = reproj)
    if (check == FALSE & reproj == TRUE){
        print("Reprojecting validation to match map.")

        # If val is a Spatial object
        if (inherits(val,'Spatial')){
            val <- spTransform(val,CRSobj = projection(map))
            print(projection(val))
        }

        # If val is a Raster object
        if (inherits(val,'Raster')){
        val <- projectRaster(val,crs = projection(map))
        print(projection(val))
        }
    }

    # Build confusion matrix for Raster vs raster
    if (class(map) == "RasterLayer" & class(val) == "RasterLayer"){
        map_val <- resample(map,val)
        cmat <- crosstab(map_val,val)
    }
    # Build confusion matrix for Raster vs SpatialPolygons
    if (class(map) == "RasterLayer" & inherits(val,"SpatialPolygons")) {
        rasval <- rasterize(val, map, field = field)
        cmat <-  crosstab(map,rasval)
    }
    # Build confusion matrix for Raster vs SpatialPoints
    if (class(map) == "RasterLayer" & inherits(val,"SpatialPoints")) {
        mapvals <- extract(map,val,sp=T)
        cmat <-  table(rasval[,field],mapvals)
    }
    # Build confusion matrix for SpatialPolygons vs SpatialPolygons
    if (inherits(map, "SpatialPolygons") & inherits(val,"SpatialPolygons")) {
        stop("Not supported yet! :-(")
    }
    # Build confusion matrix for SpatialPolygons vs SpatialPoints
    if (inherits(map, "SpatialPolygons") & inherits(val,"SpatialPoints")) {
        ids <- unlist(over(val,map_poly))
        classdf <- data.frame(mapvals=map_poly@data$DN[ids],valvals=val$DN)
        cmat <- table(classdf)
    }
    return(cmat)
}

# Testing functions

# raster vs poly
# raster vs points
# poly vs poly
# poly vs points


