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
### NOTE: Projections are intentionally not the same between classification
### and validation, to test projection checks and reprojection options

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
    message(paste("Map data is a", class(map)))
    message(paste("Reference data is a", class(val)))

    # Inform user of map and val projections and test if they
    # are the same. Throw error if reproj = FALSE

    if (projection(map) != projection(val)){
        message(paste("Map projection: ",projection(map)))
        message(paste("Reference projection: ",projection(val)))
        if (reproj == TRUE){
           return(FALSE)
        } else {
            return(FALSE)
            stop("Error! Map projections are not equal. Use reproj=TRUE.")
        }
    } else {
        message(paste("Projections are the same: ",projection(map)))
        return(TRUE)
    }
}

### Function to build the confusion matrix
#### Parameters are:
#### map = classification results: raster or spatial polygons
#### val = reference data: raster, spatial polygons or spatial points
#### field = name of the class label field on spatial data
#### reproj = should the validation be reprojected to match the classification?

conf_mat <- function(map, val, map_field=NA, val_field=NA, na_val=NA, reproj=FALSE){

    # Check if all required parameters are given
    if (inherits(map,"Spatial") & is.na(map_field)){
        stop("Classification dataset is a vector. Please specify the field name holding class names using 'val_field'.")
    }
    if (inherits(val,"Spatial") & is.na(val_field)){
        stop("Reference dataset is a vector. Please specify the field name holding class names using 'val_field'.")
    }

    # Reprojects val data to match map
    check <- check_inputs(map,val,reproj = reproj)
    if (check == FALSE & reproj == FALSE){
        stop("Projections are not the same!")} else {
        message("Reprojecting validation to match map.")

        # If val is a Spatial object
        if (inherits(val,'Spatial')){
            val <- spTransform(val,CRSobj = projection(map))
        }

        # If val is a Raster object
        if (inherits(val,'Raster')){
        val <- projectRaster(val,crs = projection(map), method = "ngb")
        }
    }

    # Build confusion matrix for Raster vs Raster
    if (class(map) == "RasterLayer" & class(val) == "RasterLayer"){
        map_val <- resample(map,val)

        # Replace specified na_val with NA
        if (!is.na(na_val)){
            val[val == na_val] <- NA
            map_val[map_val == na_val] <- NA
        }

        # Compute confusion matrix
        cmat <- na.omit(crosstab(map_val,val))
        cdf <- as.data.frame(matrix(cmat$Freq,
                                    nrow=length(unique(cmat$Var1)),
                                    ncol=length(unique(cmat$Var2)),
                                    byrow=F))
        valnames <- na.omit(as.character(unique(cmat$Var2)))
        mapnames <- na.omit(as.character(unique(cmat$Var1)))
        names(cdf) <- valnames
        rownames(cdf) <- mapnames
        return(cdf)
    }

    # Build confusion matrix for Raster vs SpatialPolygons
    if (class(map) == "RasterLayer" & inherits(val,"SpatialPolygons")) {
        rasval <- rasterize(val, map, field = val_field)

        # Replace specified na_val with NA
        if (!is.na(na_val)){
            rasval[rasval == na_val] <- NA
            map[map == na_val] <- NA
        }

        cmat <-  na.omit(crosstab(map,rasval))
        cdf <- as.data.frame(matrix(cmat$Freq,
                                    nrow=length(unique(cmat$Var1)),
                                    ncol=length(unique(cmat$Var2)),
                                    byrow=F))
        valnames <- na.omit(as.character(unique(cmat$Var2)))
        mapnames <- na.omit(as.character(unique(cmat$Var1)))
        names(cdf) <- valnames
        rownames(cdf) <- mapnames
        return(cdf)
    }

    # Build confusion matrix for Raster vs SpatialPoints
    if (class(map) == "RasterLayer" & inherits(val,"SpatialPoints")) {

        # Replace specified na_val with NA
        if (!is.na(na_val)){
            val@data[val@data[,val_field] == na_val,val_field] <- NA
            map[map == na_val] <- NA
        }

        mapvals <- extract(map,val)
        cmat <-  table(val@data[,val_field],mapvals)
    }

    # Build confusion matrix for SpatialPolygons vs SpatialPolygons
    if (inherits(map, "SpatialPolygons") & inherits(val,"SpatialPolygons")) {
        stop("Not supported yet! :-(")
    }

    # Build confusion matrix for SpatialPolygons vs SpatialPoints
    if (inherits(map, "SpatialPolygons") & inherits(val,"SpatialPoints")) {
        mapvals <- over(val, map[,map_field])
        cmat <- table(val@data[,val_field],mapvals[,map_field])
    }
    return(cmat)
}

### Testing the functions

## Raster vs Raster
# reproj = F PASS
conf_mat(map_ras,val_ras)
# reproj = T PASS
cf <- conf_mat(map_ras,val_ras,reproj=T)
# reproj = T & na_val = 0 PASS
conf_mat(map_ras,val_ras,reproj=T, na_val=0)


## Raster vs SpatialPolygons
# reproj = F PASS
conf_mat(map_ras,val_poly)
# reproj = T
conf_mat(map_ras,val_poly,reproj=T)
# reproj = T & val_field given PASS
conf_mat(map_ras,val_poly,reproj=T,val_field = "DN")
# reproj = T & val_field given & na_val = 0 PASS
conf_mat(map_ras,val_poly,reproj=T,val_field = "DN",na_val = 0)

## Raster vs SpatialPoints

# reproj = F PASS
conf_mat(map_ras,val_points)
# reproj = T PASS
conf_mat(map_ras,val_points,reproj=T)
# reproj = T & field != NA PASS
cf <- conf_mat(map_ras, val_points, val_field = "DN", reproj=T)
# reproj = T & field != NA & na_val given PASS
cf <- conf_mat(map_ras, val_points, val_field = "DN", reproj=T, na_val=0)


## SpatialPolygon vs SpatialPolygon
# map_field = NA PASS
conf_mat(map_poly,val_poly)
# map_field = given and val_field = NA PASS
conf_mat(map_poly,val_poly, map_field="DN")
# map_field = given, val_field = given, reproj = F PASS
conf_mat(map_poly,val_poly, map_field="DN", val_field = "DN")
# map_field = given, val_field = given, reproj = T PASS **BUT NOT IMPLEMENTED**
conf_mat(map_poly,val_poly, map_field="DN", val_field = "DN", reproj = T)
# map_field = given, val_field = given, reproj = T, na_val = given NOT TESTED
conf_mat(map_poly,val_poly, map_field="DN", val_field = "DN", reproj = T)


## SpatialPolygon vs SpatialPoints
# map_field = NA PASS
conf_mat(map_poly,val_points)
# map_field = given and val_field = NA PASS
conf_mat(map_poly,val_points, map_field="DN")
# map_field = given, val_field = given, reproj = F PASS
conf_mat(map_poly,val_points, map_field="DN", val_field = "DN")
# map_field = given, val_field = given, reproj = T PASS
cf <- conf_mat(map_poly,val_points, map_field="DN", val_field = "DN", reproj = T)
# map_field = given, val_field = given, reproj = T, na_val = given PASS
cf <- conf_mat(map_poly,val_points, map_field="DN", val_field = "DN", reproj = T, na_val = 0)


