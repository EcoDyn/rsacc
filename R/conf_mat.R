#' Build a confusion matrix from classified *raster* or *Spatial* objects.
#'
#' This function builds confusion matrices by overlaying *raster* or *Spatial* objects, automatically choosing the best method depending on input class.
#'
#' @param map classification results as a *raster* or *SpatialPolygons* object.
#' @param val reference data for validating the classification. Accepts *raster*, *SpatialPolygons* or *SpatialPoints* objects.
#' @param map_field Which column of the classified *SpatialPolygonsDataFrame* has class labels? Only used if map input is a *SpatialPolygon* object
#' @param val_field Which column of the validation *SpatialPolygonsDataFrame* or *SpatialPointsDataFrame* has class labels? Only used if validation input is a *Spatial* object.
#' @param reproj logical flag indicating if classification data should be reprojected to match reference data.
#' @param na_val are there data values that should be considered as NODATA? Specified value will be replaced by NA.
#' @param use_extract use *extract* method instead of *resample* method, See notes.
#'
#' @details When the classification results are a *raster* object and the reference data is a *SpatialPolygonsDataFrame*, the default method is to rasterize the reference data to match the classification grid. This can be slow and memory-heavy for large rasters. Setting 'use_extract = TRUE' will extract pixel values from the classification raster, based on vector layer instead. Thus method may be slower than the 'rasterize" method if polygons cover a large portion of the classification raster.
#'
#' @return The sum of \code{x} and \code{y}
#'
#' @importFrom sp spTransform
#' @importFrom sp over
#' @importFrom raster projection
#' @importFrom raster resample
#' @importFrom raster projectRaster
#' @importFrom raster extract
#' @importFrom stats na.omit



#' @export
conf_mat <- function(map, val, map_field=NA, val_field=NA, na_val=NA, reproj=FALSE, use_extract = FALSE){

    # Check if all required parameters are given
    if (inherits(map,"Spatial") & is.na(map_field)){
        stop("Classification dataset is a vector. Please specify the field name holding class names using 'val_field'.")
    }
    if (inherits(val,"Spatial") & is.na(val_field)){
        stop("Reference dataset is a vector. Please specify the field name holding class names using 'val_field'.")
    }

    # Reprojects val data to match map
    check <- check_inputs(map,val,reproj = reproj)
    if (check == FALSE){
        if (reproj == FALSE){
        stop("Projections are not the same!")} else {
        message("Reprojecting validation to match map.")

            # If val is a Spatial object
            if (inherits(val,'Spatial')){
                val <- sp::spTransform(val,CRSobj = raster::projection(map))
            }

            # If val is a Raster object
            if (inherits(val,'Raster')){
            val <- raster::projectRaster(val,crs = raster::projection(map), method = "ngb")
            }
        }
    }
    # Build confusion matrix for Raster vs Raster
    if (class(map) == "RasterLayer" & class(val) == "RasterLayer"){
        map_val <- raster::resample(map,val)

        # Replace specified na_val with NA
        if (!is.na(na_val)){
            val[val == na_val] <- NA
            map_val[map_val == na_val] <- NA
        }

        # Compute confusion matrix
        cmat <- na.omit(raster::crosstab(map_val,val))
        cdf <- matrix(cmat$Freq,
                      nrow=length(unique(cmat$Var1)),
                      ncol=length(unique(cmat$Var2)),
                      byrow=F)
        valnames <- na.omit(as.character(unique(cmat$Var2)))
        mapnames <- na.omit(as.character(unique(cmat$Var1)))
        colnames(cdf) <- valnames
        rownames(cdf) <- mapnames
        return(cdf)
    }

    # Build confusion matrix for Raster vs SpatialPolygons
    if (class(map) == "RasterLayer" & inherits(val,"SpatialPolygons")) {
        if (use_extract == FALSE){
            
            ### rasterize method
            message("Using 'rasterize' method. Might be slow and/or result in a memory error if classification raster is too large. Consider using 'use_extract = TRUE'.")
            rasval <- raster::rasterize(val, map, field = as.numeric(val@data[,val_field]))

            # Replace specified na_val with NA
            if (!is.na(na_val)){
                rasval[rasval == na_val] <- NA
                map[map == na_val] <- NA
            }

            cmat <- raster::crosstab(map,rasval)
            cdf <- matrix(cmat$Freq,
                          nrow=length(unique(cmat$Var1)),
                          ncol=length(unique(cmat$Var2)),
                          byrow=F)
            valnames <- na.omit(as.character(unique(cmat$Var2)))
            mapnames <- na.omit(as.character(unique(cmat$Var1)))
            colnames(cdf) <- valnames
            rownames(cdf) <- mapnames
            return(cdf)

        } else {
            
            ### extract method
            message("Using 'extract' method. Might be slow if validation polygons cover a large portion of the classified raster. Consider using 'use_extract = FALSE'.")
            classdf <- raster::extract(map,val,df=T)
            valdf <- data.frame(class_val = val@data[,c(val_field)],
                      ID = sapply(slot(val, "polygons"), function(x) slot(x, "ID")))
            cvdf <- merge(classdf,valdf,by='ID')
            cmat <-  table(cvdf[,-1])
            return(cmat)
        }
    }

    # Build confusion matrix for Raster vs SpatialPoints
    if (class(map) == "RasterLayer" & inherits(val,"SpatialPoints")) {

        # Replace specified na_val with NA
        if (!is.na(na_val)){
            val@data[val@data[,val_field] == na_val,val_field] <- NA
            map[map == na_val] <- NA
        }

        mapvals <- raster::extract(map,val)
        cmat <-  table(val@data[,val_field],mapvals)
    }

    # Build confusion matrix for SpatialPolygons vs SpatialPolygons
    if (inherits(map, "SpatialPolygons") & inherits(val,"SpatialPolygons")) {
        stop("Not supported yet! :-(")
    }

    # Build confusion matrix for SpatialPolygons vs SpatialPoints
    if (inherits(map, "SpatialPolygons") & inherits(val,"SpatialPoints")) {
        mapvals <- sp::over(val, map[,map_field])
        cmat <- table(val@data[,val_field],mapvals[,map_field])
    }
    return(cmat)
}
