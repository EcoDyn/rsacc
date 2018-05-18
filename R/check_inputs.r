#' Check the validity of remote sensing classification data
#'
#' This function takes classification and validation inputs, given as *raster* or *Spatial* objects, and tests if projection are the same.
#'
#' @param map classification results as a *raster* or *SpatialPolygons* object.
#' @param val reference data for validating the classification. Accepts *raster*, *SpatialPolygons* or *SpatialPoints* objects.
#' @param reproj logical flag indicating if classification data should be reprojected to match reference data.
#' @return The sum of \code{x} and \code{y}
#'
#' @importFrom raster projection
#'
check_inputs <- function(map,val,reproj=FALSE){
    # Returns object classes so user knows what she is working with
    message(paste("Map data is a", class(map)))
    message(paste("Reference data is a", class(val)))

    # Inform user of map and val projections and test if they
    # are the same. Throw error if reproj = FALSE

    if (raster::projection(map) != raster::projection(val)){
        message(paste("Map projection: ",raster::projection(map)))
        message(paste("Reference projection: ",raster::projection(val)))
        if (reproj == TRUE){
            return(FALSE)
        } else {
            return(FALSE)
            stop("Error! Map projections are not equal. Use reproj=TRUE.")
        }
    } else {
        message(paste("Projections are the same: ",raster::projection(map)))
        return(TRUE)
    }
}
