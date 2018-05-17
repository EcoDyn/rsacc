### Package rsacc: accuracy assessment of remote sensing classifications

### Testing script for all package functions

# Get test data
source("R/rsacc_test.R")

# We need our error matrix functions
source("R/build_confusion_matrix.R")



### Testing the functions

## Raster vs Raster
# reproj = F PASS
conf_mat(map_ras,val_ras)
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










