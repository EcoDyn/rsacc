### Package rsacc: accuracy assessment of remote sensing classifications

### Testing script for all package functions

# Get test data
source("R/rsacc_test.R")

# We need our error matrix functions
source("R/build_confusion_matrix.R")

### Testing the functions

## Raster vs Raster
# reproj = F PASS
RvR_rpjF <- conf_mat(map_ras,val_ras)
# reproj = T PASS
RvR_rpjT <- conf_mat(map_ras,val_ras,reproj=T)
# reproj = T & na_val = 0 PASS
RvR_rpjT_nav0 <- conf_mat(map_ras,val_ras,reproj=T, na_val=0)

## Raster vs SpatialPolygons
# reproj = F PASS
RvSPl_rpjF <- conf_mat(map_ras,val_poly)
# reproj = T
RvSPl_rpjT <- conf_mat(map_ras,val_poly,reproj=T)
# reproj = T & val_field given PASS
RvSPl_rpjT_vf <- conf_mat(map_ras,val_poly,reproj=T,val_field = "DN")
# reproj = T & val_field given & na_val = 0 PASS
RvSPl_rpjT_vf_nav0 <- conf_mat(map_ras,val_poly,reproj=T,val_field = "DN",na_val = 0)

## Raster vs SpatialPoints

# reproj = F PASS
RvSPt_rpjF <- conf_mat(map_ras,val_points)
# reproj = T PASS
RvSPt_rpjT <- conf_mat(map_ras,val_points,reproj=T)
# reproj = T & val_field != NA PASS
RvSPt_rpjT_vf <- conf_mat(map_ras, val_points, val_field = "DN", reproj=T)
# reproj = T & val_field != NA & na_val given PASS
RvSPt_rpjT_vf_nav0 <- conf_mat(map_ras, val_points, val_field = "DN", reproj=T, na_val=0)


## SpatialPolygon vs SpatialPolygon
# map_field = NA PASS
SPlvSPl <- conf_mat(map_poly,val_poly)
# map_field = given and val_field = NA PASS
SPlvSPl_mf <- conf_mat(map_poly,val_poly, map_field="DN")
# map_field = given, val_field = given, reproj = F PASS
SPlvSPl_mf_vf_rpjF <- conf_mat(map_poly,val_poly, map_field="DN", val_field = "DN")
# map_field = given, val_field = given, reproj = T PASS **BUT NOT IMPLEMENTED**
SPlvSPl_mf_vf_rpjT <- conf_mat(map_poly,val_poly, map_field="DN", val_field = "DN", reproj = T)
# map_field = given, val_field = given, reproj = T, na_val = given NOT TESTED
SPlvSPl_mf_vf_rpjT_nav0 <- conf_mat(map_poly,val_poly, map_field="DN", val_field = "DN", reproj = T)


## SpatialPolygon vs SpatialPoints
# map_field = NA PASS
SPlvSPt <- conf_mat(map_poly,val_points)
# map_field = given and val_field = NA PASS
SPlvSPt_mf <- conf_mat(map_poly,val_points, map_field="DN")
# map_field = given, val_field = given, reproj = F PASS
SPlvSPt_mf_vf_rpjF <- conf_mat(map_poly,val_points, map_field="DN", val_field = "DN")
# map_field = given, val_field = given, reproj = T PASS
SPlvSPt_mf_vf_rpjT <- conf_mat(map_poly,val_points, map_field="DN", val_field = "DN", reproj = T)
# map_field = given, val_field = given, reproj = T, na_val = given PASS
SPlvSPt_mf_vf_rpjT_nva0 <- conf_mat(map_poly,val_points, map_field="DN", val_field = "DN", reproj = T, na_val = 0)

# Bring accuracy functions
source("R/accuracy_functions.R")

####### Testing accuracy functions

### RvR PASS
pontius(RvR_rpjT)

### RvR NA0 PASS
pontius(RvR_rpjT_nav0)
kia(RvR_rpjT_nav0)

### RvSPl PASS
pontius(RvSPl_rpjT_vf)
kia(RvSPt_rpjT_vf)

### RvSPl NA 0 PASS
pontius(RvSPl_rpjT_vf_nav0)
kia(RvSPl_rpjT_vf_nav0)

### RvSPt
pontius(RvSPt_rpjT_vf)
kia(RvSPt_rpjT_vf)

### RvSPt NA0
pontius(RvSPt_rpjT_vf_nav0)
kia(RvSPt_rpjT_vf_nav0)

### SPlvSPt
pontius(SPlvSPt_mf_vf_rpjT)
kia(SPlvSPt_mf_vf_rpjT)

