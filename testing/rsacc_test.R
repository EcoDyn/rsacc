### Package rsacc: accuracy assessment of remote sensing classifications

### rscacc_test -  scriot to load testing data for all package functions

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

library(raster)

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









