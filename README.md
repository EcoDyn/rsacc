# rsacc

An `R` package to perform accuracy assessment of remote sensing land-use classifications. The package can auto-detect input types, and then reproject and/or resample data if necessary and adapt calculations for different input combinations.

To install, you will need the `devtools` package installed:
```R
install.packages("devtools")
```

You can then install `rsacc` using:
```R
devtools::install_github("EcoDyn/rsacc")
```

There are only three functions for now. First, build the confusion matrix between classification and validation datasets. Classification can be given as a `raster` object (package `raster`) or a `SpatialPolygonsDataFrame` (usually imported using `raster::shapefile`). Validation data can be `raster`, `SpatialPolygonsDataFrame`, or `SpatialPointsDataFrame`. 

Example:
```R
library(raster)

> # read in classification data
> map_data <- raster("sample_data/LandsatClassification.tif")

> # read in validation data
> val_points <- shapefile("sample_data/site_447_classified_points.shp")

> # Build confusion matrix. 
> # Use reproj=T if projections are different.
> # Since validation data is a Spatial object, we need to specify the class field name

> cmat <- conf_mat(map_data,val_points,reproj=T,val_field="DN")

Map data is a RasterLayer
Reference data is a SpatialPointsDataFrame
Map projection:  +proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0
Reference projection:  +proj=utm +zone=23 +south +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0
Reprojecting validation to match map.

> cmat
   mapvals
      1   2   3   4
  1 187   0   0  17
  2   3 177   0   1
  3   3   1   1   5
  4  16   4   0  85
> 

```

If you are working with `Spatial` objects, be sure to specify column names where the class labels are stored, using `map_field = ` and `val_field = `. Also remember to specify nodata values using `na_val = `, to avoid class mismatches between classification and validation. For now, all datasets need to have the same nodata value. 

External confusion matrices can also be used, as long as they are an object of the `matrix` class and have named rows and columns.

Once you have a confusion matrix, there are two accuracy functions:`kia` calculates Overall Accuracy, Kappa Index of Agreement, and per-class Omission and Comission errors, and `pontius()` calculates Allocation and Quantity disagrement according to Pontius & Millones (2011).

```R
> kia(cmat)
$`Overall Accuracy`
                         Accuracy_results
Overall Accuracy                   0.9000
Overall Error                      0.1000
Kappa Index of Agreement           0.8467

$`Class Accuracy`
  Omission.Error Comission.Error
1         0.1053          0.0833
2         0.0275          0.0221
3         0.0000          0.9000
4         0.2130          0.1905

> pontius(cmat)
$`Pontius Disagreement Metrics`
                        Value
Overall Agreement       0.900
Overall Disagreement    0.100
Allocation Disagreement 0.082
Quantity Disagreement   0.018

> 
```












