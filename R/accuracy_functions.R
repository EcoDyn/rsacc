### Remote sensing classification validation using R

### Required libraries
library(raster)
library(rfUtilities)

# Set working folder
setwd("~/GoogleDrive/EcoDyn/People/Beatriz/map_validation/")

xeipe <- shapefile("acuracia_interest_2015.shp")

errmat <- accuracy(xeipe@data$classes,xeipe@data$validaclas)


#### Implementing Pontius

observed <- xeipe@data$validaclas
predicted <- xeipe@data$classes


# J - number of classes
if (length(unique(observed)) == length(unique(predicted))) J <- length(unique(predicted)) else stop("Number of observed and estimated classes does not match")

N <- length(observed)

# Error matrix
errmat <- table(predicted,observed)

# Pig (colsums)
Pig <- colSums(errmat)

# Pgj (rowsums)
Pgj <- rowSums(errmat)

# Pgg - diagonal

Pgg <- diag(errmat)

# Class quantity disagreement vector for J classes
Qvec <- abs(Pig - Pgj)

# Overall Quantity Disagreement
Q <- sum(Qvec/2)/N

# Allocation disagreement
Amat <- cbind(Pig-Pgg,Pgj-Pgg)
Avec <- unname(2 * apply(Amat,1,min))
A <- sum(Avec)/2/N

# Overall Agreement (Percent Correctly Classified - PCC)
PCC <- sum(Pgg)/N

# Overall Disagreement
D = 1 - PCC

##### Kappa Statistics #####

# Expected agreement

Eg <- Pig*Pgj

E <- sum(Eg)

K_std <-
