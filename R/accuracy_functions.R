### Package rsacc: accuracy assessment of remote sensing classifications

### This file contains functions to compute several accuracy metrics for
### evaluating remote sensing classifications

### Dependencies

# Get test data
source("R/rsacc_test.R")

# We need our error matrix functions
source("R/build_confusion_matrix.R")

# We'll work with Raster vs SpatialPoints for now

# Compute error matrix
confmat<- conf_mat(map = map_ras,
                  val = val_points,
                  val_field = "DN",
                  na_val = 0,
                  reproj = T)

### conf_summs: function to compute all commonly derived row,
### column and diagonal summaries from confidence matrices.
### Parameters:
### confmat: a confusion matrix computed using conf_mat()

conf_summs <- function(confmat){
    rn <- nrow(confmat) # number of rows
    cn <- ncol(confmat) # number of columns
    # Compute J - number of classes
    J <- if(rn==cn) {nrow(errmat)} else
    {stop("Unequal number of map and reference classes!")}

    # Total number of observations
    N <- sum(confmat)

    # Pig (colsums) x+j
    Pig <- colSums(confmat)

    # Pgj (rowsums) xi+
    Pgj <- rowSums(confmat)

    # Pgg - diagonal
    Pgg <- diag(confmat)

    # For proportion matrix

    confpmat <- confmat/N

    # Pig (colsums) x+j
    pPig <- colSums(confpmat)

    # Pgj (rowsums) xi+
    pPgj <- rowSums(confpmat)

    # Pgg - diagonal
    pPgg <- diag(confpmat)

    # Get all results into a list

    summs_list <- list(
        "confmat" =  confmat,
        "confpmat" = confpmat,
        "J" =  J,
        "N" =  N,
        "Pig" = Pig,
        "Pgj" = Pgj,
        "Pgg" = Pgg,
        "pPig" = pPig,
        "pPgj" = pPgj,
        "pPgg" = pPgg)

    return(summs_list)
}

### pontius: function to calculate the disagreement measures from
### Pontius Jr, Robert Gilmore, and Marco Millones. "Death to Kappa: birth of
### quantity disagreement and allocation disagreement for accuracy assessment."
### International Journal of Remote Sensing 32.15 (2011): 4407-4429.

pontius <- function(confmat){
    confsumms <- conf_summs(confmat)

    # Class quantity disagreement vector for J classes
    Qvec <- abs(confsumms$Pig - confsumms$Pgj)
    pQvec <- abs(confsumms$pPig - confsumms$pPgj)

    # Overall Quantity Disagreement
    Q <- sum(Qvec/2)/N
    pQ <- sum(pQvec/2)

    # Allocation disagreement
    Amat <- cbind(confsumms$Pig-confsumms$Pgg,confsumms$Pgj-confsumms$Pgg)
    Avec <- unname(2 * apply(Amat,1,min))
    A <- sum(Avec)/2/N

    pAmat <- cbind(confsumms$pPig-confsumms$pPgg,confsumms$pPgj-confsumms$pPgg)
    pAvec <- unname(2 * apply(pAmat,1,min))
    pA <- sum(pAvec)/2

    # Overall Agreement (Percent Correctly Classified - PCC)
    PCC <- sum(confsumms$Pgg)/N
    pPCC <- sum(confsumms$pPgg)
    # Overall Disagreement
    D = 1 - PCC
    pD = 1 - pPCC
    # Present results as a nice table:
    pontius_df <- data.frame(count_based = c(PCC,D,A,Q),
                             proportion_based = c(pPCC,pD,pA,pQ),
                             row.names = c("Overall Agreement",
                                           "Overall Disagreement",
                                           "Allocation Disagreement",
                                           "Quantity Disagreement"))
    return(pontius_df)
}

### kia: function for computing the Kappa index of agreement
### Parameters:
### confmat = confusion matrix computed with conf_mat()

kia <- function(confmat)
    # Calculate necessary confusion matrix summaries
    confsumms <- conf_summs(confmat)

    # Expected agreement
    Eg <- confsumms$Pig*confsumms$Pgj
    E <- sum(Eg)

K_std <-
}
