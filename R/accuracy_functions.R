### Package rsacc: accuracy assessment of remote sensing classifications

### This file contains functions to compute several accuracy metrics for
### evaluating remote sensing classifications

### conf_summs: function to compute all commonly derived row,
### column and diagonal summaries from confidence matrices.
### Parameters:
### confmat: a confusion matrix computed using conf_mat()

conf_summs <- function(confmat){
    rn <- nrow(confmat) # number of rows
    cn <- ncol(confmat) # number of columns

    # Compute J - number of classes
    J <- if(rn==cn) {nrow(confmat)} else
    {stop("Unequal number of map and reference classes!")}

    # Class Names
    classnames <- colnames(confmat)

    # Total number of observations
    N <- sum(confmat)

    # Convert matrix to proportions to avoid integer overflowing
    confpmat <- confmat/N*100

    # Pig (colsums) x+j
    Pig <- colSums(confpmat)

    # Pgj (rowsums) xi+
    Pgj <- rowSums(confpmat)

    # Pgg - diagonal
    Pgg <- diag(confpmat)

    # Get all results into a list

    summs_list <- list(
        "confmat" =  confmat,
        "confpmat" = confpmat,
        "class_names" = classnames,
        "J" =  J,
        "N" =  N,
        "Pig" = Pig,
        "Pgj" = Pgj,
        "Pgg" = Pgg)

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

    # Overall Quantity Disagreement
    Q <- sum(Qvec/2)/100

    # Allocation disagreement
    Amat <- cbind(confsumms$Pig-confsumms$Pgg,confsumms$Pgj-confsumms$Pgg)
    Avec <- unname(2 * apply(Amat,1,min))
    A <- sum(Avec)/2/100

    # Overall Agreement (Percent Correctly Classified - PCC)
    PCC <- sum(confsumms$Pgg)/100

        # Overall Disagreement
    D = 1 - PCC

    # Present results as a nice table:
    pontius_df <- data.frame("Value" = round(c(PCC,D,A,Q),4),
                             row.names = c("Overall Agreement",
                                           "Overall Disagreement",
                                           "Allocation Disagreement",
                                           "Quantity Disagreement"))

    return(list("Pontius Disagreement Metrics" = pontius_df))
}

### kia: function for computing the Kappa index of agreement
### Parameters:
### confmat = confusion matrix computed with conf_mat()

kia <- function(confmat){
    # Calculate necessary confusion matrix summaries
    confsumms <- conf_summs(confmat)

    # Expected agreement
    Eg <- sum(confsumms$Pig*confsumms$Pgj)

    # N * Diagonal
    E <- 100 * sum(confsumms$Pgg)

    # Denominator N² * Eg
    D <- 100^2 - Eg

    # Kappa Index of Agreement
    khat <- (E-Eg)/D

    # Overall Accuracy (Percent Correctly Classified - PCC)
    PCC <- sum(confsumms$Pgg)/100

    # Per class accuracies: producer's accuracy / omission error
    pro_omm <- 1 - (confsumms$Pgg/confsumms$Pig)

    # Per class accuracies: user's accuracy / comission error
    use_com <- 1 - (confsumms$Pgg/confsumms$Pgj)

    # Format output as a data frame
    khat_df <- data.frame(Accuracy_results = round(c(PCC,1-PCC,khat),4),
                          row.names = c("Overall Accuracy",
                                        "Overall Error",
                                        "Kappa Index of Agreement"))

    perclass_df <- data.frame("Omission Error"= round(pro_omm,4),
                              "Comission Error" = round(use_com,4),
                              row.names=confsumms$class_names)


    return(list("Overall Accuracy"= khat_df,
                "Class Accuracy" = perclass_df))
}
