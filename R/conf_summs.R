#' Calculates summary statistics and totals from a confusion matrix
#'
#' This function calculates diagonal sums, row sums, and similar summaries from the confusion matrix, which are needed to calculate accuracy metrics.
#'
#' @param confmat a confusion matrix created by conf_mat(), or matrix with named columns and rows.
#' @return Several useful statistics.

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
