#' Calculates quantity and allocation disagreement indexes.
#'
#' function to calculate the disagreement measures from:
#' Pontius Jr, Robert Gilmore, and Marco Millones. "Death to Kappa: birth of quantity disagreement and allocation disagreement for accuracy assessment." International Journal of Remote Sensing 32.15 (2011): 4407-4429.
#'
#' @param confmat a confusion matrix created by conf_mat(), or matrix with named columns and rows.
#' @return Overall agreement and disagreement, Allocation Disagreement, Quantity Disagreement.
#' @export
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
