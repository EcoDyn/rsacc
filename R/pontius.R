#' Calculates quantity, allocation, exchange and shift disagreement indexes.
#'
#' function to calculate the disagreement measures from:
#' Pontius Jr, Robert Gilmore, and Millones, Marco. "Death to Kappa: birth of quantity disagreement and allocation disagreement for accuracy assessment." International Journal of Remote Sensing 32.15 (2011): 4407-4429 AND Pontius Jr, Robert Gilmore, and Santacruz, Alí. "Quantity, exchange, and shift components of difference in a square contingency table." International Journal of Remote Sensing, 35.21 (2014): 7543–7554.
#'
#' @param confmat a confusion matrix created by conf_mat(), or matrix with named columns and rows.
#' @return Overall agreement, disagreement, Quantity Disagreement, Allocation Disagreement, Exchange Disagreement, Shift Disagreement, and metrics at class level.
#' @importFrom diffeR overallExchangeD
#' @importFrom diffeR overallShiftD
#' @importFrom diffeR diffTablej
#' @export
pontius <- function(confmat){
    confsumms <- conf_summs(confmat)

    # Class quantity disagreement vector for J classes as in Pontius et al 2011
    Qvec <- abs(confsumms$Pig - confsumms$Pgj)

    # Overall Quantity Disagreement as in Pontius et al 2011
    Q <- sum(Qvec/2)/100

    # Allocation disagreement as in Pontius et al 2011
    Amat <- cbind(confsumms$Pig-confsumms$Pgg,confsumms$Pgj-confsumms$Pgg)
    Avec <- unname(2 * apply(Amat,1,min))
    A <- sum(Avec)/2/100

    # Overall Agreement (Percent Correctly Classified - PCC) as in Pontius et al 2011
    PCC <- sum(confsumms$Pgg)/100

    # Overall Disagreement as in Pontius et al 2011
    D = 1 - PCC

    # Overall Exchange Disagreement as in Pontius and Santacruz 2014
    E <- overallExchangeD(confsumms$confpmat)/100

    # Overall Shift Disagreement as in Pontius and Santacruz 2014
    S <- overallShiftD(confsumms$confpmat)/100

    # Decomponsing metrics per class as in Pontius and Santacruz 2014
    class_metrics <- diffTablej(unclass(confsumms$confpmat))

    # Droping E and S from diffTablej, alreade computed to be shown in our 'output style'
    class_metrics <- class_metrics[-nrow(class_metrics), ]

    # Converting number display to absolute instead of relative (default of diffTablej)
    # and round to 4 decimal digits (droping meaningful factor column for this calculation)
    temp <- round(class_metrics[,2:ncol(class_metrics)]/100,4)

    # Joining classes again
    class_metrics <- cbind(class_metrics$Category, temp)
    names(class_metrics) <- c("Classes", names(temp))
    rownames(class_metrics) <- NULL


    # Present results as a nice table:
    pontius2011_df <- data.frame("Value" = round(c(PCC,D,Q,A),4),
                             row.names = c("Overall Agreement",
                                           "Overall Disagreement",
                                           "Overall Quantity Disagreement",
                                           "Overall Allocation Disagreement"))

    pontius2014_df <- data.frame("Value" = round(c(E,S),4),
                                 row.names = c("Overall Exchange Disagreement",
                                               "Overall Shift Disagreement"))



    return(c(list("Pontius et al. 2011 Disagreement Metrics:" = pontius2011_df),
             list("Decomposing your Allocation Disagreement as in Pontius and Santacruz 2014:" = pontius2014_df),
             list("Disagreement Metrics at class level:" = class_metrics)))


}
