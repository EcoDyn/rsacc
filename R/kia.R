#' Per class accuracies and *kappa* index of agreement
#'
#' Calculate per-class Omission and Comission errors and the Kappa index of agreement.
#'
#' @param confmat a confusion matrix created by conf_mat(), or matrix with named columns and rows.
#' @return Overall Accuracy, Kappa Index of Agreement, Per-class Omission and Comission Errors
#' @export
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
