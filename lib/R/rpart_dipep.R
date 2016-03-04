#' Run Recurrsive Partitioning on DiPEP
#'
#' @description Perform Recursive Partitioning on DiPEP study
#'
#' @details
#'
#' Recursive Partitioning is one of the methods of testing for predictors of pulmonary embolism
#' in pregnant women that are to be tested as part of the DiPEP study.  This function serves as
#' a wrapper, allowing the user to choose the variables to include in the model
#' between the LASSO method implemented in the \code{lars} package or that in the \code{glmnet}
#' package.
#'
#' @param df Data frame with all variables
#' @param outcome Outcome variable.
#' @param predictors List of predictor variables to include in the model.
#' @param method Method for fitting regression (default \code{class} is for a factor variable, see \code{?rpart} for more details).
#' @param cost A vector of non-negative costs (one for each variable), see \code{?rpart} for more details.
#' @param minsplit Minimum number of observations in a node for a split to be attempted, see \code{?rpart.control} for more details.
#' @param xval Number of cross-validations, see \code{?rpart.control} for more details.
#' @param latex Produce LaTeX summary.
#' @param html Produce HTML summary.
#' @param ascii Produce ASCII summary.
#' @param caption Caption for table summary.
#' @param label Label for table summary.
#'
#' @return List containing the results and diagnostic plots of the LASSO fit.
#'
#' @export
rpart_dipep <- function(df          = dipep,
                        outcome     = 'group',
                        predictors  = c('age'),
                        method      = 'class',
                        cost        = NULL,
                        minsplit    = 20,
                        xval        = 10,
                        plot        = TRUE,
                        latex       = TRUE,
                        html        = FALSE,
                        ascii       = FALSE,
                        caption     = 'Recursive Partitioning for DiPEP',
                        label       = 'rpart',
                        ...){
    ## Initialise results list
    results <- list()
    ## Parse the equation without clustering
    if(is.na(cluster)){
        .formula <- reformulate(response   = outcome,
                                termlabels = predictors)
    }
    ## Select variables and create vector of outcome and matrix of predictors
    outcome    <- dplyr::select(df, outcome) %>%
                  as.vector()
    predictors <- dplyr::select(df, predictors) %>%
                  as.matrix()
    ## Fir recursive partitioning
    results$model <- rpart(data      = df,
                           formula   = .formula,
                           method    = method,
                           model     = TRUE,
                           minsplit  = minsplit,
                           xval      = xval)
    ## Generate a dendogram?
    if(plot == TRUE){
        results$ddata <- dendro_data(results$model)
        results$plot <- ggplot() +
                        geom_segment(data = results$ddata$segments,
                                     aes(x = x, xend = xend,
                                         y = y, yend = yend)) +
                        geom_text(data  = results$ddata$labels,
                                  aes(x = x,
                                      y = y,
                                      label = label),
                                  size  = 3,
                                  vjust = 2) +
                        geom_text(data  = results$ddata$leaf_labels,
                                  aes(x = x,
                                      y = y,
                                      label = label),
                                  size  = 3,
                                  vjust = 0) +
                        theme_dendro()
    }
    ## Summary of model fit
    if(latex == TRUE){
        results$cptable <- results$model$cptable %>%
                           xtable(caption = paste0("Optimal prunings from ",
                                                   caption),
                                  label   = paste0(label,
                                                   "-cptable"))
        results$splits <- results$model$splits %>%
                          xtable(caption = paste0("Splits from ",
                                                  caption),
                                 label   = paste0(label,
                                                  "-splits"))
        results$variable.importance <- results$model$variable.importance %>%
                                       data.frame() %>%
                                       xtable(caption = paste0("Variable Importance from ",
                                                               caption),
                                              label   = paste0(label,
                                                               "-var-import"))
    }
    return(results)
}
