#' Tidy and plot Recursive Partitioning Model
#'
#' @description Tidy and plot Recursive Partitioning Model
#'
#' @details
#'
#' Recursive Partitioning using the \code{rpart} package is performed, this
#' helper function tidies the results and plots them.
#'
#' @param df Data frame of fitted \code{rpart} model
#'
#' @return List containing the results and diagnostic plots of the LASSO fit.
#'
#' @export
rpart_tidy_dipep <- function(df          = strict,
                             ...){
    ## Initialise results list
    results <- list()
    ## Generate ddata
    results$ddata <- dendro_data(df)
    ## Tidy the labels
    results$ddata$labels <- mutate(results$ddata$labels,
                                   label = gsub('age', 'Age ', label),
                                   label = gsub('temperature.cat', 'Temperature (B) ', label),
                                   label = gsub('smoking', 'Smoking ', label),
                                   label = gsub('bp.systolic.cat', 'Systolic BP (Categorised)  ', label),
                                   label = gsub('bp.diastolic.cat', 'Diastolic BP (Categorised) ', label),
                                   label = gsub('respiratory.rate.cat', 'Respiratory Rate (Categorised) ', label),
                                   label = gsub('bmi.cat', 'BMI (Categorised)', label),
                                   label = gsub('multiple.preg', 'Multiple Pregnancy ', label),
                                   label = gsub('o2.saturation.cat', 'O2 Saturation (Categorised) ', label),
                                   label = gsub('ecg', 'Echo-Cardiogram ', label),
                                   label = gsub('thromb.event', ' ', label),
                                   label = gsub('injury', 'Injury ', label),
                                   label = gsub('thrombosis', 'Thrombosis ', label),
                                   label = gsub('preg.post', 'Partum ', label),
                                   label = gsub('pregnancies.under', 'Pregnancies <24 weeks ', label),
                                   label = gsub('pregnancies.over', 'Pregnancies >24 weeks ', label),
                                   label = gsub('prev.preg.problem', 'Previous Pregnancy Problems ', label),
                                   label = gsub('presenting.features.pleuritic', 'Pleuritic ', label),
                                   label = gsub('presenting.features.non.pleuritic', 'Non-Pleuritic ', label),
                                   label = gsub('presenting.features.sob.exertion', 'Shortness of Breath (Exertion) ', label),
                                   label = gsub('presenting.features.sob.rest', 'Shortness of Breath (Rest) ', label),
                                   label = gsub('presenting.features.haemoptysis', 'Haemoptysis ', label),
                                   label = gsub('presenting.features.cough', 'Cough ', label),
                                   label = gsub('presenting.features.syncope', 'Syncope ', label),
                                   label = gsub('presenting.features.palpitations', 'Palpitations ', label),
                                   label = gsub('presenting.features.other', 'Other ', label)
                                   )
    ## Generate a dendogram
    results$ddata <- dendro_data(df)
    results$dendrogram <- ggplot() +
                          geom_segment(data = results$ddata$segments,
                                       aes(x = x, xend = xend,
                                           y = y, yend = yend)) +
                          geom_text(data  = results$ddata$labels,
                                    aes(x = x,
                                        y = y,
                                        label = label),
                                    size  = 4,
                                    vjust = -1.5) +
                          geom_text(data  = results$ddata$leaf_labels,
                                    aes(x = x,
                                        y = y,
                                        label = label),
                                    size  = 4,
                                    vjust = 1) +
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
