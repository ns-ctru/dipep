#' Summarise Dipep variables
#'
#' @description Summary statistics for any given numerical variable
#'
#' @details
#' Many quantitative assessments are made, it is useful to know, the mean, standard deviation
#' (SD), median and inter-quartile range (IQR), minimum and maximum.  This funciton calculates
#' these for a given grouing
#'
#'
#' @param df Data frame holding data
#' @param by Variable to group data by, options are \code{group} | \code{primary.class} | \code{secondary.class}.
#' @param to.sum Variable to be summarised.
#' @param group.as.col Logical as to whether to transpose resulting summary data frame to have groups as columns.
#' @param exclude Vector of \code{screening}  to exclude.
#' @param exclude.non.recuirted Logical indicator of whether to exclude \code{group == 'Non recruited'}.
#' @param exclude.dvt Logical indicator of whether to exclude \code{group == 'Diagnosed DVT'}.
#' @param exclude.anti.coag Logical indicator of whether to exclude individuals who had received anti-coagulents prior to blood samples being taken (default is \code{FALSE} and it is only relevant to set to \code{TRUE} when analysing certain biomarkers).#'
#'
#' @export
dipep_summarise <- function(df                    = dipep,
                            grouping              = 'group',
                            group.as.col          = FALSE,
                            exclude               = NULL,
                            exclude.non.recruited = TRUE,
                            exclude.dvt           = TRUE,
                            exclude.anti.coag     = FALSE,
                            ...){
    results <- list()
    ## Remove non-recruited and DVT
    if(exclude.non.recruited == TRUE){
        df <- dplyr::filter(df, group != 'Non recruited')
    }
    if(exclude.dvt == TRUE){
        df <- dplyr::filter(df, group != 'Diagnosed DVT')
    }
    ## Remove biomarker data for those on anticoagulents
    if(exclude.anti.coag == TRUE){
        df <- mutate(df,
                     prothombin.time                          = ifelse(exclude.anti.coag == 'Yes',
                                                                       yes = NA,
                                                                       no  = prothombin.time),
                     aprothombin                              = ifelse(exclude.anti.coag == 'Yes',
                                                                       yes = NA,
                                                                       no  = aprothombin),
                     clauss.fibrinogen                        = ifelse(exclude.anti.coag == 'Yes',
                                                                       yes = NA,
                                                                       no  = clauss.fibrinogen),
                     ddimer.innovan                           = ifelse(exclude.anti.coag == 'Yes',
                                                                       yes = NA,
                                                                       no  = ddimer.innovan),
                     ddimer.elisa                             = ifelse(exclude.anti.coag == 'Yes',
                                                                       yes = NA,
                                                                       no  = ddimer.elisa),
                     thrombin.generation.lag.time             = ifelse(exclude.anti.coag == 'Yes',
                                                                       yes = NA,
                                                                       no  = thrombin.generation.lag.time),
                     thrombin.generation.endogenous.potential = ifelse(exclude.anti.coag == 'Yes',
                                                                       yes = NA,
                                                                       no  = thrombin.generation.endogenous.potential),
                     thrombin.generation.peak                 = ifelse(exclude.anti.coag == 'Yes',
                                                                       yes = NA,
                                                                       no  = thrombin.generation.peak),
                     thrombin.generation.time.to.peak         = ifelse(exclude.anti.coag == 'Yes',
                                                                       yes = NA,
                                                                       no  = thrombin.generation.time.to.peak),
                     ddimer.elisa                             = ifelse(exclude.anti.coag == 'Yes',
                                                                       yes = NA,
                                                                       no  = ddimer.elisa),
                     plasmin.antiplasmin                      = ifelse(exclude.anti.coag == 'Yes',
                                                                       yes = NA,
                                                                       no  = plasmin.antiplasmin),
                     prothrombin.fragments                    = ifelse(exclude.anti.coag == 'Yes',
                                                                       yes = NA,
                                                                       no  = prothrombin.fragments),
                     soluble.tissue.factor                    = ifelse(exclude.anti.coag == 'Yes',
                                                                       yes = NA,
                                                                       no  = soluble.tissue.factor),
                     troponin                                 = ifelse(exclude.anti.coag == 'Yes',
                                                                       yes = NA,
                                                                       no  = troponin),
                     natriuertic.peptide                      = ifelse(exclude.anti.coag == 'Yes',
                                                                       yes = NA,
                                                                       no  = natriuertic.peptide),
                     mrproanp                                 = ifelse(exclude.anti.coag == 'Yes',
                                                                       yes = NA,
                                                                       no  = mrproanp))
    }
    ## Get the levels for the grouping
    observed.levels <- table(df[grouping], useNA = 'ifany') %>% names()
    observed.levels[is.na(observed.levels)] <- 'Exclude'
    ## Overall summary
    overall <- dplyr::select_(df, .dots = lazyeval::lazy_dots(...)) %>%
               summarise_each(funs(N      = sum(!is.na(.)),
                                   mean   = mean(., na.rm = TRUE),
                                   sd     = sd(., na.rm = TRUE),
                                   median = quantile(., probs = 0.5, na.rm = TRUE),
                                   p25    = quantile(., probs = 0.25, na.rm = TRUE),
                                   p75    = quantile(., probs = 0.75, na.rm = TRUE),
                                   min    = min(., na.rm = TRUE),
                                   max    = max(., na.rm = TRUE)))
    overall[grouping] <- 'All'
    ## Summarise by grouping
    results <- group_by_(df, grouping) %>%
               dplyr::select_(.dots = lazyeval::lazy_dots(...)) %>%
               summarise_each(funs(N      = sum(!is.na(.)),
                                   mean   = mean(., na.rm = TRUE),
                                   sd     = sd(., na.rm = TRUE),
                                   median = quantile(., probs = 0.5, na.rm = TRUE),
                                   p25    = quantile(., probs = 0.25, na.rm = TRUE),
                                   p75    = quantile(., probs = 0.75, na.rm = TRUE),
                                   min    = min(., na.rm = TRUE),
                                   max    = max(., na.rm = TRUE)))
    ## Combine Overall and by group
    results <- rbind(overall, results)
    ## Conditionally reshape the data
    if(group.as.col == TRUE){
        results <- results %>%
                   t() %>%
                   data.frame() %>%
                   mutate(Statistic = row.names(.),
                          Measurement = gsub('_.*', '', Statistic),
                          Measurement = gsub('age', 'Age', Measurement),
                          Measurement = gsub('bmi', 'BMI', Measurement),
                          Measurement = gsub('heart.rate', 'Heart Rate', Measurement),
                          Measurement = gsub('bp.systolic', 'Systolic BP', Measurement),
                          Measurement = gsub('bp.diastolic', 'Diastolic BP', Measurement),
                          Measurement = gsub('height', 'Height', Measurement),
                          Measurement = gsub('weight', 'Weight', Measurement),
                          Measurement = gsub('respiratory.rate', 'Respiratory Rate', Measurement),
                          Measurement = gsub('o2.saturation', 'Oxygen Saturation', Measurement),
                          Measurement = gsub('gestation', 'Gestation', Measurement),
                          Measurement = gsub('aprothombin', 'Aprothombin', Measurement),
                          Measurement = gsub('prothombin.time', 'Prothombin (Time)', Measurement),
                          Measurement = gsub('clauss.fibrinogen', 'Clauss Fibrinogen', Measurement),
                          Measurement = gsub('ddimer.innovan', 'D-Dimer (Innovan)', Measurement),
                          Measurement = gsub('ddimer.elisa', 'D-Dimer (ELISA)', Measurement),
                          Measurement = gsub('thrombin.generation.lag.time', 'Thrombin Generation (Lag Time)', Measurement),
                          Measurement = gsub('thrombin.generation.endogenous.potential', 'Thrombin Generation (Endogenous Potential)', Measurement),
                          Measurement = gsub('thrombin.generation.peak', 'Thrombin Generation (Peak)', Measurement),
                          Measurement = gsub('thrombin.generation.time.to.peak', 'Thrombin Generation (Time to Peak)', Measurement),
                          Measurement = gsub('plasmin.antiplasmin', 'Plasmin (Antiplasmin)', Measurement),
                          Measurement = gsub('prothrombin.fragments', 'Prothombin Fragments', Measurement),
                          Measurement = gsub('soluble.tissue.factor', 'Soluble Tissue Factor', Measurement),
                          Measurement = gsub('troponin', 'Troponin', Measurement),
                          Measurement = gsub('natriuretic.peptide', 'Natriuertic Peptide', Measurement),
                          Measurement = gsub('mrproanp', 'MRproANP', Measurement),
                          Statistic = gsub('.*_', '', Statistic),
                          Statistic = gsub('mean', 'Mean', Statistic),
                          Statistic = gsub('sd', 'SD', Statistic),
                          Statistic = gsub('median', 'Median', Statistic),
                          Statistic = gsub('p25', 'Lower Quartile', Statistic),
                          Statistic = gsub('p75', 'Upper Quartile', Statistic),
                          Statistic = gsub('min', 'Min', Statistic),
                          Statistic = gsub('max', 'Max', Statistic)) %>%
            dplyr::filter(Statistic != grouping)
        ## Rename and order rows and columns
        names(results) <- c('All', observed.levels, 'Statistic', 'Measurement')
        results <- mutate(results,
                          order1 = 1,
                          order1 = ifelse(Statistic == 'N'              , 1, order1),
                          order1 = ifelse(Statistic == 'Mean'           , 2, order1),
                          order1 = ifelse(Statistic == 'SD'             , 3, order1),
                          order1 = ifelse(Statistic == 'Lower Quartile' , 4, order1),
                          order1 = ifelse(Statistic == 'Median'         , 5, order1),
                          order1 = ifelse(Statistic == 'Upper Quartile' , 6, order1),
                          order1 = ifelse(Statistic == 'Min'            , 7, order1),
                          order1 = ifelse(Statistic == 'Max'            , 8, order1)) %>%
                   arrange(Measurement, order1) %>%
                   dplyr::select(-order1)
        refcols <- c('Measurement', 'Statistic')
        results <- results[, c(refcols, setdiff(names(results), refcols))]
    }
    return(results)
}
