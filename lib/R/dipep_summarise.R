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
#' @param exclude.missing Exclude individuals flagged as having excessive missing data.
#'
#' @export
dipep_summarise <- function(df                    = dipep,
                            grouping              = 'group',
                            group.as.col          = FALSE,
                            exclude               = NULL,
                            exclude.non.recruited = TRUE,
                            exclude.dvt           = TRUE,
                            exclude.anti.coag     = FALSE,
                            exclude.missing       = FALSE,
                            ...){
    results <- list()
    ## Remove non-recruited, DVT and/or missing
    if(exclude.non.recruited == TRUE){
        df <- dplyr::filter(df, group != 'Non recruited')
    }
    if(exclude.dvt == TRUE){
        df <- dplyr::filter(df, group != 'Diagnosed DVT')
    }
    else{
        df <- df %>%
            mutate(first.st  = as.character(first.st),
                   second.st = as.character(second.st),
                   third.st  = as.character(third.st),
                   fourth.st = as.character(fourth.st)) %>%
            mutate(first.st = case_when(is.na(.$first.st) & substr(.$screening, 1, 1) == 'D' ~ 'DVT',
                                        is.na(.$first.st) & substr(.$screening, 1, 1) != 'D' ~ 'Exclude',
                                        !is.na(.$first.st)                                   ~ .$first.st),
                   second.st = case_when(is.na(.$second.st) & substr(.$screening, 1, 1) == 'D' ~ 'DVT',
                                         is.na(.$second.st) & substr(.$screening, 1, 1) != 'D' ~ 'Exclude',
                                         !is.na(.$second.st)                                   ~ .$second.st),
                   third.st = case_when(is.na(.$third.st) & substr(.$screening, 1, 1) == 'D' ~ 'DVT',
                                        is.na(.$third.st) & substr(.$screening, 1, 1) != 'D' ~ 'Exclude',
                                        !is.na(.$third.st)                                   ~ .$third.st),
                   fourth.st = case_when(is.na(.$fourth.st) & substr(.$screening, 1, 1) == 'D' ~ 'DVT',
                                         is.na(.$fourth.st) & substr(.$screening, 1, 1) != 'D' ~ 'Exclude',
                                         !is.na(.$fourth.st)                                   ~ .$fourth.st))
    }
    if(exclude.missing == TRUE){
        df <- dplyr::filter(df, missing.exclude == FALSE)
    }
    ## Remove biomarker data for those on anticoagulents
    if(exclude.anti.coag == TRUE){
        df <- dplyr::filter(df, exclude.anti.coag == 'No')
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
                          Measurement = gsub('temperature', 'Temperature', Measurement),
                          Measurement = gsub('heart.rate', 'Heart Rate', Measurement),
                          Measurement = gsub('bp.systolic', 'Systolic BP', Measurement),
                          Measurement = gsub('bp.diastolic', 'Diastolic BP', Measurement),
                          Measurement = gsub('height', 'Height', Measurement),
                          Measurement = gsub('weight', 'Weight', Measurement),
                          Measurement = gsub('respiratory.rate', 'Respiratory Rate', Measurement),
                          Measurement = gsub('o2.saturation', 'Oxygen Saturation', Measurement),
                          Measurement = gsub('gestation', 'Gestation', Measurement),
                          Measurement = gsub('aptt', 'APTT', Measurement),
                          Measurement = gsub('aptt.cat', 'APTT : Dichotomised', Measurement),
                          Measurement = gsub('prothombin.time', 'Prothrombin Time', Measurement),
                          Measurement = gsub('prothombin.time.cat', 'Prothrombin Time : Dichotomised', Measurement),
                          Measurement = gsub('clauss.fibrinogen', 'Clauss Fibrinogen', Measurement),
                          Measurement = gsub('clauss.fibrinogen.cat', 'Clauss Fibrinogen : Dichotomised', Measurement),
                          Measurement = gsub('ddimer.innovance', 'D-Dimer (Innovance)', Measurement),
                          Measurement = gsub('ddimer.innovance.cat', 'D-Dimer (Innovance) : Dichotomised', Measurement),
                          Measurement = gsub('ddimer.elisa', 'D-Dimer (ELISA)', Measurement),
                          Measurement = gsub('ddimer.elisa.cat', 'D-Dimer (ELISA) : Dichotomised', Measurement),
                          Measurement = gsub('d.dimer', 'D-Dimer (Hospital)', Measurement),
                          Measurement = gsub('d.dimer.cat', 'D-Dimer (Hospital) : Dichotomised', Measurement),
                          Measurement = gsub('thrombin.generation.lag.time', 'Thrombin Generation (Lag Time)', Measurement),
                          Measurement = gsub('thrombin.generation.lag.time.cat', 'Thrombin Generation (Lag Time) : Dichotomised', Measurement),
                          Measurement = gsub('thrombin.generation.endogenous.potential', 'Thrombin Generation (Endogenous Potential)', Measurement),
                          Measurement = gsub('thrombin.generation.endogenous.potential.cat', 'Thrombin Generation (Endogenous Potential) : Dichotomised', Measurement),
                          Measurement = gsub('thrombin.generation.peak', 'Thrombin Generation (Peak)', Measurement),
                          Measurement = gsub('thrombin.generation.peak.cat', 'Thrombin Generation (Peak) : Dichotomised', Measurement),
                          Measurement = gsub('thrombin.generation.time.to.peak', 'Thrombin Generation (Time to Peak)', Measurement),
                          Measurement = gsub('thrombin.generation.time.to.peak.cat', 'Thrombin Generation (Time to Peak) : Dichotomised', Measurement),
                          Measurement = gsub('plasmin.antiplasmin', 'Plasmin-antiplasmin', Measurement),
                          Measurement = gsub('plasmin.antiplasmin.cat', 'Plasmin-antiplasmin : Dichotomised', Measurement),
                          Measurement = gsub('prothrombin.fragments', 'PF 1 + 2', Measurement),
                          Measurement = gsub('prothrombin.fragments.cat', 'PF 1 + 2 : Dichotomised', Measurement),
                          Measurement = gsub('tissue.factor', 'Tissue Factor', Measurement),
                          Measurement = gsub('tissue.factor.cat', 'Tissue Factor : Dichotomised', Measurement),
                          Measurement = gsub('troponin', 'Troponin', Measurement),
                          Measurement = gsub('troponin.cat', 'Troponin : Dichotomised', Measurement),
                          Measurement = gsub('bnp', 'BNP', Measurement),
                          Measurement = gsub('bnp.cat', 'BNP : Dichotomised', Measurement),
                          Measurement = gsub('mrproanp', 'MRProANP', Measurement),
                          Measurement = gsub('mrproanp.cat', 'MRProANP : Dichotomised', Measurement),
                          Measurement = gsub('crp', 'C-Reactive Protein', Measurement),
                          Measurement = gsub('crp.cat', 'C-Reactive Protein : Dichotomised', Measurement),
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
