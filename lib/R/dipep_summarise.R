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
#'
#' @export
dipep_summarise <- function(df              = dipep,
                            grouping        = 'group',
                            group.as.col    = FALSE,
                            ...){
    results <- list()
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
                          Statistic = gsub('.*_', '', Statistic),
                          Statistic = gsub('mean', 'Mean', Statistic),
                          Statistic = gsub('sd', 'SD', Statistic),
                          Statistic = gsub('median', 'Median', Statistic),
                          Statistic = gsub('p25', 'Lower Quartile', Statistic),
                          Statistic = gsub('p75', 'Upper Quartile', Statistic),
                          Statistic = gsub('min', 'Min', Statistic),
                          Statistic = gsub('max', 'Max', Statistic)) %>%
            dplyr::filter(Statistic != grouping)
        ## Rename and order
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
    }
    return(results)
}
