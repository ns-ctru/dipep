#' Summarise Dipep variables
#'
#' @description Summary statistics for any given numerical variable
#'
#' @details
#' Many quantitative assessments are made, it is useful to know, the mean, standard deviation
#' (SD), median and inter-quartile range (IQR), minimum and maximum.  This funciton calculates
#' these for a given
#'
#'
#' @param df Data frame holding data
#' @param by Variable to group data by.
#' @param to.sum Variable to be summarised.
#' @param na.rm How to handle NAs (default is )
#' @param mean Logical indicator of whether to calculate the Mean
#'
#' @export
dipep_summarise <- function(df              = .data,
                            grouping        = 'group',
                            to.sum          = c('respiratory.rate', 'heart.rate'),
                            na.rm           = 'TRUE',
                            ## mean            = TRUE,
                            ## sd              = TRUE,
                            ## p25             = TRUE,
                            ## median          = TRUE,
                            ## p75             = TRUE,
                            ## min             = TRUE,
                            ## max             = TRUE,
                            ## missing         = TRUE,
                            ...){
    results <- list()
    ## Subset the data
    df <- dplyr::select_(df, .dots = c(grouping, to.sum))
    head(df) %>% print()
    mean(df$respiratory.rate, na.rm = TRUE) %>% print()
    ## Overall
    overall <- summarise_each_(df,
                               funs(mean(na.rm = TRUE),
                                    sd(na.rm = TRUE)),
                               .dots = to.sum)
                               ## vars(~to.sum))
    overall %>% print()
    ## By Group
    return(results)
}
