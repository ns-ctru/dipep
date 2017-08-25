#' Summarise by observation the completeness of data
#'
#' @description Summarise by observation the completeness of subsets of data
#'
#' @details
#'
#' It is desirable to know how much missing data there is for subsets of variables
#' for each individual
#'
#'
#' @param df Data frame.
#' @param group Variable to group by.
#' @param to.check to be assessed for missing.
#'
#' @export
dipep_incomplete <- function(df              = .data,
                             group           = group
                             ...){
    ## results <- list()
    ## Subset the data and calculate the number of missing obs for each individual
    results <- dplyr::select_(df, .dots = lazyeval::lazy_dots(group, ...)) %>%
               mutate(n_missing = rowSums(is.na(.)))
    return(results)
}
