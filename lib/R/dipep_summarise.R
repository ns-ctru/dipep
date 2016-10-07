#' Summarise Dipep variaables
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
                            grouping        = group,
                            to.sum          = respiratory.rate,
                            na.rm           = TRUE,
                            mean            = TRUE,
                            sd              = TRUE,
                            p25             = TRUE,
                            median          = TRUE,
                            p75             = TRUE,
                            min             = TRUE,
                            max             = TRUE,
                            missing         = TRUE,
                            ...){
    results <- list()
    if(mean == TRUE){
        results$mean <- mean(df$to.sum,
                             na.rm = na.rm)
        results$mean_diagnosed_dvt <- filter(df, group == 'Diagnosed DVT') %>% mean(to.sum, na.rm = na.rm)
        results$mean_non_recruited <- filter(df, group == 'Non recruited') %>% mean(to.sum, na.rm = na.rm)
        results$mean_suspected_pe  <- filter(df, group == 'Suspected PE')  %>% mean(to.sum, na.rm = na.rm)
    }
    if(sd == TRUE){
        results$sd <- sd(df$to.sum,
                         na.rm = na.rm)
        results$sd_diagnosed_dvt <- filter(df, group == 'Diagnosed DVT') %>% sd(to.sum, na.rm = na.rm)
        results$sd_non_recruited <- filter(df, group == 'Non recruited') %>% sd(to.sum, na.rm = na.rm)
        results$sd_suspected_pe  <- filter(df, group == 'Suspected PE')  %>% sd(to.sum, na.rm = na.rm)
    }
    ## if(p25 == TRUE){
    ##     p25 <- group_by_(df, grouping) %>%
    ##            summarise(p25 = quantile(to.sum,
    ##                                     probs = c(0.25),
    ##                                     na.rm = na.rm))
    ## }
    ## if(p50 == TRUE){
    ##     p50 <- group_by_(df, grouping) %>%
    ##            summarise(p50 = quantile(to.sum,
    ##                                     probs = c(0.50),
    ##                                     na.rm = na.rm))
    ## }
    ## if(p75 == TRUE){
    ##     p75 <- group_by_(df, grouping) %>%
    ##            summarise(p75 = quantile(to.sum,
    ##                                     probs = c(0.25),
    ##                                     na.rm = na.rm))
    ## }
    ## if(min == TRUE){
    ##     min <- group_by_(df, grouping) %>%
    ##            summarise(min = min(to.sum,
    ##                                na.rm = na.rm))
    ## }
    ## if(max == TRUE){
    ##     max <- group_by_(df, grouping) %>%
    ##            summarise(max = max(to.sum,
    ##                                na.rm = na.rm))
    ## }
    summary <- rbind(mean,
                     sd,
                     p25,
                     p50,
                     p75,
                     min,
                     max)
    summary$measurement <- to.sum
    return(summary)
}
