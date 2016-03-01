#' Field Descriptions
#'
#' @description Descriptions of fields (variables) in the Dipep dataset
#'
#' @details
#' Variable names are sometimes informative, sometimes cryptic.  To avoid this each data frames
#' variables are described using the database specification produced by Data Management.
#'
#' @param df The data frame who's variables need describing
#' @param fields A data frame that contains the information from the database specification
#'               spreadsheet produced by Data Management. It should have a bare minimum of two
#'               columns the 'Identifier' column (normally column D) and the 'Label' column
#'               (usually column E) and these should be named \code{variable} and
#'               \code{description} respectively.
#'
#' @export
fields_dipep <- function(df      = master$follow.up.30.day,
                         fields  = fields){
    ## Get the dataframes variable names and merge with the fields data frame
    names.df <- names(df) %>%
        data.frame()
    names(names.df) <- c("variable")
    names.df <- merge(names.df,
                      fields,
                      by    = 'variable',
                      all.x = TRUE)
    ## Additional fields not documented (e.g. from forms/fields or derived variable)
    names.df$description[names.df$variable == 'event.name'] <- 'Event Name'
    names.df$description[names.df$variable == 'event.date'] <- 'Event Date'
    names.df$description[names.df$variable == 'form.name']  <- 'Form Name'
    names.df$description[names.df$variable == 'group']      <- 'Group (Suspected PE/)'
    names.df$description[names.df$variable == 'identifier'] <- 'identifier'
    return(names.df)
}

