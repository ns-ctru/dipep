#' Read DiPEP Files
#'
#' @description Read in the raw CSV files exported from Prospect for the DiPEP Study
#'
#' @details
#' Case Report Forms are stored in the Prospect Database (https://www.ctru-prospect.shef.ac.uk/)
#' and individual tables are exported as CSV files (see \code{README} in this directory for
#' export options).  These need reading into R for linking and analysing (it would be nice if
#' the relational database that underpins Prospect could be queried directly so as to avoid
#' reinventing the wheel but thats not going to happen in the foreseeable future if at all).
#'
#' When using this function the _first_ file that should be converted is 'Lookups.csv' as this
#' is the dictionary file that can be used to automatically encode all factor variables across
#' all subsequent files.
#'
#' @param file File to be imported.
#' @param header Header option (default \code{TRUE} shouldn't need changing).
#' @param sep Seperator used (default \code{,} (comma) shouldn't need changing).
#' @param convert.dates Logical indicator of whether to convert dates.
#' @param date.format Format dates are exported (suggest sticking to ISO8601 of \code{yyyy-mm-dd}).
#' @param dictionary Dictionary object.
#'
#' @export
read_dipep <- function(file            = 'Lookups.csv',
                       header          = TRUE,
                       sep             = ',',
                       convert.dates   = TRUE,
                       date.format     = 'ymd',
                       dictionary      = data.dictionary,
                       ...){
    # Read in the file
    new <- read.csv(file     = file,
                    header   = header,
                    sep      = sep)
    ## Lowercase and convert variable names
    names(new) <- gsub("_", ".", names(new)) %>%
        tolower()
    names(new) <- gsub("participant.no.", "screening", names(new))
    names(new) <- gsub("\\.o$", "", names(new))
    ## If this is the data dictionary convert '_' in field to '.' so that
    ## we can use it subsequently for labelling variables
    if(file == 'Lookups.csv'){
        new <- mutate(new,
                      field = gsub("_", ".", field))
    }
    ## Convert specified dates
    if(convert.dates == TRUE){
        ## Convert '_' to '.'
        ## convert.dates <- gsub('_', '.', convert.dates)
        for(x in grep('dt', colnames(new))){
            new[,x] <- ymd(new[,x])
        }
        for(x in grep('date', colnames(new))){
            new[,x] <- ymd(new[,x])
        }
    }
    ## Convert specified factors
    if(!is.null(dictionary)){
        ## Convert all variables...
        for(x in colnames(new)){
            ## ...but only if the variable is in the dictionary
            if(subset(dictionary, field == x) %>% nrow() > 0){
                new[[x]] <- factor(new[[x]],
                                   levels = c(subset(dictionary,
                                                     form  == file &
                                                     field == x))$code,
                                   labels = c(subset(dictionary,
                                                     form  == file &
                                                     field == x))$label)
            }
        }
    }
    return(new)
}
