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
                      field = gsub("_", ".", field),
        ## 2017-04-26 - Something has gone awry (perhaps inclusion of the Case Review in Lookups.csv now?)
        ##              But there are now forms with the same field (30 day follow-up.csv and each of the
        ##              case reviews).  This messes up the labelling as there are then multiple definitions
        ##              of the levels, so rename the field to be prefixed with cr. in the case review files.
                      field = ifelse(form == 'Case Review 1',
                                     yes = paste0("cr1.", field),
                                     no  = field),
                      field = ifelse(field %in% c('cr1.primary.class', 'cr1.secondary.class', 'cr1.img.class', 'cr1.trt.class', 'cr1.fup.class'),
                                     yes = gsub('cr1\\.', '', field),
                                     no  = field),
                      field = ifelse(form == 'Case Review 2',
                                     yes = paste0("cr2.", field),
                                     no  = field),
                      field = ifelse(field %in% c('cr2.primary.class', 'cr2.secondary.class', 'cr2.img.class', 'cr2.trt.class', 'cr2.fup.class'),
                                     yes = gsub('cr2\\.', '', field),
                                     no  = field),
                      field = ifelse(form == 'Case Review 3',
                                     yes = paste0("cr3.", field),
                                     no  = field),
                      field = ifelse(field %in% c('cr3.primary.class', 'cr3.secondary.class', 'cr3.img.class', 'cr3.trt.class', 'cr3.fup.class'),
                                     yes = gsub('cr3\\.', '', field),
                                     no  = field),
                      form = gsub("Woman's details", "Womans details", form))

    }
    ## 2017-04-26 - Following on if we are reading the case reviews we need to rename the fields
    keep.same.names <- c('screening',
                         'site',
                         'site.code',
                         'group',
                         'event.name',
                         'event.date',
                         'form.name',
                         'primary.class',
                         'secondary.class',
                         'img.class',
                         'trt.class',
                         'fup.class')
    if(file == 'Case Review 1.csv'){
        names(new) <- ifelse((names(new) %in% keep.same.names),
                             yes = names(new),
                             no  = paste0('cr1.', names(new)))
    }
    if(file == 'Case Review 2.csv'){
        names(new) <- ifelse((names(new) %in% keep.same.names),
                             yes = names(new),
                             no  = paste0('cr2.', names(new)))
    }
    if(file == 'Case Review 3.csv'){
        names(new) <- ifelse((names(new) %in% keep.same.names),
                             yes = names(new),
                             no  = paste0('cr3.', names(new)))
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
                ## Now need to pull out the form name from the full filename if its one of the following
                if(file == 'Case Review 1.csv')                 file <- 'Case Review 1'
                if(file == 'Case Review 1 - Investigation.csv') file <- 'Case Review 1'
                if(file == 'Case Review 2.csv')                 file <- 'Case Review 2'
                if(file == 'Case Review 2 - Investigation.csv') file <- 'Case Review 2'
                if(file == 'Case Review 3.csv')                 file <- 'Case Review 3'
                if(file == 'Investigations - Investigation.csv') file <- 'Investigations'
                if(file == 'Medical History - Medical problems.csv') file <- 'Medical History'
                if(file == 'Medical History - Thrombophilia.csv') file <- 'Thrombophilia'
                if(file == 'Previous pregnancies - Previous pregnancy problems.csv') file <- 'Previous pregnancies'
                if(file == 'This Pregnancy continued - Problems during this pregnancy.csv') file <- 'This Pregnancy continued'
                if(file == 'This Pregnancy continued - Immobility.csv') file <- 'This Pregnancy continued'
                if(file == 'This Pregnancy continued - Long-haul travel.csv') file <- 'This Pregnancy continued'
                if(file == "Woman's details.csv") file <- 'Womans details'
                if(file == "Womans_details_all.csv") file <- 'Womans details'
                ## print(x)
                ## print(file)
                ## table(new[[x]]) %>% print()
                new[[x]] <- factor(new[[x]],
                                   levels = c(subset(dictionary,
                                                     form  == gsub('\\.csv', '', file) &
                                                     field == x))$code,
                                   labels = c(subset(dictionary,
                                                     form  == gsub('\\.csv', '', file) &
                                                     field == x))$label)
                ## table(new[[x]]) %>% print()
            }
        }
    }
    return(new)
}
