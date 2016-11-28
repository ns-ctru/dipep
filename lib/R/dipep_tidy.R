#' Replaces concatenations of variables/levels to descriptive forms
#'
#' @description Replaces concatenations of variables/levels to descriptive forms
#'
#' @details
#'
#' When factor variables are included in regression models the functions return
#' a concatenation of the variable name and the levels in the results.  This function
#' replaces those with easier to read descriptive forms
#'
#'
#' @param df Data frame to analyse (default is \code{dipep} and shouldn't need changing)
#'
#'
#' @export
dipep_tidy <- function(df     = results,
                       ...){
    df <- mutate(term = sub('age', 'Age', term),
                 term = sub('smokingnever', 'Smoking - Never', term),
                 term = sub('smokinggave up prior to pregnancy', 'Smoking - Gave up Prior to Pregnancy', term),
                 term = sub('smokinggave up during pregnancy', 'Smoking - Gave up During Pregnancy', term),
                 term = sub('pregnancies.under', 'Pregnancies < 24 weeks (Continouous)', term),
                 term = sub('pregnancies.over', 'Pregnancies < 24 weeks (Continouous)', term),
                 term = sub('history.thrombosisYes', 'Family History of Thrombosis - Yes', term),
                 term = sub('history.thrombosisNo', 'Family History of Thrombosis - No', term),
                 term = sub('history.veinsYes', 'History of Varicose Veins - Yes', term),
                 term = sub('history.veinsNo', 'History of Varicose Veins - No', term),
                 term = sub('history.iv.drugYes', 'History of IV Drug Use - Yes', term),
                 term = sub('history.iv.drugNo', 'History of IV Drug Use - No', term),
                 term = sub('thromboYes', 'Known Thrombophilia - Yes', term),
                 term = sub('thromboNo', 'Known Thrombophilia - No', term),
                 term = sub('cesareanCesarean', 'Cesarean Delivery - Yes', term),
                 term = sub('cesareanNo Cesarean', 'Cesarean Delivery - No', term),
                 term = sub('thrombosisYes', 'History of Thrombosis - Yes', term),
                 term = sub('thrombosisNo', 'History of Thrombosis - No', term),
                 term = sub('injuryYes', 'Injury <4 Weeks - Yes', term),
                 term = sub('injuryNo', 'Injury <4 Weeks - No', term),
                 term = sub('trimester1st Trimester', 'First Trimester', term),
                 term = sub('trimester2nd Trimester', 'Second Trimester', term),
                 term = sub('trimester3rd Trimester', 'Third Trimester', term),
                 term = sub('multiple.pregYes', 'Multiple Pregnancy - Yes', term),
                 term = sub('multiple.pregNo', 'Multiple Pregnancy - No', term),
                 )
    return(df)
}
