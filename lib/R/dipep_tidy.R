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
                 term = sub('travelYes', 'Long Haul Travel (>4hrs) - Yes', term),
                 term = sub('travelNo', 'Long Haul Travel (>4hrs) - No', term),
                 term = sub('immobilYes', '>3 days Immobility - Yes', term),
                 term = sub('immobilNo', '>3 days Immobility - No', term),
                 term = sub('this.pregnancy.problemsYes', 'Problems with this Pregnancy - Yes', term),
                 term = sub('this.pregnancy.problemsNo', 'Problems with this  Pregnancy - No', term),
                 term = sub('prev.preg.problemYes', 'Problems with Previous Pregnancy - Yes', term),
                 term = sub('prev.preg.problemNo', 'Problems with Previous Pregnancy - No', term),
                 term = sub('presenting.features.pleuriticNot Ticked', 'Pleuritic - No', term),
                 term = sub('presenting.features.pleuriticTicked', 'Pleuritic - Yes', term),
                 term = sub('presenting.features.sob.exertionNot Ticked', 'Shortness of Breath (Exertion) - No', term),
                 term = sub('presenting.features.sob.exertionTicked', 'Shortness of Breath (Exertion) - Yes', term),
                 term = sub('presenting.features.sob.restNot Ticked', 'Shortness of Breath (Rest) - No', term),
                 term = sub('presenting.features.sob.restTicked', 'Shortness of Breath (Rest) - Yes', term),
                 term = sub('presenting.features.haemoptysisNot Ticked', 'Haemoptysis - No', term),
                 term = sub('presenting.features.haemoptysisTicked', 'Haemoptysis - Yes', term),
                 term = sub('presenting.features.syncopeNot Ticked', 'Syncope - No', term),
                 term = sub('presenting.features.syncopeTicked', 'Syncope - Yes', term),
                 term = sub('presenting.features.palpitationsNot Ticked', 'Palpitations - No', term),
                 term = sub('presenting.features.palpitationsTicked', 'Palpitations - Yes', term),
                 term = sub('presenting.features.otherNot Ticked', 'Other - No', term),
                 term = sub('presenting.features.otherTicked', 'Other - Yes', term),
                 term = sub('respiratory.rate', 'Respiratory Rate', term),
                 term = sub('heart.rate', 'Heart Rate', term),
                 term = sub('o2.saturation', 'Oxygen Saturation', term),
                 term = sub('bp.systolic', 'Systolic BP', term),
                 term = sub('bp.diastolic', 'Diastolic BP', term),
                 term = sub('ecgNot performed', 'ECG Not Performed', term),
                 term = sub('ecgNormal', 'ECG Normal', term),
                 term = sub('ecgAbnormal', 'ECG Abnormal', term),
                 )
    return(df)
}
