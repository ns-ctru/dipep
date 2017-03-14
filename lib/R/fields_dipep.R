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
    names.df$description[names.df$variable == 'identifier'] <- 'identifier'
    names.df$description[names.df$variable == 'wells.alternative.permissive'] <- 'Wells  (Permissive) - Alternative Diagnosis'
    names.df$description[names.df$variable == 'wells.alternative.strict'] <- 'Wells (Strict) - Alternative Diagnosis'
    names.df$description[names.df$variable == 'wells.dvt'] <- 'Wells - Clinical DVT'
    names.df$description[names.df$variable == 'wells.dvt.pe'] <- 'Wells - Previous DVT/PE'
    names.df$description[names.df$variable == 'wells.haemoptysis'] <- 'Wells - Haemoptysis'
    names.df$description[names.df$variable == 'wells.heart.rate'] <- 'Wells - Heart Rate'
    names.df$description[names.df$variable == 'wells.immobil'] <- 'Wells - Immobility'
    names.df$description[names.df$variable == 'wells.neoplasm'] <- 'Wells - Neoplasm'
    names.df$description[names.df$variable == 'wells.permissive'] <- 'Wells (Permissive) - Score'
    names.df$description[names.df$variable == 'wells.permissive.pe'] <- 'Wells (Permissive) - Binary'
    names.df$description[names.df$variable == 'wells.permissive.risk'] <- 'Wells (Permissive) - Risk'
    names.df$description[names.df$variable == 'wells.strict'] <- 'Wells (Strict) - Score'
    names.df$description[names.df$variable == 'wells.strict.pe'] <- 'Wells (Strict)- Binary'
    names.df$description[names.df$variable == 'wells.strict.risk'] <- 'Wells (Strict) - Risk'
    names.df$description[names.df$variable == 'wells.surgery'] <- 'Wells - Surgery'
    names.df$description[names.df$variable == 'wells.surgery.immobil'] <- 'Wells - Surgery/Immobility'
    names.df$description[names.df$variable == 'simplified'] <- 'Simplified Revised Geneva - Score'
    names.df$description[names.df$variable == 'simplified.age'] <- 'Simplified Revised Geneva - Age'
    names.df$description[names.df$variable == 'simplified.haemoptysis'] <- 'Simplified Revised Geneva - Haemoptysis'
    names.df$description[names.df$variable == 'simplified.heart.rate'] <- 'Simplified Revised Geneva - Heart Rate'
    names.df$description[names.df$variable == 'simplified.lower.limb.unilateral.pain'] <- 'Simplified Revised Geneva - Lower Limb Unilateral Pain'
    names.df$description[names.df$variable == 'simplified.neoplasm'] <- 'Simplified Revised Geneva - Neoplasm'
    names.df$description[names.df$variable == 'simplified.pain.palpitations'] <- 'Simplified Revised Geneva - Pain on Leg Palpitations'
    names.df$description[names.df$variable == 'simplified.pe'] <- 'Simplified Revised Geneva - Pulmonary Embolism (Binary)'
    names.df$description[names.df$variable == 'simplified.prev.dvt.pe'] <- 'Simplified Revised Geneva - Previous DVT/PE'
    names.df$description[names.df$variable == 'simplified.risk'] <- 'Simplified Revised Geneva - Risk'
    names.df$description[names.df$variable == 'simplified.surgery'] <- 'Simplified Revised Geneva - Surgery'
    names.df$description[names.df$variable == 'perc'] <- 'PERC - Score'
    names.df$description[names.df$variable == 'perc.age'] <- 'PERC - Age'
    names.df$description[names.df$variable == 'perc.haemoptysis'] <- 'PERC - Haemoptysis'
    names.df$description[names.df$variable == 'perc.heart.rate'] <- 'PERC - Heart Rate'
    names.df$description[names.df$variable == 'perc.hormone'] <- 'PERC - Horomone Therapy'
    names.df$description[names.df$variable == 'perc.leg.swelling'] <- 'PERC - Unilateral Leg Swelling'
    names.df$description[names.df$variable == 'perc.o2'] <- 'PERC - O2 Saturation'
    names.df$description[names.df$variable == 'perc.pe'] <- 'PERC - Pulmonary Embolism (Binary)'
    names.df$description[names.df$variable == 'perc.prev.dvt.pe'] <- 'PERC - Previous DVT/PE'
    names.df$description[names.df$variable == 'perc.surgery'] <- 'PERC - Surgery'
    names.df$description[names.df$variable == 'delphi.primary'] <- 'Delphi (Primary) - Binary'
    names.df$description[names.df$variable == 'delphi.primary.bmi'] <- 'Delphi (Primary) - BMI > 30'
    names.df$description[names.df$variable == 'delphi.primary.clinical.dvt'] <- 'Delphi (Primary) - Clinical Signs of DVT'
    names.df$description[names.df$variable == 'delphi.primary.family.history'] <- 'Delphi (Primary) - Family History of Thrombosis'
    names.df$description[names.df$variable == 'delphi.primary.gestation'] <- 'Delphi (Primary) - Gestation'
    names.df$description[names.df$variable == 'delphi.primary.haemoptysis'] <- 'Delphi (Primary) - Haemoptysis'
    names.df$description[names.df$variable == 'delphi.primary.heart.rate.100.bpm'] <- 'Delphi (Primary) - Heart Rate > 100bpm'
    names.df$description[names.df$variable == 'delphi.primary.heart.rate.110.bpm'] <- 'Delphi (Primary) - Heart Rate > 110bpm'
    names.df$description[names.df$variable == 'delphi.primary.history.dvt.pe'] <- 'Delphi (Primary) - History of DVT/PE'
    names.df$description[names.df$variable == 'delphi.primary.history.iv.drug'] <- 'Delphi (Primary) - History of IV Drug Use'
    names.df$description[names.df$variable == 'delphi.primary.medical.complication'] <- 'Delphi (Primary) - Medical Complication'
    names.df$description[names.df$variable == 'delphi.primary.medical.history'] <- 'Delphi (Primary) - Medical History (admissions/surgery/significant injury)'
    names.df$description[names.df$variable == 'delphi.primary.o2.saturation'] <- 'Delphi (Primary) - O2 Saturation'
    names.df$description[names.df$variable == 'delphi.primary.obstetric.complication'] <- 'Delphi (Primary) - Obstetric Complications'
    names.df$description[names.df$variable == 'delphi.primary.pleuritic'] <- 'Delphi (Primary) - Pleuritic'
    names.df$description[names.df$variable == 'delphi.primary.respiratory.rate'] <- 'Delphi (Primary) - Respiratory Rate > 20 breaths/min'
    names.df$description[names.df$variable == 'delphi.primary.score'] <- 'Delphi (Primary) - Score'
    names.df$description[names.df$variable == 'delphi.primary.syncope'] <- 'Delphi (Primary) - Syncope'
    names.df$description[names.df$variable == 'delphi.sensitivity'] <- 'Delphi (Sensitivity) - Binary'
    names.df$description[names.df$variable == 'delphi.sensitivity.bmi'] <- 'Delphi (Sensitivity) - BMI > 30'
    names.df$description[names.df$variable == 'delphi.sensitivity.clinical.dvt'] <- 'Delphi (Sensitivity) - Clincal Signs of DVT'
    names.df$description[names.df$variable == 'delphi.sensitivity.family.history'] <- 'Delphi (Sensitivity) - Family History of Thrombosis'
    names.df$description[names.df$variable == 'delphi.sensitivity.gestation'] <- 'Delphi (Sensitivity) - Gestation'
    names.df$description[names.df$variable == 'delphi.sensitivity.haemoptysis'] <- 'Delphi (Sensitivity) - Haemoptysis'
    names.df$description[names.df$variable == 'delphi.sensitivity.heart.rate.100.bpm'] <- 'Delphi (Sensitivity) - Heart Rate > 100bpm'
    names.df$description[names.df$variable == 'delphi.sensitivity.heart.rate.110.bpm'] <- 'Delphi (Sensitivity) - Heart Rate > 110bpm'
    names.df$description[names.df$variable == 'delphi.sensitivity.history.dvt.pe'] <- 'Delphi (Sensitivity) - History of DVT/PE'
    names.df$description[names.df$variable == 'delphi.sensitivity.history.iv.drug'] <- 'Delphi (Sensitivity) - History of IV Drug Use'
    names.df$description[names.df$variable == 'delphi.sensitivity.medical.complication'] <- 'Delphi (Sensitivity) - Medical Complications'
    names.df$description[names.df$variable == 'delphi.sensitivity.medical.history'] <- 'Delphi (Sensitivity) - Medical History (admissions/surgery/significant injury)'
    names.df$description[names.df$variable == 'delphi.sensitivity.o2.saturation'] <- 'Delphi (Sensitivity) - O2 Saturation'
    names.df$description[names.df$variable == 'delphi.sensitivity.obstetric.complication'] <- 'Delphi (Sensitivity) - Obstetric Complication'
    names.df$description[names.df$variable == 'delphi.sensitivity.pleuritic'] <- 'Delphi (Sensitivity) - Pleuritic'
    names.df$description[names.df$variable == 'delphi.sensitivity.respiratory.rate'] <- 'Delphi (Sensitivity) - Respiratyr Rate > 20 breahs/min'
    names.df$description[names.df$variable == 'delphi.sensitivity.score'] <- 'Delphi (Sensitivity) - Score'
    names.df$description[names.df$variable == 'delphi.sensitivity.syncope'] <- 'Delphi (Sensitivity) - Syncope'
    names.df$description[names.df$variable == 'delphi.specificity'] <- 'Delphi (Specificity) - Binary'
    names.df$description[names.df$variable == 'delphi.specificity.bmi'] <- 'Delphi (Specificity) - BMI > 30'
    names.df$description[names.df$variable == 'delphi.specificity.clinical.dvt'] <- 'Delphi (Specificity) - Clinical Signs of DVT'
    names.df$description[names.df$variable == 'delphi.specificity.family.history'] <- 'Delphi (Specificity) - Family History of Thrombosis'
    names.df$description[names.df$variable == 'delphi.specificity.gestation'] <- 'Delphi (Specificity) - Gestation'
    names.df$description[names.df$variable == 'delphi.specificity.haemoptysis'] <- 'Delphi (Specificity) - Haemoptysis'
    names.df$description[names.df$variable == 'delphi.specificity.heart.rate.100.bpm'] <- 'Delphi (Specificity) - Heart Rate > 100bpm'
    names.df$description[names.df$variable == 'delphi.specificity.heart.rate.110.bpm'] <- 'Delphi (Specificity) - Heart Rate > 110bpm'
    names.df$description[names.df$variable == 'delphi.specificity.history.dvt.pe'] <- 'Delphi (Specificity) - History of DVT/PE'
    names.df$description[names.df$variable == 'delphi.specificity.history.iv.drug'] <- 'Delphi (Specificity) - History of IV Drug Use'
    names.df$description[names.df$variable == 'delphi.specificity.medical.complication'] <- 'Delphi (Specificity) - Medical Complications'
    names.df$description[names.df$variable == 'delphi.specificity.medical.history'] <- 'Delphi (Specificity) - Medical History (admissions/surgery/significant injury)'
    names.df$description[names.df$variable == 'delphi.specificity.o2.saturation'] <- 'Delphi (Specificity) - O2 Saturation'
    names.df$description[names.df$variable == 'delphi.specificity.obstetric.complication'] <- 'Delphi (Specificity) - Obstetric Complications'
    names.df$description[names.df$variable == 'delphi.specificity.pleuritic'] <- 'Delphi (Specificity) - Pleuritic'
    names.df$description[names.df$variable == 'delphi.specificity.respiratory.rate'] <- 'Delphi (Specificity) - Respiratory Rate > 20 breaths/min'
    names.df$description[names.df$variable == 'delphi.specificity.score'] <- 'Delphi (Specificity) - Score'
    names.df$description[names.df$variable == 'delphi.specificity.syncope'] <- 'Delphi (Specificity) - Syncope'
    return(names.df)
}
