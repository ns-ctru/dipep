#' Classifies status with regards to Pulmonary Embolism
#'
#' @description Classifiy participants with regards to Pulmonary Embolism
#'
#' @details
#'
#' The study seeks to develop prediction rules for Pulmonary Embolism.
#' Case notes have been reviewed and individuals classified with regards
#' to whether they have Pulmonary Embolism (\code{PE}), No Pulmonary
#' Embolism (\code{No PE}) or are to be excluded (\code{Exclude}).  Four
#' variations of disease classification are required and all four classifers
#' are derived by this function.
#'
#'
#' @param df Data frame to classify there are three options \code{master$case.review#} where \code{#} denotes the reviewer of \code{1-3}
#' @param reviewer The reviewer number
#'
#' @export
dipep_case_review <- function(df       = master$case.review1,
                              reviewer = 1,
                              ...){
    df <- dplyr::select(df, screening, primary.class, secondary.class, img.class, trt.class, fup.class) %>%
          mutate(primary.class = as.character(primary.class),
                 secondary.class = as.character(secondary.class),
                 img.class       = as.character(img.class),
                 trt.class       = as.character(trt.class),
                 fup.class       = as.character(fup.class)) %>%
        mutate(first.st = case_when(.$img.class == 'I1a' & .$trt.class == 'T1' & .$fup.class == 'F1' ~ 'PE',
                                    .$img.class == 'I1a' & .$trt.class == 'T1' & .$fup.class == 'F2' ~ 'PE',
                                    .$img.class == 'I1a' & .$trt.class == 'T2' & .$fup.class == 'F1' ~ 'PE',
                                    .$img.class == 'I1a' & .$trt.class == 'T2' & .$fup.class == 'F2' ~ 'PE',
                                    .$img.class == 'I1b' & .$trt.class == 'T1' & .$fup.class == 'F1' ~ 'PE',
                                    .$img.class == 'I1b' & .$trt.class == 'T1' & .$fup.class == 'F2' ~ 'PE',
                                    .$img.class == 'I1b' & .$trt.class == 'T2' & .$fup.class == 'F1' ~ 'PE',
                                    .$img.class == 'I1b' & .$trt.class == 'T2' & .$fup.class == 'F2' ~ 'PE',
                                    .$img.class == 'I2'  & .$trt.class == 'T1' & .$fup.class == 'F1' ~ 'PE',
                                    .$img.class == 'I2'  & .$trt.class == 'T1' & .$fup.class == 'F2' ~ 'Exclude',
                                    .$img.class == 'I2'  & .$trt.class == 'T2' & .$fup.class == 'F1' ~ 'PE',
                                    .$img.class == 'I2'  & .$trt.class == 'T2' & .$fup.class == 'F2' ~ 'No PE',
                                    .$img.class == 'I3'  & .$trt.class == 'T1' & .$fup.class == 'F1' ~ 'PE',
                                    .$img.class == 'I3'  & .$trt.class == 'T1' & .$fup.class == 'F2' ~ 'No PE',
                                    .$img.class == 'I3'  & .$trt.class == 'T2' & .$fup.class == 'F1' ~ 'PE',
                                    .$img.class == 'I3'  & .$trt.class == 'T2' & .$fup.class == 'F2' ~ 'No PE',
                                    .$img.class == 'I4'  & .$trt.class == 'T1' & .$fup.class == 'F1' ~ 'PE',
                                    .$img.class == 'I4'  & .$trt.class == 'T1' & .$fup.class == 'F2' ~ 'Exclude',
                                    .$img.class == 'I4'  & .$trt.class == 'T2' & .$fup.class == 'F1' ~ 'PE',
                                    .$img.class == 'I4'  & .$trt.class == 'T2' & .$fup.class == 'F2' ~ 'Exclude'),
               second.st = case_when(.$img.class == 'I1a' & .$trt.class == 'T1' & .$fup.class == 'F1' ~ 'PE',
                                     .$img.class == 'I1a' & .$trt.class == 'T1' & .$fup.class == 'F2' ~ 'PE',
                                     .$img.class == 'I1a' & .$trt.class == 'T2' & .$fup.class == 'F1' ~ 'PE',
                                     .$img.class == 'I1a' & .$trt.class == 'T2' & .$fup.class == 'F2' ~ 'PE',
                                     .$img.class == 'I1b' & .$trt.class == 'T1' & .$fup.class == 'F1' ~ 'PE',
                                     .$img.class == 'I1b' & .$trt.class == 'T1' & .$fup.class == 'F2' ~ 'PE',
                                     .$img.class == 'I1b' & .$trt.class == 'T2' & .$fup.class == 'F1' ~ 'PE',
                                     .$img.class == 'I1b' & .$trt.class == 'T2' & .$fup.class == 'F2' ~ 'PE',
                                     .$img.class == 'I2'  & .$trt.class == 'T1' & .$fup.class == 'F1' ~ 'PE',
                                     .$img.class == 'I2'  & .$trt.class == 'T1' & .$fup.class == 'F2' ~ 'PE',
                                     .$img.class == 'I2'  & .$trt.class == 'T2' & .$fup.class == 'F1' ~ 'PE',
                                     .$img.class == 'I2'  & .$trt.class == 'T2' & .$fup.class == 'F2' ~ 'No PE',
                                     .$img.class == 'I3'  & .$trt.class == 'T1' & .$fup.class == 'F1' ~ 'PE',
                                     .$img.class == 'I3'  & .$trt.class == 'T1' & .$fup.class == 'F2' ~ 'No PE',
                                     .$img.class == 'I3'  & .$trt.class == 'T2' & .$fup.class == 'F1' ~ 'PE',
                                     .$img.class == 'I3'  & .$trt.class == 'T2' & .$fup.class == 'F2' ~ 'No PE',
                                     .$img.class == 'I4'  & .$trt.class == 'T1' & .$fup.class == 'F1' ~ 'PE',
                                     .$img.class == 'I4'  & .$trt.class == 'T1' & .$fup.class == 'F2' ~ 'PE',
                                     .$img.class == 'I4'  & .$trt.class == 'T2' & .$fup.class == 'F1' ~ 'PE',
                                     .$img.class == 'I4'  & .$trt.class == 'T2' & .$fup.class == 'F2' ~ 'Exclude'),
               third.st = case_when(.$img.class == 'I1a' & .$trt.class == 'T1' & .$fup.class == 'F1' ~ 'PE',
                                    .$img.class == 'I1a' & .$trt.class == 'T1' & .$fup.class == 'F2' ~ 'PE',
                                    .$img.class == 'I1a' & .$trt.class == 'T2' & .$fup.class == 'F1' ~ 'PE',
                                    .$img.class == 'I1a' & .$trt.class == 'T2' & .$fup.class == 'F2' ~ 'PE',
                                    .$img.class == 'I1b' & .$trt.class == 'T1' & .$fup.class == 'F1' ~ 'PE',
                                    .$img.class == 'I1b' & .$trt.class == 'T1' & .$fup.class == 'F2' ~ 'PE',
                                    .$img.class == 'I1b' & .$trt.class == 'T2' & .$fup.class == 'F1' ~ 'PE',
                                    .$img.class == 'I1b' & .$trt.class == 'T2' & .$fup.class == 'F2' ~ 'PE',
                                    .$img.class == 'I2'  & .$trt.class == 'T1' & .$fup.class == 'F1' ~ 'PE',
                                    .$img.class == 'I2'  & .$trt.class == 'T1' & .$fup.class == 'F2' ~ 'Exclude',
                                    .$img.class == 'I2'  & .$trt.class == 'T2' & .$fup.class == 'F1' ~ 'PE',
                                    .$img.class == 'I2'  & .$trt.class == 'T2' & .$fup.class == 'F2' ~ 'No PE',
                                    .$img.class == 'I3'  & .$trt.class == 'T1' & .$fup.class == 'F1' ~ 'PE',
                                    .$img.class == 'I3'  & .$trt.class == 'T1' & .$fup.class == 'F2' ~ 'No PE',
                                    .$img.class == 'I3'  & .$trt.class == 'T2' & .$fup.class == 'F1' ~ 'PE',
                                    .$img.class == 'I3'  & .$trt.class == 'T2' & .$fup.class == 'F2' ~ 'No PE',
                                    .$img.class == 'I4'  & .$trt.class == 'T1' & .$fup.class == 'F1' ~ 'PE',
                                    .$img.class == 'I4'  & .$trt.class == 'T1' & .$fup.class == 'F2' ~ 'Exclude',
                                    .$img.class == 'I4'  & .$trt.class == 'T2' & .$fup.class == 'F1' ~ 'PE',
                                    .$img.class == 'I4'  & .$trt.class == 'T2' & .$fup.class == 'F2' ~ 'No PE'),
               fourth.st = case_when(.$img.class == 'I1a' & .$trt.class == 'T1' & .$fup.class == 'F1' ~ 'PE',
                                     .$img.class == 'I1a' & .$trt.class == 'T1' & .$fup.class == 'F2' ~ 'PE',
                                     .$img.class == 'I1a' & .$trt.class == 'T2' & .$fup.class == 'F1' ~ 'PE',
                                     .$img.class == 'I1a' & .$trt.class == 'T2' & .$fup.class == 'F2' ~ 'PE',
                                     .$img.class == 'I1b' & .$trt.class == 'T1' & .$fup.class == 'F1' ~ 'Exclude',
                                     .$img.class == 'I1b' & .$trt.class == 'T1' & .$fup.class == 'F2' ~ 'Exclude',
                                     .$img.class == 'I1b' & .$trt.class == 'T2' & .$fup.class == 'F1' ~ 'Exclude',
                                     .$img.class == 'I1b' & .$trt.class == 'T2' & .$fup.class == 'F2' ~ 'Exclude',
                                     .$img.class == 'I2'  & .$trt.class == 'T1' & .$fup.class == 'F1' ~ 'PE',
                                     .$img.class == 'I2'  & .$trt.class == 'T1' & .$fup.class == 'F2' ~ 'Exclude',
                                     .$img.class == 'I2'  & .$trt.class == 'T2' & .$fup.class == 'F1' ~ 'PE',
                                     .$img.class == 'I2'  & .$trt.class == 'T2' & .$fup.class == 'F2' ~ 'No PE',
                                     .$img.class == 'I3'  & .$trt.class == 'T1' & .$fup.class == 'F1' ~ 'PE',
                                     .$img.class == 'I3'  & .$trt.class == 'T1' & .$fup.class == 'F2' ~ 'No PE',
                                     .$img.class == 'I3'  & .$trt.class == 'T2' & .$fup.class == 'F1' ~ 'PE',
                                     .$img.class == 'I3'  & .$trt.class == 'T2' & .$fup.class == 'F2' ~ 'No PE',
                                     .$img.class == 'I4'  & .$trt.class == 'T1' & .$fup.class == 'F1' ~ 'PE',
                                     .$img.class == 'I4'  & .$trt.class == 'T1' & .$fup.class == 'F2' ~ 'Exclude',
                                     .$img.class == 'I4'  & .$trt.class == 'T2' & .$fup.class == 'F1' ~ 'PE',
                                     .$img.class == 'I4'  & .$trt.class == 'T2' & .$fup.class == 'F2' ~ 'Exclude'))
    names(df) <- gsub('class',  paste0('class', reviewer), names(df))
    names(df) <- gsub('\\.st',  paste0('.st', reviewer), names(df))
    return(df)
}
