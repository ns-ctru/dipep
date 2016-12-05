shinyUI(
    fluidPage(
        titlePanel(title       = "DiPEP - Predicting Pulmonary Embolism in Pregnant Women",
                   windowTitle = "DiPEP - Predicting Pulmonary Embolism in Pregnant Women"),
        sidebarPanel(width = 3,
                     h2('Co-variates'),
                     p('Select co-variates to include in your model...'),
                     sliderInput('digits',
                                 'Decimal Places',
                                 min   = 1,
                                 max   = 5,
                                 value = 2),
                     checkboxInput('categorical', 'Use categorical variables?',
                                   value = FALSE
                                   ),
                     checkboxGroupInput('demog','Demographics (Continuous)...',
                                        choices = c('Age'                                         = 'age',
                                                    'Smoking'                                     = 'smoking',
                                                    'Temperature'                                 = 'temperature',
                                                    'Diastolic BP'                                = 'bp.diastolic',
                                                    'Systolic BP'                                 = 'bp.systolic',
                                                    'Oxygen Saturation'                           = 'o2.saturation',
                                                    'Respiratory Rate'                            = 'respiratory.rate',
                                                    'BMI'                                         = 'bmi'),
                                        selected = c('age', 'smoking', 'temperature', 'bp.diastolic',
                                                     'bp.systolic', 'o2.saturation', 'respiratory.rate')) ,
                     checkboxGroupInput('demog.cat','Demographics (Categorical)...',
                                        choices = c('Age (Binary)'                                = 'age.cat',
                                                    'Smoking (Binary)'                            = 'smoking.cat',
                                                    'Temperature (Binary)'                        = 'temperature.cat',
                                                    'Diastolic BP (Binary)'                       = 'bp.diastolic.cat',
                                                    'Systolic BP (Binary)'                        = 'bp.systolic.cat',
                                                    'Oxygen Saturation (Binary)'                  = 'o2.saturation.cat',
                                                    'Respiratory Rate (Binary)'                   = 'respiratory.rate.cat',
                                                    'BMI(Binary)'                                 = 'bmi.cat'),
                                        selected = c('age.cat', 'smoking.cat', 'temperature.cat',
                                                     'bp.diastolic.cat', 'bp.systolic.cat',
                                                     'o2.saturation.cat', 'respiratory.rate.cat', 'bmi.cat')
                                        ) ,
                     checkboxGroupInput('history', 'Medical History...',
                                        choices = c('Previous Pregnancy > 24 weeks' = 'pregnancies.over.cat',
                                                    'Previous Pregnancy < 24 weeks' = 'pregnancies.under.cat',
                                                    'History of Thrombosis in 1st-degree relatives' = 'history.thrombosis',
                                                    'History of Varicose Veins' = 'history.veins',
                                                    'History of IV Drug use' = 'history.iv.drug',
                                                    'History of Thrombosis'          = 'thrombosis'),
                                        selected = c('pregnancies.over.cat', 'pregnancies.under.cat',
                                                     'history.thrombosis', 'history.iv.drug', 'history.veins',
                                                     'thrombosis')
                                        ),
                     checkboxGroupInput('presenting','Presenting Features...',
                                        choices = c('Pleuritic'                      = 'presenting.features.pleuritic',
                                                    'Non-Pleuritic'                  = 'presenting.features.non.pleuritic',
                                                    'Shortness of Breath (Exertion)' = 'presenting.features.sob.exertion',
                                                    'Shortness of Breath (Rest)'     = 'presenting.features.sob.rest',
                                                    'Haemoptysis'                    = 'presenting.features.haemoptysis',
                                                    'Cough'                          = 'presenting.features.cough',
                                                    'Syncope'                        = 'presenting.features.syncope',
                                                    'Palpitations'                   = 'presenting.features.palpitations',
                                                    'Other'                          = 'presenting.features.other'),
                                        selected = c('presenting.features.pleuritic',
                                                     'presenting.features.non.pleuritic',
                                                     'presenting.features.sob.exertion',
                                                     'presenting.features.sob.rest',
                                                     'presenting.features.haemoptysis',
                                                     'presenting.features.cough',
                                                     'presenting.features.syncope',
                                                     'presenting.features.palpitations',
                                                     'presenting.features.other')
                                        ),
                     checkboxGroupInput('current', 'Current Pregnancy...',
                                        choices                                         = c('Multiple Pregnancy'  = 'multiple.preg',
                                                    'Cesarean'                          = 'cesarean',
                                                    'Injury'                            = 'injury',
                                                    'Trimester'                         = 'trimester',
                                                    'Known thrombophilia'               = 'thrombo',
                                                    'Existing Medical Problems'         = 'existing.medical',
                                                    'Long-haul travel during pregnancy' = 'travel',
                                                    '>= 3 days Immobility'              = 'immobil'),
                                        selected = c('multiple.preg', 'cesarean'))
                     ## checkboxGroupInput('history', 'Medical History...',
                     ##                    choices = c('Previous Pregnancy Problems'                 = 'prev.preg.problem',
                     ##                                'Pregnancies < 24 weeks'                      = 'pregnancies.under',
                     ##                                'Pregnancies > 24 weeks'                      = 'pregnancies.over'),
                     ##                    selected = c('prev.preg.problem'))
                     ),
        mainPanel(width = 7,
                  height = 16,
                  column(12,
                         tabsetPanel(type = 'pills',
                                     ## tabPanel('Clinical Opinion',
                                     ##          tabsetPanel(type = 'pills',
                                     ##                      tabPanel('PERC',
                                     ##                               p('Classification based on the PERC score (Kline et al. 2004)')),
                                     ##                      tabPanel('Wells',
                                     ##                               p('Classification based on the Wells score (Wells et al. 2001)')),
                                     ##                      tabPanel('Geneva',
                                     ##                               p('Classification based on the Geneva Score (Wicki et al. 2001)')),
                                     ##                      tabPanel('Shortened Geneva',
                                     ##                               p('Classification based on the Geneva Score (Klok et al. 2008)')),
                                     ##                      tabPanel('Delphi Consensus',
                                     ##                               p('Classification based on the Delphi Consensus of study members.')))
                                     ##          ),
                                     tabPanel('Recursive Partitioning',
                                              p('Results of Recursive Partitioning'),
                                              plotOutput('part.plot',
                                                         width  = '100%',
                                                         height = '100%')## ,
                                              ## verbatimTextOutput('part.cp')
                                              ),
                                     tabPanel('LASSO Regression',
                                              plotOutput('lasso.plot'),
                                              plotOutput('cv.lasso.plot')
                                              ),
                                     tabPanel('Saturated Regression',
                                              p(paste0('Whilst parsimony in modelling can be useful in situations such as the current scenario where the most accurate prediction is required it makes sense to use as much available information as possible rather than having a trade-off in the amount of information used and a reduction in accuracy as the subset of predictors is redcuded by the LASSO.  Practically this makes sense too, in A&E clinicians will have all of the information available on which to make a decision, and increasingly are not reliant on remembering a set of rules with the rise of software and applications to take the information and test it against a set of rules.  Therefore there seems little value in ignoring some of the information available by being parsimonious when it can all be used to inform the decision of how to treat the patient.  There is the website',
                                                       '<a href="http://www.mdcalc.com/" target="_blank">MD Calc</a>',
                                                       ') for calculating scores which removes all of the burden for individuals to have to remember and apply the rules.'))
                                              ),
                                     tabPanel('ToDo',
                                              fluidRow(p('Everything needs doing, this list is not exhaustive...'),
                                                       HTML('<ul>
                                                              <li> Recursive Partitioning (dendogram in ggplot2; summary table).
                                                              <li> LASSO regression results; summary table of coefficients.
                                                              <li> Saturated regression show all coefficients/model summary.
                                                              <li> Table comparing the utility (sensitivity, specificity, PPV, NPV, ROC AUC, pseudo-R2, etc.) of all models.
                                                              <li> Finish of References list (include R packages used)
                                                            </ul>'))
                                              ),
                                     tabPanel('References',
                                              fluidRow(h2('Pulmonary Embolism Classification'),
                                                       HTML('<ul>
                                                              <li> Leung A.N. <i>et al.</i> (2012) Evaluation of Suspected Pulmonary Embolism in Pregnancy. <i>Radiology</i>, <b>262</b>:635–646. ISSN 15271315. <a href="http://dx.doi.org/10.1148/radiol.11114045" target="_blank">DOI: 10.1148/radiol.11114045</a>.
                                                              <li> Klok F.A. <i>et al.</i> (2008) Simplification of the revised geneva score for assessing clinical probability of pulmonary embolism. <i>Archives of Internal Medicine</i> <b>168</b>:2131–2136 <a href="http://dx.doi.org/10.1001/archinte.168.19.2131" target="_blank">DOI:10.1001/archinte.168.19.2131</a>
                                                              <li> Wicki J. <i>et al.</i> (2001) Assessing clinical probability of pulmonary embolism in the emergency ward: a simple score. <i>Archives of Internal medicine</i> <b>161</b>:92–97 <a href="" target="_blank"> ???</a>
                                                              <li> Wells P.S. <i>et al.</i> (2001) Excluding pulmonary embolism at the bedside without diagnostic imaging: management of patients with suspected pulmonary embolism presenting to the emergency department by using a simple clinical model and d-dimer. <i>Annals of Internal Medicine</i> <b>135</b>:98–107 <a href="http://dx.doi.org/10.7326/0003-4819-135-2-200107170-00010" target="_blank"> DOI:10.7326/0003-4819-135-2-200107170-00010</a>
                                                              <li> Kline J. A.  <i>et al.</i> (2004) Clinical criteria to prevent unnecessary diagnostic testing in emergency department patients with suspected pulmonary embolism. <i>Journal of Thrombosis and Haemostasis</i> : <b>2</b>:1247–1255 <a href="http://dx.doi.org/10.1111/j.1538-7836.2004.00790.x" target="_blank">DOI: 10.1111/j.1538-7836.2004.00790.x</a>
                                                            </ul>'),
                                                       h2('Statistical Methods'),
                                                       HTML('<ul>
                                                              <li> Tibshirani R. (1996) Regression Shrinkage and Selection via the Lasso. <i>Journal of the Royal Statistical Society. Series B (Methodological)</i>, <b>58</b>:267–288. <a href="http://dx.doi.org/10.2307/2346178" target="_blank">DOI: 10.2307/2346178</a>
                                                              <li> Hastie T., Tibshirani R., Friedman J. (2003) <i>The Elements of Statistical Learning: Data Mining, Inference, and Prediction.</i> Springer, corrected edition ISBN 0387952845 <a href="http://statweb.stanford.edu/~tibs/ElemStatLearn/" target="_blank">PDF</a>.
                                                              <li> <a href="" target="_blank"></a>
                                                             </ul>'))
                                              )

                                     )
                         )
                  )
    )
)
