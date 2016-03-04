shinyUI(
    fluidPage(
        titlePanel("DiPEP - Predicting Pulmonary Embolism in Pregnant Women"),
        sidebarPanel(width = 2,
                     h2('Options...'),
                     p('Welcome, you are viewing the results of the DiPEP study which seeks to predict Pulmonary Embolism in Pregnant Women.'),
                     ## selectInput('hospital',
                     ##             'Hospital of Interest',
                     ##             choices = c('Bishop Auckland'  = 'bishop',
                     ##                         'Hartlepool'       = 'hartlepool',
                     ##                         'Hemel Hempstead'  = 'hemel',
                     ##                         'Newark'           = 'newark',
                     ##                         'Rochdale'         = 'rochdale',
                     ##                         'All'              = 'all'),
                     ##             selected = 'all',
                     ##             multiple = FALSE),
                     ## p('Select which condition to display for each Mortality and Emergency Admissions...'),
                     ## selectInput('mortality',
                     ##             'Mortality',
                     ##             choices  = c('Acute Heart Failure'                     = 'ahf',
                     ##                          'Anaphalaxis'                             = 'anaphalaxis',
                     ##                          'Asthma'                                  = 'asthma',
                     ##                          'Cardiac Arrest'                          = 'ca',
                     ##                          'Falls in those < 75'                     = 'falls',
                     ##                          'Fractured Neck of Femur'                 = 'femur',
                     ##                          'Meningitis'                              = 'meningitis',
                     ##                          'Myocradial Infarction'                   = 'mi',
                     ##                          'Non-superficial Head Injuries'           = 'head',
                     ##                          'Pregnancy and Birth Related Conditions'  = 'pregnancy',
                     ##                          'Road Traffic Accidents'                  = 'rta',
                     ##                          'Ruptured Aurtic Aneuyrsm'                = 'aneurysm',
                     ##                          'Self-harm'                               = 'selfharm',
                     ##                          'Septic Shock'                            = 'septicshock',
                     ##                          'Stroke/CVA'                              = 'stroke'),
                     ##             selected = 'mi',
                     ##             multiple = FALSE),
                     ## selectInput('admissions',
                     ##             'Emergency Admissions',
                     ##             choices  = c('Acute Mental Crisis'                    = 'mentalcrisis',
                     ##                          'Angina'                                 = 'angina',
                     ##                          'Blocked Urinary Catheter'               = 'urinarycatheter',
                     ##                          'Cellulitis'                             = 'cellulitis',
                     ##                          'Childhood Pyrexial'                     = 'pyrexial',
                     ##                          'Chronic Obstructive Pulmonary Disease'  = 'copd',
                     ##                          'Deep Vein Thrombosis'                   = 'dvt',
                     ##                          'Epileptic Seizure'                      = 'seizure',
                     ##                          'Falls in those > 75'                    = 'falls',
                     ##                          'Hypoglycemia'                           = 'hypoglycemia',
                     ##                          'Non-specific Abdominal Pains'           = 'abdominalpains',
                     ##                          'Non-specific Chest Pains'               = 'chestpaints',
                     ##                          'Superficial Head Injuries'              = 'head',
                     ##                          'Urinary Tract Infections'               = 'uti'),
                     ##             selected = 'mi',
                     ##             multiple = FALSE),
                     sliderInput('digits',
                                 'Decimal Places',
                                 min   = 1,
                                 max   = 5,
                                 value = 2)
                     ),
        mainPanel(width = 8,
                  height = 16,
                  column(12,
                         tabsetPanel(tabPanel('Clinical Opinion',
                                              tabsetPanel(tabPanel('PERC',
                                                                   p('Classification based on the PERC score (Kline et al. 2004)')),
                                                          tabPanel('Wells',
                                                                   p('Classification based on the Wells score (Wells et al. 2001)')),
                                                          tabPanel('Geneva',
                                                                   p('Classification based on the Geneva Score (Wicki et al. 2001)')),
                                                          tabPanel('Shortened Geneva',
                                                                   p('Classification based on the Geneva Score (Klok et al. 2008)')),
                                                          tabPanel('Delphi Consensus',
                                                                   p('Classification based on the Delphi Consensus of study members.')))
                                              ),
                                     tabPanel('Recursive Partitioning',
                                              p('Results of Recursive Partitioning')
                                              ),
                                     tabPanel('Regression',
                                              tabsetPanel(tabPanel('LASSO Regression',
                                                                   plotOutput(lasso.plot),
                                                                   renderTable(lasso.coef)
                                                                   ),
                                                          tabPanel('Saturated Regression',
                                                                   p('Whilst parsimony in modelling can be useful in situations such as the current scenario where the most accurate prediction is required it makes sense to use as much available information as possible rather than having a trade-off in the amount of information used and a reduction in accuracy as the subset of predictors is redcude by the LASSO.  Practically this makes sense too, in A&E clinicians will have all of the information available on which to make a decision, there seems little value in ignoring some of it when it can all be used to inform the decision of how to treat the patient.  It has even been indicated that there is a website (MD Calc) for calculating scores which removes all of the burden for individuals to have to remember and apply the rules.')))
                                              ),
                                     tabPanel('Biomarkers & Clinical Predictors',
                                              p('Summary tables.')
                                              ),
                                     tabPanel('Comparison of Methods',
                                              p('Comparing the different approaches'),
                                              p('Table comparing...'),
                                              HTML('<ul>
                                                      <li> PPV
                                                      <li> NPV
                                                      <li> Sensitivity
                                                      <li> Specificity
                                                      <li> Area under ROC
                                                    </ul>')
                                              ),
                                     tabPanel("ToDo",
                                              fluidRow(p("Everything needs doing, this list isn't exhaustive..."),
                                                       HTML("<ul>
                                                              <li> Data import and cleaning.
                                                              <li> Derivation of classification based on existing and new rules (refer to articles).
                                                              <li> Recursive Partitioning (dendogram in ggplot2; summary table).
                                                              <li> Regression (LASSO regression results; summary table).
                                                              <li> Summary table of biomarkers and other clinical predictor.
                                                              <li> Table comparing the utility (sensitivity, specificity, PPV, NPV, ROC AUC, pseudo-R2, etc.).
                                                              <li> Finish of References list (include R packages used)
                                                            </ul>"))
                                              ),
                                     tabPanel("References",
                                              fluidRow(h2('Pulmonary Embolism Classification'),
                                                       HTML('<ul>
                                                              <li> Leung A.N. <i>et al.</i> (2012) Evaluation of Suspected Pulmonary Embolism in Pregnancy. <i>Radiology</i>, <b>262</b>:635–646. ISSN 15271315. <a href="http://dx.doi.org/10.1148/radiol.11114045" target="_blank">DOI: 10.1148/radiol.11114045</a>.
                                                              <li> Klok F.A. <i>et al.</i> (2008) Simplification of the revised geneva score for assessing clinical probability of pulmonary embolism. <i>Archives of Internal Medicine</i> <b>168</b>:2131–2136 <a href="http://dx.doi.org/10.1001/archinte.168.19.2131" target="_blank">DOI:10.1001/archinte.168.19.2131</a>
                                                              <li> Wells P.S. <i>et al.</i> (2001) Excluding pulmonary embolism at the bedside without diagnostic imaging: management of patients with suspected pulmonary embolism presenting to the emergency department by using a simple clinical modeland d-dimer. <i>Annals of internal medicine</i> <b>135</b>:98–107 <a href="http://dx.doi.org/10.7326/0003-4819-135-2-200107170-00010" target="_blank"> DOI:10.7326/0003-4819-135-2-200107170-00010</a>
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
