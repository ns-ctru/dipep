# Load libraries
library(dplyr)
library(ggplot2)
library(glmnet)
library(lars)
library(magrittr)
library(rpart)
library(shiny)
library(RColorBrewer)
# Define Server Output
shinyServer(function(input, output){
    ############################################################################
    ## Set up the model                                                       ##
    ############################################################################
    model <- reactive({
        reformulate(response = 'pe',
                    termlabels = c(input$demog, input$presenting, input$history))
    })
    ############################################################################
    ## Logistic Regression                                                    ##
    ############################################################################
    output$logistic <- renderText({
                           glm(formula = pe ~ age,
                               data = filter(dipep, group == 'Suspected PE'),
                               family = binomial(link = 'logit')) %>%
                           summary()
    })
    ############################################################################
    ## Clinical decision rules                                                ##
    ############################################################################
    ## Table - PERC  (Kline et al. 2004)
    ## Table - Wells (Wells et al. 2001)
    ## Table - Geneva (Wicki et al. 2001)
    ## Table - Shortened Geneva (Klok et al. 2008)
    ## Table - Delphi Consensus
    ############################################################################
    ## Recurrsive Partitioning                                                ##
    ############################################################################
    fitted <- reactive({
                  dipep$pe <- ifelse(runif(n = nrow(dipep)) > 0.7, 1, 0)
                  dipep$pe <- factor(dipep$pe,
                                     levels = c(0, 1),
                                     labels = c('No PE', 'PE'))
                  fitted <- rpart(formula  = model(),
                                  data   = filter(dipep, group == 'Suspected PE'),
                                  method = 'class')
    })
    output$part.plot <- renderPlot({
                            prp(fitted(),
                                type   = 4,
                                extra  = 106,
                                yesno  = 1,
                                branch = 1,
                                varlen = 0,
                                faclen = 0)
    })
    output$part.cp <- renderText({
                          printcp(fitted())
    })
    ############################################################################
    ## Regression - LASSO                                                     ##
    ############################################################################
    covars <- reactive({covar <- dplyr::filter(dipep, group == 'Suspected PE') %>%
                            dplyr::select_(input$demog, input$presenting, input$current, input$history) %>%
                            mutate(use = complete.cases(.)) %>%
                            filter(use == TRUE) %>%
                            dplyr::select(-use) %>%
                            data.matrix()
    })
    status <- reactive({## ToDo - Switch to filtering based on actual variable and complete.cases()
                 status <- ifelse(runif(n = nrow(covars())) > 0.8, 1, 0) %>%
                           factor(levels = c(0,1))
    })
    lasso <- reactive({## Run LASSO
                lasso <- glmnet(x      = covars(),
                                y      = status(),
                                family = 'binomial')
    })
    cv.lasso <- reactive({## Run Cross validation
                   cv.lasso <- cv.glmnet(x      = covars(),
                                         y      = status(),
                                         family = 'binomial',
                                         nfolds = length(status()))
    })
    output$lasso.plot <- renderPlot({
        autoplot(lasso()) + theme_bw()
    })
    output$lasso.cvplot <- renderPlot({
        autoplot(cv.lasso()) + theme_bw()
    })
    ############################################################################
    ## Regression - Saturated                                                 ##
    ############################################################################
    ## Model
    ## Plot?
    ## Table
    model <- reformulate(response = 'pe',
                         termlabels = c('age.cat', 'bmi.cat', 'smoking.cat', 'pregnancies.over.cat', 'pregnancies.under.cat',
                                       'history.thrombosis', 'history.veins', 'history.iv.drug', 'thrombo', 'cesarean',
                                       'injury', 'thrombosis', 'existing.medical', 'preg.post', 'trimester', 'multiple.preg',
                                       'travel', 'immobil', 'this.pregnancy.problems', 'prev.preg.problem',
                                       'presenting.features.pleuritic', 'presenting.features.non.pleuritic',
                                       'presenting.features.sob.exertion', 'presenting.features.sob.rest',
                                       'presenting.features.haemoptysis', 'presenting.features.cough',
                                       'presenting.features.syncope', 'presenting.features.palpitations',
                                       'presenting.features.other', 'respiratory.rate.cat', 'heart.rate',
                                       'o2.saturation.cat', 'bp.systolic.cat', 'bp.diastolic.cat', 'ecg.cat', 'xray.cat'))
    output$saturated.model <- renderPrint(
        saturated <- glm(data = filter(dipep, group == 'Suspected PE'),
                         formula = model(),
                         family  = 'binomial')
    )
})
