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
        if(input$categorical == TRUE){
            reformulate(response = 'pe',
                    termlabels = c(input$demog.cat, input$presenting, input$history, input$current))
        }
        else{
            reformulate(response = 'pe',
                        termlabels = c(input$demog, input$presenting, input$history, input$current))
        }
    })
    ############################################################################
    ## Logistic Regression                                                    ##
    ############################################################################
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
    ## Fit the full regression tree
    rpart.fit.full <- reactive({
        rpart(formula  = model(),
              data   = dplyr::filter(dipep, group == 'Suspected PE'),
              method = 'class',
              minsplit  = input$minsplit,
              minbucket = input$minbucket)
    })
    ## Plot the full tree
    output$part.plot <- renderPlot({
        prp(rpart.fit.full(),
            type   = 4,
            extra  = 106,
            yesno  = 1,
            branch = 1,
            varlen = 0,
            faclen = 0)
    })
    output$part.cp.full <- renderPrint({
        rpart.fit.full() %>% printcp()
    })
    ## Prune the tree
    rpart.fit.prune <- reactive({
        prune(rpart.fit.full(), cp = input$cp)
    })
    output$pruned.plot <- renderPlot({
        prp(rpart.fit.prune(),
            type   = 4,
            extra  = 106,
            yesno  = 1,
            branch = 1,
            varlen = 0,
            faclen = 0)
    })
    output$part.cp.prune <- renderPrint({
        rpart.fit.prune() %>% printcp()
    })
    ## Generate ROC Curve
    output$part.roc <- renderPlot({
        ## Extract predicted variables, rename and add obs for binding
        predicted <- cbind(predict(rpart.fit.prune())[,2],
                           rpart.fit.prune()$y) %>%
                     as.data.frame()
        names(predicted) <- c('predicted', 'pe')
        roc <- ggplot(predicted, aes(d = pe, m = predicted)) +
               geom_roc() +
               guides(guide = guide_legend(title = 'Predictor...')) +
               ## ggtitle('ROC curve for multivariable logistic regression') +
               style_roc() + theme_bw()
        ## Calculate AUC + annotate plot
        auc <- calc_auc(roc)
        roc + annotate('text', x = 0.75, y = 0.25,
                       label = paste0('AUC = ', round(auc$AUC, input$digits)))
    })
    ############################################################################
    ## Regression - LASSO                                                     ##
    ############################################################################
    lasso.model <- reactive({
        glmnetUtils::glmnet(formula = model(),
                            data    = dplyr::filter(dipep, group == 'Suspected PE'),
                            family  = 'binomial')
    })
    output$lasso <- renderPrint({
        lasso.model() %>% print()
    })
    cv.lasso.model <- reactive({
        lasso <- glmnetUtils::cv.glmnet(formula = model(),
                                        data    = dplyr::filter(dipep, group == 'Suspected PE'),
                                        family  = 'binomial',
                                        nfolds  = nrow(dplyr::filter(dipep, group == 'Suspected PE')))
    })
    output$lasso.cv <- renderPrint({
        cv.lasso.model() %>% print()
    })
    output$lasso.plot <- renderPlot({
        autoplot(lasso.model()) + theme_bw()
    })
    output$lasso.cvplot <- renderPlot({
        autoplot(cv.lasso.model()) + theme_bw()
    })
    ############################################################################
    ## Regression - Saturated                                                 ##
    ############################################################################
    logistic.model <- reactive({
        saturated <- glm(data = dplyr::filter(dipep, group == 'Suspected PE'),
                         formula = model(),
                         family  = 'binomial')
    })
    output$logistic <- renderPrint({
        summary(logistic.model())
    })
    output$logistic.roc <- renderPlot({
        ## Extract predicted variables, rename and add obs for binding
        predicted <- cbind(logistic.model() %>% predict(),
                           logistic.model()$y) %>%
                     as.data.frame()
        names(predicted) <- c('predicted', 'pe')
        roc <- ggplot(predicted, aes(d = pe, m = predicted)) +
               geom_roc() +
               guides(guide = guide_legend(title = 'Predictor...')) +
               ## ggtitle('ROC curve for multivariable logistic regression') +
               style_roc() + theme_bw()
        ## Calculate AUC + annotate plot
        auc <- calc_auc(roc)
        roc + annotate('text', x = 0.75, y = 0.25,
                       label = paste0('AUC = ', round(auc$AUC, input$digits)))
    })
})
