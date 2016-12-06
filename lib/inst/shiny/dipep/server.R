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
                    termlabels = c(input$demog.cat, input$presenting, input$history))
        }
        else{
            reformulate(response = 'pe',
                        termlabels = c(input$demog, input$presenting, input$history))
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
              data   = filter(dipep, group == 'Suspected PE'),
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
    ############################################################################
    ## Regression - LASSO                                                     ##
    ############################################################################
    ## ToDo - Switch to glmnetUtils
    ## covars <- reactive({covar <- dplyr::filter(dipep, group == 'Suspected PE') %>%
    ##                         dplyr::select_(input$demog, input$presenting, input$current, input$history) %>%
    ##                         mutate(use = complete.cases(.)) %>%
    ##                         filter(use == TRUE) %>%
    ##                         dplyr::select(-use) %>%
    ##                         data.matrix()
    ## })
    ## status <- reactive({## ToDo - Switch to filtering based on actual variable and complete.cases()
    ##              status <- ifelse(runif(n = nrow(covars())) > 0.8, 1, 0) %>%
    ##                        factor(levels = c(0,1))
    ## })
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
    logistic.model <- reactive({
        saturated <- glm(data = filter(dipep, group == 'Suspected PE'),
                         formula = model(),
                         family  = 'binomial')
    })
    output$logistic <- renderPrint({
        summary(logistic.model())
    })
})
