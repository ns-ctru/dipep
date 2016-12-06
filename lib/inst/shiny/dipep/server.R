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
        pred.obs <- cbind(predict(rpart.fit.prune(), type = 'prob'),
                          rpart.fit.prune()$y) %>%
                    as.data.frame()
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
    ##              status <- dplyr::filter(dipep, group == 'Suspected PE') %>%
    ##                        dplyr::select(pe)
    ## })
    ## glmnetUtils() not quite fully functional yet as it doesn't seem to like subsetting
    ## the data, so we do that manually now...
    lasso.df <- reactive({
        if(input$categorical == TRUE){
            dplyr::filter(dipep, group == 'Suspected PE') %>%
            dplyr::select_(input$demog.cat, input$presenting, input$history, input$current)
        }
        else{
            dplyr::filter(dipep, group == 'Suspected PE') %>%
            dplyr::select_(input$demog, input$presenting, input$history, input$current)
        }
    })
    lasso <- reactive({## Run LASSO
                ## lasso <- glmnet(x      = covars(),
                ##                 y      = status(),
        ##                 family = 'binomial')
        print('Guess we are about to complain...')
        head(dipep) %>% print()
        typeof(dipep) %>% print()
        dipep <- as.data.frame()
        head(dipep) %>% print()
        typeof(dipep) %>% print()
        lasso <- glmnetUtils::glmnet(data = dplyr::filter(dipep, group == 'Suspected PE'),
                                     formula = model(),
                                     family  = 'binomial')
        print('Nope made it past there')
    })
    cv.lasso <- reactive({## Run Cross validation
                   ## cv.lasso <- cv.glmnet(x      = covars(),
                   ##                       y      = status(),
                   ##                       family = 'binomial',
                   ##                       nfolds = length(status()))
        lasso <- glmnetUtils::cv.glmnet(data = lasso.df(),
                                        formula = model(),
                                        family  = 'binomial',
                                        nfolds  = nrow(lasso.df))
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
        saturated <- glm(data = dplyr::filter(dipep, group == 'Suspected PE'),
                         formula = model(),
                         family  = 'binomial')
    })
    output$logistic <- renderPrint({
        summary(logistic.model())
    })
    output$logistic.roc <- renderPlot({
        ## Extract predicted variables, rename and add obs for binding
        logistic.model()$y %>% print()
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
