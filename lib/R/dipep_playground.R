#' Test different models for their predictive accuracy.
#'
#' @description Shiny Server that provides a simple web-based UI to test Dipep models.
#'
#' @details
#'
#' This function starts a Shiny server which provides a web-based User Interface (UI)
#' to select different variables for testing their predictive accuracy of diagnosing
#' Pulmonary Embolisms (PE) in pregnant women.
#'
#' @export
dipep_playground <- function(){
    appDir <- system.file('shiny-examples', 'myapp', package = 'dipep')
    if(appDir == ''){
        stop('Could not find example directory.  Try re-installing the `dipep` package.',
             call. = FALSE)
    }
    shiny::runApp(appDir, display.mode = normal)
}
