#' Runs Shiny web-server for display the results
#'
#' @export
shiny_results <- function() {
    # locate all the shiny app examples that exist
  appDir <- system.file("shiny", "dipep", package = "dipep")
  if (appDir == "") {
    stop("Could not find example directory. Try re-installing `mypackage`.", call. = FALSE)
  }

  shiny::runApp(appDir, display.mode = "normal")
}
