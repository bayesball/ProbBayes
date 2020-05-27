ChooseBeta <- function() {
  appDir <- system.file("shiny-examples", "ChooseBetaPrior",
                        package = "ProbBayes")
  if (appDir == "") {
    stop("Could not find example directory. Try re-installing `ProbBayes`.", call. = FALSE)
  }

  shiny::runApp(appDir, display.mode = "normal")
}
