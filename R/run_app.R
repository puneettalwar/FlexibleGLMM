#**************************************************************
# run_app.R
# call FlexibleGLMM::run_app() instead of sourcing a script.
#**************************************************************

#' Launch the Flexible GLMM Toolbox Shiny app
#'
#' Fits GLMM models via afex/lme4/nlme with SAS-like output,
#' distribution fitting, diagnostics, and post-hoc comparisons.
#'
#' @param ... Additional arguments passed on to \code{shiny::shinyApp()},
#'   e.g. \code{options = list(launch.browser = TRUE)}.
#'
#' @return A Shiny app object (invisible when run interactively, since
#'   \code{shinyApp()} launches the app as a side effect).
#' @export
#'
#' @examples
#' if (interactive()) {
#'   run_app()
#' }
run_app <- function(...) {

  # Serve inst/app/www/ (e.g. GLMM_Table.jpg) at the "www/" URL path.
  # system.file() resolves correctly both in a dev install and once
  # the package is built/installed.
  shiny::addResourcePath(
    "www",
    system.file("app", "www", package = "FlexibleGLMM")
  )

  shiny::shinyApp(
    ui = app_ui,
    server = app_server,
    ...
  )
}
