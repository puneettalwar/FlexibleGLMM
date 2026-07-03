#**************************************************************
# distributions.R
# Distribution fitting (fitdistrplus) for the selected dependent
# variable: fits Normal/Lognormal/Gamma/Weibull and renders the
# associated logs, comparison plots, and goodness-of-fit stats.
#
# distributions_server() is called from server.R.
#**************************************************************

distributions_server <- function(input, output, session, rv) {

  #------------------------------------------------------------------
  # Distribution fitting that runs ONLY for the selected dependent variable
  #------------------------------------------------------------------

  observeEvent(input$fit_distribution, {

    df <- isolate(
      if (is.null(rv$data_no_outliers))
        if (is.null(rv$cleaned_data)) rv$selected_data else rv$cleaned_data
      else rv$data_no_outliers
    )

    varname <- input$y
    z <- df[[varname]]

    if (!is.numeric(z)) {
      output$fitStatus <- renderText("Dependent variable must be numeric.")
      return(NULL)
    }

    z1 <- na.exclude(z)
    if (length(z1) < 5) {
      output$fitStatus <- renderText("Not enough numeric data to fit distributions.")
      return(NULL)
    }

    # ---- Fit 4 Standard Distributions ----
    fn  <- fitdist(z1, "norm",   method = "mme")
    fg  <- fitdist(z1, "gamma",  method = "mme")
    fln <- fitdist(z1, "lnorm",  method = "mme")
    fw  <- fitdist(z1, "weibull", method = "mle")

    rv$dist_fits <- list(
      Normal    = fn,
      Lognormal = fln,
      Gamma     = fg,
      Weibull   = fw
    )

    # Log output for FitDist Output tab
    logs <- c(
      paste0("===== Dependent Variable: ", varname, " ====="),
      "",
      "---- Normal ----",
      paste(capture.output(summary(fn)), collapse = "\n"),
      "",
      "---- Lognormal ----",
      paste(capture.output(summary(fln)), collapse = "\n"),
      "",
      "---- Gamma ----",
      paste(capture.output(summary(fg)), collapse = "\n"),
      "",
      "---- Weibull ----",
      paste(capture.output(summary(fw)), collapse = "\n"),
      "",
      "---- Goodness-of-fit ----",
      paste(capture.output(gofstat(rv$dist_fits)), collapse = "\n")
    )

    output$fitDistLogs <- renderText(paste(logs, collapse="\n"))
    output$fitStatus   <- renderText("Distribution fitting complete.")

  })

  output$dist_descriptive <- renderPlot({
    req(rv$dist_fits, input$y)

    # Get currently selected y from the same data source
    df <- if (!is.null(rv$data_no_outliers)) rv$data_no_outliers else
      if (!is.null(rv$cleaned_data)) rv$cleaned_data else rv$selected_data

    z <- suppressWarnings(as.numeric(df[[input$y]]))
    z <- na.exclude(z)

    if (length(z) < 5) return(NULL)

    descdist(z, boot = 500)
  })


  output$dist_denscomp <- renderPlot({
    req(rv$dist_fits)
    denscomp(rv$dist_fits, legendtext=c("normal","lognormal","gamma","weibull"))
  })

  output$dist_qqcomp <- renderPlot({
    req(rv$dist_fits)
    qqcomp(rv$dist_fits, legendtext=c("normal","lognormal","gamma","weibull"))
  })

  output$dist_cdfcomp <- renderPlot({
    req(rv$dist_fits)
    cdfcomp(rv$dist_fits, legendtext=c("normal","lognormal","gamma","weibull"))
  })

  output$dist_ppcomp <- renderPlot({
    req(rv$dist_fits)
    ppcomp(rv$dist_fits, legendtext=c("normal","lognormal","gamma","weibull"))
  })

  output$dist_gof <- renderPrint({
    req(rv$dist_fits)
    gofstat(rv$dist_fits)
  })
}
