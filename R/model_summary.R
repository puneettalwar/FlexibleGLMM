#**************************************************************
# model_summary.R
# Effect-size extraction, model/ANOVA text output, per-model
# summary tables, and the auto-generated HTML report download.
#
# model_summary_server() is called from server.R with the shared
# `runModels` reactive.
#**************************************************************

#----------------------
# Effect size calculation
#----------------------

extract_effect_sizes <- function(model) {

  model_unwrapped <- unwrap_model(model)
  out <- NULL

  # ---- Case 1: afex ----
  if (inherits(model, "mixed")) {

    aov_tab <- tryCatch(anova(model), error = function(e) NULL)

    if (!is.null(aov_tab)) {
      es <- tryCatch(
        effectsize::eta_squared(aov_tab, partial = TRUE),
        error = function(e) NULL
      )

      if (!is.null(es)) {
        out <- as.data.frame(es)
      }
    }
  }

  # ---- Case 2: lme4 / nlme ----
  else if (inherits(model_unwrapped, c("lmerMod","glmerMod","lme"))) {

    # ---- NEW: r2glmm EFFECT SIZES ----
    r2beta_res <- tryCatch(
      r2glmm::r2beta(model_unwrapped, method = "nsj", partial = TRUE),
      error = function(e) NULL
    )

    if (!is.null(r2beta_res)) {

      out <- as.data.frame(r2beta_res)

      # Clean column names for Shiny display
      colnames(out) <- gsub("\\.", "_", colnames(out))

    } else {
      message("r2glmm failed, falling back to standard parameters")

      # ---- fallback ----
      std_params <- tryCatch(
        parameters::standardize_parameters(model_unwrapped),
        error = function(e) NULL
      )

      if (!is.null(std_params) && nrow(std_params) > 0) {
        out <- as.data.frame(std_params)
      }
    }

    # ---- SAFE R2 ATTACH ----
    r2 <- tryCatch(
      performance::r2(model_unwrapped),
      error = function(e) NULL
    )

    if (!is.null(out) && !is.null(r2)) {
      attr(out, "R2") <- r2
    }
  }

  return(out)
}

#----------------------
# Coefficient / effect table extraction
#----------------------

extract_table <- function(model) {

  model_unwrapped <- unwrap_model(model)

  # ---- afex ----
  if (inherits(model, "mixed")) {

    tab <- as.data.frame(anova(model))
    tab$Effect <- rownames(tab)
    rownames(tab) <- NULL

    es <- tryCatch(
      effectsize::eta_squared(tab, partial = TRUE),
      error = function(e) NULL
    )

    if (!is.null(es)) {
      tab <- dplyr::left_join(tab, es, by.x = "Effect", by.y = "Parameter", all.x = TRUE)
    }

    return(tab)
  }

  # ---- lme4 / nlme ----
  else if (inherits(model_unwrapped, c("glmerMod","lmerMod","lme"))) {

    if (inherits(model_unwrapped, "lme")) {

      coefs <- summary(model_unwrapped)$tTable

      tab <- data.frame(
        term      = rownames(coefs),
        estimate  = coefs[, "Value"],
        std.error = coefs[, "Std.Error"],
        statistic = coefs[, "t-value"],
        p.value   = coefs[, "p-value"]
      )

    } else {

      tab <- broom.mixed::tidy(model_unwrapped, effects = "fixed")
    }

    r2 <- tryCatch(performance::r2(model_unwrapped), error = function(e) NULL)

    if (!is.null(r2)) {
      attr(tab, "R2") <- r2
    }

    # Standardized coefficients
    std <- tryCatch(
      parameters::standardize_parameters(model_unwrapped),
      error = function(e) NULL
    )

    if (!is.null(std) && nrow(std) > 0) {

      std <- std[, c("Parameter","Std_Coefficient","CI_low","CI_high")]
      names(std) <- c("term","Std_Beta","Std_CI_low","Std_CI_high")

      tab <- merge(tab, std, by = "term", all.x = TRUE)
    }

    return(tab)
  }

  NULL
}

format_table <- function(df) {
  df |>
    dplyr::mutate(
      dplyr::across(where(is.numeric), ~ round(.x, 4))
    )
}

#----------------------
# Server-side wiring
#----------------------

model_summary_server <- function(input, output, session, rv, runModels) {

  #----------------------
  # Model output
  #----------------------

  output$modelOutput <- renderPrint({
    results <- runModels()
    if (length(results) == 0) return("No models fitted yet.Check input variables and distribution")

    for (nm in names(results)) {
      res <- results[[nm]]

      cat("\n--- Model:", nm, "---\n")
      #cat("Formula:", res$formula, "\n")
      if (!is.null(res$formula)) {
        cat("Formula:", res$formula, "\n")
      }
      if (!is.null(res$fixed)) {
        cat("Engine:", res$engine, "\n")
        cat("Fixed:", res$fixed, "\n")
        cat("Random:", res$random, "\n")
        cat("Correlation:", res$correlation, "\n")
      }
      if (!is.null(res$model)) {
        print(summary(res$model))
        cat("\n=== Effect Sizes ===\n")

        es <- extract_effect_sizes(res$model)

        if (!is.null(es)) {
          print(es)

          r2 <- attr(es, "R2")
          if (!is.null(r2)) {
            cat("\n--- R-squared ---\n")
            print(r2)
          }
        } else {
          cat("Effect size not available for this model\n")
        }

        if (inherits(res$model, "lme")) {
          cat("\nCorrelation structure:\n")
          print(res$model$modelStruct$corStruct)

          r2 <- tryCatch(performance::r2(res$model), error = function(e) NULL)

          if (!is.null(r2)) {
            cat("\n--- R-squared ---\n")
            print(r2)
          }
        }

      } else {
        cat("Error:", res$error, "\n")
      }
    }
  })

  #----------------------
  # ANOVA output
  #----------------------

  output$anovaOutput <- renderPrint({
    results <- runModels()
    if (length(results) == 0) return("No ANOVA results available.")

    for (nm in names(results)) {
      cat("\n--- ANOVA for:", nm, "---\n")
      res <- results[[nm]]
      #cat("Formula:", res$formula, "\n")
      if (!is.null(res$formula)) {
        cat("Formula:", res$formula, "\n")
      }
      if (!is.null(res$fixed)) {
        cat("Engine:", res$engine, "\n")
        cat("Fixed:", res$fixed, "\n")
        cat("Random:", res$random, "\n")
        cat("Correlation:", res$correlation, "\n")
        print(res$anova)

      }

      if (!is.null(res$anova)) {
        print(res$anova)
      } else {
        cat("Error:", res$error, "\n")
      }
    }
  })

  #----------------------
  # Per-model summary tables (Summary Table tab)
  #----------------------

  observe({
    results <- runModels()
    req(results)

    for (nm in names(results)) {
      local({
        name <- nm
        res  <- results[[name]]

        if (is.null(res$model)) return()

        output[[paste0("summary_", make.names(name))]] <- renderUI({

          tab <- extract_table(res$model)
          req(tab, nrow(tab) > 0)

          tab <- dplyr::mutate(
            tab,
            dplyr::across(where(is.numeric), ~ round(.x, 4))
          )

          HTML(
            knitr::kable(
              tab,
              format  = "html",
              caption = paste("Model results:", name),
              align   = "l"
            ) |>
              kableExtra::kable_styling(
                bootstrap_options = c("striped", "hover", "condensed"),
                full_width = TRUE,
                font_size = 16
              )
          )
        })
      })
    }
  })

  output$summary_tables_ui <- renderUI({
    results <- runModels()
    req(results)

    tabs <- lapply(names(results), function(nm) {
      res <- results[[nm]]
      if (is.null(res$model)) return(NULL)

      tabPanel(
        nm,
        uiOutput(paste0("summary_", make.names(nm)))
      )
    })

    do.call(tabsetPanel, tabs)
  })

  #----------------------
  # AUTO-REPORT GENERATION
  #----------------------

  output$download_report <- downloadHandler(

    filename = function() {
      paste0("GLMM_report_", Sys.Date(), ".html")
    },

    content = function(file) {

      results <- runModels()
      req(results)

      # Use first model (can extend later)
      res <- results[[1]]

      model_obj <- unwrap_model(res$model)

      dharma_obj <- NULL
      if (inherits(model_obj, c("glmerMod","lmerMod"))) {
        dharma_obj <- simulateResiduals(model_obj)
      }

      rmarkdown::render(
        system.file("app", "report_template.Rmd", package = "FlexibleGLMM"),
        output_file = file,
        params = list(
          model = model_obj,
          anova = res$anova,
          formula = res$formula,
          engine = res$engine,
          family = input$family,
          dharma = dharma_obj
        ),
        envir = new.env(parent = globalenv())
      )
    }
  )
}
