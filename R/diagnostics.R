#**************************************************************
# diagnostics.R
# Model diagnostics: DHARMa residual simulation/tests, singularity
# & convergence flags, performance metrics, check_model-style
# residual plots, and Cook's/Mahalanobis distance plots per model.
#
# diagnostics_server() is called from server.R with the shared
# `runModels` reactive. (check_singularity_flag / check_convergence_flag
# live in utilities.R.)
#**************************************************************

diagnostics_server <- function(input, output, session, rv, runModels) {

  #------------------------------------------------------------------
  # DHARMa INTEGRATION for GLMM
  #------------------------------------------------------------------

  observe({
    results <- runModels()
    req(results)

    for (nm in names(results)) {
      local({
        safe_name <- make.names(nm)
        res <- results[[nm]]

        if (is.null(res$model)) return()

        model_obj <- unwrap_model(res$model)

        output[[paste0("dharma_", safe_name)]] <- renderPlot({

          if (!inherits(model_obj, c("glmerMod","lmerMod","lme"))) {
            plot.new()
            text(0.5,0.5,"DHARMa not available")
            return()
          }

          sim_res <- DHARMa::simulateResiduals(model_obj, n = 500)
          plot(sim_res)
        })

        output[[paste0("dharma_tests_", safe_name)]] <- renderPrint({

          model_obj <- unwrap_model(res$model)

          if (!inherits(model_obj, c("glmerMod","lmerMod"))) {
            cat("DHARMa tests not available\n")
            return()
          }

          sim_res <- simulateResiduals(model_obj, n = 500)

          cat("=== DHARMa Diagnostics ===\n")

          print(testUniformity(sim_res))
          print(testDispersion(sim_res))

          out_test <- testOutliers(sim_res)
          print(out_test)

          cat("\nNumber of outliers:", sum(out_test$outliers, na.rm = TRUE), "\n")

          if (family(model_obj)$family == "poisson") {
            print(testZeroInflation(sim_res))
          }
        })
      })
    }
  })

  #------------------------------------------------------------------
  # MODEL PERFORMANCE
  #------------------------------------------------------------------

  observe({
    results <- runModels()
    req(results)

    for (nm in names(results)) {
      local({

        safe_name <- make.names(nm)
        res <- results[[nm]]

        if (is.null(res$model)) return()

        model_obj <- unwrap_model(res$model)

        # ---- Diagnostic flags ----
        output[[paste0("diagnostic_flags_", safe_name)]] <- renderPrint({

          singular_flag <- check_singularity_flag(model_obj)
          convergence   <- check_convergence_flag(model_obj)

          cat("=== Diagnostic Flags ===\n")

          if (!is.na(singular_flag)) {
            cat(
              "Singular fit:",
              ifelse(singular_flag, "YES \u26a0\ufe0f", "NO \u2705"),
              "\n"
            )
          } else {
            cat("Singular fit: N/A (nlme)\n")
          }

          cat("Convergence:", convergence, "\n")
        })

        # ---- Performance metrics ----
        output[[paste0("perf_metrics_", safe_name)]] <- renderPrint({

          if (inherits(model_obj, "lme")) {
            data.frame(
              AIC    = AIC(model_obj),
              BIC    = BIC(model_obj),
              logLik = as.numeric(logLik(model_obj)),
              sigma  = model_obj$sigma,
              nobs   = nobs(model_obj)
            )
          } else {
            performance::model_performance(model_obj)
          }
        })

        output[[paste0("check_model_", safe_name)]] <- renderPlot({

          if (inherits(model_obj, "lme")) {
            #nlme::plot.lme(model_obj, resid(., type = "p") ~ fitted(.))
            #multilevelTools::modelDiagnostics(model_obj)

            predictmeans::residplot(
              model_obj,
              level = 1,
              id = FALSE,
              newwd = FALSE,
              ask = FALSE
            )
            #invisible(NULL)

          } else {
            performance::check_model(model_obj) #Update packages "patchwork" and "parameters" if there is plot window error
            #HLMdiag::residplot(model_obj, type = "all")
            # predictmeans::residplot(
            #   model_obj,
            #   level = 1,
            #   id = FALSE,
            #   newwd = FALSE,
            #   ask = FALSE
            # )

          }
        })
      })
    }
  })

  #------------------------------------------------------------------
  # PERFORMANCE UI
  #------------------------------------------------------------------

  output$performance_ui <- renderUI({
    results <- runModels()
    req(results)

    tabs <- lapply(names(results), function(nm) {

      tabPanel(
        nm,

        h4("Diagnostic flags"),
        verbatimTextOutput(
          outputId = paste0("diagnostic_flags_", make.names(nm))
        ),
        hr(),

        h4("Performance metrics"),
        verbatimTextOutput(
          outputId = paste0("perf_metrics_", make.names(nm))
        ),
        hr(),

        h4("Model checks"),
        plotOutput(
          outputId = paste0("check_model_", make.names(nm)),
          height = "900px"
        )
      )
    })

    do.call(tabsetPanel, tabs)
  })

  #------------------------------------------------------------------
  # UNIFIED DIAGNOSTICS UI (Cook's / Mahalanobis)
  #------------------------------------------------------------------

  observe({

    results <- runModels()
    req(results)

    df <- if (is.null(rv$cleaned_data)) rv$selected_data else rv$cleaned_data
    df_num <- df[sapply(df, is.numeric)]

    lapply(names(results), function(nm) {

      safe <- make.names(nm)

      # ---------------------------
      # Cook's Distance Plot
      # ---------------------------

      output[[paste0("cooks_", safe)]] <- renderPlot({

        try({

          req(ncol(df_num) > 1)

          formula <- as.formula(
            #paste(names(df_num)[1], "~", paste(names(df_num)[-1], collapse = "+"))
            paste(names(df_num)[1], "~", paste(names(df_num)[2], collapse = "+"))
            )

          model <- lm(formula, data = df_num)

          cooks_d <- cooks.distance(model)
          cutoff <- 4 / length(cooks_d)

          plot(cooks_d, type = "h",
             main = "Cook's Distance",
             sub = paste("Model:", deparse(formula)),
             ylab = "Distance", xlab = "Observation")

          abline(h = cutoff, lty = 2)

          # highlight outliers
            points(which(cooks_d > cutoff),
                   cooks_d[cooks_d > cutoff],
                   pch = 19)

          }, silent = TRUE)

        })

      # ---------------------------
      # Mahalanobis Plot
      # ---------------------------
      output[[paste0("mahal_", safe)]] <- renderPlot({

        try({

          req(ncol(df_num) > 1)

          df_complete <- na.omit(df_num)

          center <- colMeans(df_complete)
          cov_mat <- cov(df_complete)

          md <- mahalanobis(df_complete, center, cov_mat)

          cutoff <- qchisq(0.99, df = ncol(df_complete))

          plot(md,
               main = "Mahalanobis Distance",
               ylab = "Distance", xlab = "Observation")

          abline(h = cutoff, lty = 2)

          # highlight outliers
          outliers <- which(md > cutoff)
          points(outliers, md[outliers], pch = 19)

        }, silent = TRUE)

      })

    })
  })

  output$diagnostics_ui <- renderUI({

    results <- runModels()
    req(results)

    tabs <- lapply(names(results), function(nm) {

      safe <- make.names(nm)

      tabPanel(nm,
               h4("Cook's Distance [Default cutoff]"),
               plotOutput(paste0("cooks_", safe), height = "300px"),
               hr(),
               h4("Mahalanobis Distance [Default cutoff]"),
               plotOutput(paste0("mahal_", safe), height = "300px"),
               hr(),
               h4("DHARMa Residual Diagnostics"),
               plotOutput(paste0("dharma_", safe), height = "400px"),
               verbatimTextOutput(paste0("dharma_tests_", safe))
      )
    })

    do.call(tabsetPanel, tabs)
  })
}
