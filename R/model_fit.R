#**************************************************************
# model_fit.R
# Core model-fitting logic. Builds and fits the GLMM(s) requested
# by the user (afex::mixed / lme4::glmer / nlme::lme), for either
# a custom equation or a loop over selected independent variables.
#
# model_fit_server() is called from server.R and returns the
# `runModels` eventReactive, which is shared by every downstream
# output module (model_summary, diagnostics, distributions,
# emmeans, plotting).
#**************************************************************

model_fit_server <- function(input, output, session, rv) {

  observe({
    rv$selected_data
    custom_eq_val <- if (is.null(input$custom_eq)) "" else input$custom_eq
    custom_mode <- nzchar(trimws(custom_eq_val))

    ignored_when_custom <- c(
      "y", "x", "covariates", "interaction_vars", "interaction_mode",
      "family", "linkfun", "engine", "corStruct",
      "random_effects", "nlme_random", "group_var", "time_var"
    )
    for (id in ignored_when_custom) {
      shinyjs::toggleState(id = id, condition = !custom_mode)
    }
  })

  runModels <- eventReactive(input$run, {
    #req(rv$selected_data, input$y, input$x, input$random_effects)
    req(rv$selected_data, input$y)

    # df <- if (!is.null(rv$data_no_outliers)) rv$data_no_outliers else
    #   if (!is.null(rv$cleaned_data)) rv$cleaned_data else rv$selected_data

    df <- if (!is.null(rv$processed_data))
      rv$processed_data
    else if (!is.null(rv$data_no_outliers))
      rv$data_no_outliers
    else if (!is.null(rv$cleaned_data))
      rv$cleaned_data
    else
      rv$selected_data

    family <- get_family(input$family, input$linkfun)

    custom_eq <- trimws(input$custom_eq)
    #mode <- input$interaction_mode

    results <- list()

    # --- CASE 1: Custom equation provided ---
    if (nzchar(custom_eq)) {
      tryCatch({
        parsed <- parse_custom_model_call(custom_eq)

        # Evaluate in a child of this function's own environment so the
        # call can see package-internal, NAMESPACE-imported functions
        # (mixed, glmer, lmer, lme, varIdent, corAR1, ...) exactly as
        # the rest of this file does, while `df` resolves to the app's
        # current working dataset - and nothing else the user might
        # have tried to reference.
        eval_env <- list2env(list(df = df), parent = environment())
        fit <- fit_with_diagnostics(eval(parsed$expr, envir = eval_env))
        if (inherits(fit$result, "FlexibleGLMM_fit_error")) stop(fit$result$error)
        model <- fit$result
        fit_warnings <- fit$warnings

        # ANOVA method is chosen from the fitted object's own class,
        # not a UI selector, since custom mode may not match
        # input$engine/input$family at all.
        anova_tab <- if (inherits(model, "mixed")) {
          tryCatch(anova(model, ddf = "Kenward-Roger", type = 3),
                   error = function(e) anova(model))
        } else {
          anova(model)
        }

        # Likewise, the Gamma-identity positivity check reads the
        # family/link back off the fitted model itself (nlme::lme
        # objects have no family() method and are skipped, correctly,
        # since lme is Gaussian-only).
        model_unwrapped <- unwrap_model(model)
        fam_info <- tryCatch(stats::family(model_unwrapped), error = function(e) NULL)
        if (!is.null(fam_info)) {
          check_gamma_identity_positivity(model, tolower(fam_info$family), fam_info$link)
        }

        results[[custom_eq]] <- list(
          engine = parsed$engine, formula = deparse(parsed$expr),
          model = model, anova = anova_tab, warnings = fit_warnings
        )
      }, error = function(e) {
        results[[custom_eq]] <- list(formula = custom_eq, error = e$message)
      })
    }

    # --- CASE 2: No custom equation, loop through IVs ---
    else {
      x_vars <- input$x
      covs <- input$covariates
      inters <- input$interaction_vars
      rand_terms <- ifelse(nzchar(input$random_effects), input$random_effects, "1")
      mode <- input$interaction_mode

      for (iv in x_vars) {
        rhs <- iv
        if (length(covs) > 0) rhs <- paste(rhs, "+", paste(covs, collapse = " + "))

        if (length(inters) > 0 && mode != "none") {
          if (mode == "iv_x_one") {
            rhs <- paste(rhs, "+", paste(paste0(iv, "*", inters), collapse = " + "))
          } else if (mode == "iv_x_two" && length(inters) >= 2) {
            rhs <- paste(rhs, "+", paste0(iv, "*", inters[1], "*", inters[2]))
          }
        }

        #f_str <- paste(input$y, "~", rhs, "+", rand_terms)
        if (input$engine == "nlme::lme") {
          f_str <- paste(input$y, "~", rhs)
        } else {
          #rand_terms <- ifelse(nzchar(input$random_effects), input$random_effects, "1")
          f_str <- paste(input$y, "~", rhs, "+", rand_terms)
        }
        f <- as.formula(f_str)

        tryCatch({
          fit_warnings <- character()

          if (input$engine == "afex::mixed") {

            # AFEX rule:
            # gaussian method = "KR"
            # non-gaussian family only (no link), method = "LRT"

            fam_name <- input$family

            if (fam_name == "gaussian") {
              fit <- fit_with_diagnostics(mixed(f, data = df, method = "KR"))
              if (inherits(fit$result, "FlexibleGLMM_fit_error")) stop(fit$result$error)
              model <- fit$result
              fit_warnings <- fit$warnings
              anova_tab <- anova(model, ddf = "Kenward-Roger", type = 3)
            } else {
              # Remove link (AFEX does NOT support custom links)
              base_family <- switch(
                fam_name,
                "gamma" = Gamma(),
                "binomial" = binomial(),
                "poisson" = poisson()
              )
              fit <- fit_with_diagnostics(mixed(f, data = df, family = base_family, method = "LRT"))
              if (inherits(fit$result, "FlexibleGLMM_fit_error")) stop(fit$result$error)
              model <- fit$result
              fit_warnings <- fit$warnings
              anova_tab <- anova(model)
            }
          }
          else if (input$engine == "lme4::glmer") {
            fit <- fit_with_diagnostics(glmer(f, data = df, family = family,
                                              control = glmerControl(optimizer = "bobyqa",
                                                                     optCtrl = list(maxfun = 2e5))))
            if (inherits(fit$result, "FlexibleGLMM_fit_error")) stop(fit$result$error)
            model <- fit$result
            fit_warnings <- fit$warnings
            anova_tab <- anova(model)
          } else if (input$engine == "nlme::lme") {

            if (input$family != "gaussian")
              stop("nlme::lme only supports Gaussian models.")

            # --- Random effects: nlme ONLY ---
            nlme_random <- trimws(input$nlme_random)

            random_formula <- if (nzchar(nlme_random)) {
              as.formula(nlme_random)
            } else {
              as.formula(paste0("~1|", input$group_var))
            }

            # --- Extract top-level subject ---
            subject_var <- get_nlme_subject(
              if (nzchar(nlme_random)) nlme_random else paste0("~1|", input$group_var)
            )

            df <- prepare_nlme_data(df, subject_var)

            # --- Correlation handling ---
            can_use_corr <- nlme_can_use_correlation(df, subject_var,input$time_var)

            suggested_corr <- suggest_correlation_structure(
              df,
              subject_var = subject_var,
              time_var = input$time_var
            )

            cor_choice <- input$corStruct
            if (cor_choice == "auto") cor_choice <- suggested_corr

            correlation <- NULL
            corr_used <- "none"

            if (can_use_corr && cor_choice != "none") {
              correlation <- get_nlme_corStruct(
                cor_choice,
                subject_var,
                input$time_var
              )
              corr_used <- cor_choice
            }

            if (!can_use_corr && cor_choice != "none") {
              showNotification(
                "Residual correlation disabled: insufficient repeated measures.",
                type = "warning"
              )
            }

            showNotification(
              paste("Correlation used:", corr_used),
              type = "message",
              duration = 4
            )

            # --- FIXED formula ONLY (no +1, no random terms) ---
            fixed_str <- strip_lme4_random(f_str)
            #fixed_str <- gsub("\\+\\s*1$", "", strip_lme4_random(f_str))

            fit <- fit_with_diagnostics(nlme::lme(
              fixed = as.formula(fixed_str),
              random = random_formula,
              correlation = correlation,
              data = df,
              method = "REML"
            ))
            if (inherits(fit$result, "FlexibleGLMM_fit_error")) stop(fit$result$error)
            model <- fit$result
            fit_warnings <- fit$warnings

            anova_tab <- anova(model)
          }

          check_gamma_identity_positivity(model, input$family, input$linkfun)
          results[[iv]] <- list(engine = input$engine, formula = f_str, model = model, anova = anova_tab, warnings = fit_warnings)

        }, error = function(e) {
          results[[iv]] <- list(formula = f_str, error = e$message)
        })
      }
    }

    rv$models <- results
    results
  })

  runModels
}
