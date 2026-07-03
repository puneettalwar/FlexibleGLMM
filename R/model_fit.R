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

  runModels <- eventReactive(input$run, {
    #req(rv$selected_data, input$y, input$x, input$random_effects)
    req(rv$selected_data, input$y)
    df <- if (!is.null(rv$data_no_outliers)) rv$data_no_outliers else
      if (!is.null(rv$cleaned_data)) rv$cleaned_data else rv$selected_data

    family <- get_family(input$family, input$linkfun)

    custom_eq <- trimws(input$custom_eq)
    #mode <- input$interaction_mode

    results <- list()

    # --- CASE 1: Custom equation provided ---
    if (nzchar(custom_eq)) {
      f_str <- if (grepl("~", custom_eq)) custom_eq else paste(input$y, "~", custom_eq)
      # if (!grepl("\\|", f_str) && nzchar(input$random_effects)) {
      #   f_str <- paste(f_str, "+", input$random_effects)
      # }
      if (input$engine != "nlme::lme") {
        if (!grepl("\\|", f_str) && nzchar(input$random_effects)) {
          f_str <- paste(f_str, "+", input$random_effects)
        }
      }
      f <- as.formula(f_str)

      tryCatch({
        if (input$engine == "afex::mixed") {
          if (input$family == "gaussian") {
            model <- mixed(f, data = df, method = "KR")
            anova_tab <- anova(model, ddf = "Kenward-Roger", type = 3)
          }
        } else if (input$engine == "lme4::glmer") {
          base_family <- switch(
            input$family,
            "gamma"    = Gamma(),
            "binomial" = binomial(),
            "poisson"  = poisson()
          )
          model <- mixed(f, data = df, family = base_family, method = "LRT")
          anova_tab <- anova(model)
        }else if(input$engine == "nlme::lme") {
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
          can_use_corr <- nlme_can_use_correlation(df, subject_var)

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


          model <- nlme::lme(
            fixed = as.formula(fixed_str),
            random = random_formula,
            correlation = correlation,
            data = df,
            method = "REML"
          )

          anova_tab <- anova(model)
        }

        results[[custom_eq]] <- list(engine  = input$engine,formula = f_str, model = model, anova = anova_tab)
      }, error = function(e) {
        results[[custom_eq]] <- list(formula = f_str, error = e$message)
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
          if (input$engine == "afex::mixed") {

            # AFEX rule:
            # gaussian method = "KR"
            # non-gaussian family only (no link), method = "LRT"

            fam_name <- input$family

            if (fam_name == "gaussian") {
              model <- mixed(f, data = df, method = "KR")
              anova_tab <- anova(model, ddf = "Kenward-Roger", type = 3)
            } else {
              # Remove link (AFEX does NOT support custom links)
              base_family <- switch(
                fam_name,
                "gamma" = Gamma(),
                "binomial" = binomial(),
                "poisson" = poisson()
              )
              model <- mixed(f, data = df, family = base_family, method = "LRT")
              anova_tab <- anova(model)
            }
          }
          else if (input$engine == "lme4::glmer") {
            model <- glmer(f, data = df, family = family,
                           control = glmerControl(optimizer = "bobyqa",
                                                  optCtrl = list(maxfun = 2e5)))
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

            model <- nlme::lme(
              fixed = as.formula(fixed_str),
              random = random_formula,
              correlation = correlation,
              data = df,
              method = "REML"
            )

            anova_tab <- anova(model)
          }

          results[[iv]] <- list(engine  = input$engine,formula = f_str, model = model, anova = anova_tab)

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
