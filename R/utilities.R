#**************************************************************
# utilities.R
# Small generic helper / utility functions used across modules
#**************************************************************

#----------------------
# Unwrap a fitted model object down to its underlying engine model
# (afex::mixed wraps lme4 objects; lme4/nlme objects pass through)
#----------------------
unwrap_model <- function(model) {

  # afex::mixed
  if (inherits(model, "mixed")) {

    # Newer afex
    if (!is.null(model$full_model)) {
      return(model$full_model)
    }

    # Older afex
    if (!is.null(model$merMod)) {
      return(model$merMod)
    }

    # Fallback
    if (!is.null(model$model)) {
      return(model$model)
    }
  }

  # lme4 / nlme models pass through
  model
}

#----------------------
# Strip lme4/afex-style random-effect terms, e.g. "+ (1|Subject)",
# out of a formula string (used to build nlme fixed-effects formulas)
#----------------------
strip_lme4_random <- function(formula_string) {
  gsub("\\+?\\s*\\([^\\)]*\\|[^\\)]*\\)", "", formula_string)
}

#----------------------
# Diagnostic flag helpers
#----------------------
check_singularity_flag <- function(model) {
  if (inherits(model, c("lmerMod", "glmerMod"))) {
    isSingular(model, tol = 1e-5)
  } else {
    NA
  }
}

check_convergence_flag <- function(model) {

  # lme4 / glmer
  if (inherits(model, c("lmerMod", "glmerMod"))) {
    optinfo <- model@optinfo
    if (!is.null(optinfo$conv$lme4$messages)) {
      return(paste(optinfo$conv$lme4$messages, collapse = "; "))
    }
    return("OK")
  }

  # nlme
  if (inherits(model, "lme")) {
    if (!is.null(model$fail) && model$fail) {
      return("Model failed to converge")
    }
    return("OK")
  }

  "OK"
}

#----------------------
# Capture every warning raised while fitting a model, regardless of
# engine (afex::mixed, lme4::glmer, nlme::lme all raise warnings
# differently - e.g. glmer's structured optinfo messages vs. plain
# base warnings from afex/nlme). This lets FlexibleGLMM surface
# convergence/boundary-fit warnings uniformly instead of relying on
# each engine's own warning mechanism.
#
# Returns list(result, warnings): on success, result is the fitted
# model; on failure, result is a "FlexibleGLMM_fit_error" object
# carrying the error message, so the caller can distinguish a failed
# fit from a successful-but-noisy one.
#----------------------
fit_with_diagnostics <- function(expr, model = NULL) {
  warnings <- character()

  value <- withCallingHandlers(
    tryCatch(
      expr,
      error = function(e) {
        structure(
          list(error = conditionMessage(e)),
          class = "FlexibleGLMM_fit_error"
        )
      }
    ),
    warning = function(w) {
      warnings <<- c(warnings, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )

  list(
    result = value,
    warnings = unique(warnings)
  )
}

#----------------------
# Positivity check for Gamma models with an identity link
# A Gamma mean must be > 0, but an
# identity-link linear predictor is not constrained to stay positive.
#----------------------
check_gamma_identity_positivity <- function(model, family_name, link_name) {
  if (!identical(family_name, "gamma") || !identical(link_name, "identity")) {
    return(invisible(NULL))
  }
  fitted_vals <- tryCatch(stats::fitted(model), error = function(e) NULL)
  if (is.null(fitted_vals)) return(invisible(NULL))
  if (any(fitted_vals <= 0, na.rm = TRUE)) {
    showNotification(
      paste0(
        "Warning: this Gamma (identity link) model has ",
        sum(fitted_vals <= 0, na.rm = TRUE),
        " non-positive fitted value(s). A Gamma mean must be > 0; ",
        "reconsider the link function or model specification before ",
        "interpreting this fit."
      ),
      type = "warning", duration = 10
    )
  }
  invisible(NULL)
}

#----------------------
# Number of subjects / observations used in a fitted model
#----------------------
get_n_subjects_obs <- function(model) {
  model_unwrapped <- unwrap_model(model)
  n_obs <- tryCatch(stats::nobs(model_unwrapped), error = function(e) NA)

  n_subj <- tryCatch({
    if (inherits(model_unwrapped, c("lmerMod", "glmerMod"))) {
      grp_counts <- lme4::ngrps(model_unwrapped)
      if (length(grp_counts) > 0) grp_counts[[1]] else NA
    } else if (inherits(model_unwrapped, "lme")) {
      length(unique(nlme::getGroups(model_unwrapped)))
    } else {
      NA
    }
  }, error = function(e) NA)

  list(n_subjects = n_subj, n_observations = n_obs)
}

#----------------------
# Family + link function builder
#----------------------
get_family <- function(fam, linkfun) {
  if (linkfun == "default") {
    return(
      switch(fam,
             "gaussian" = gaussian(),
             "gamma" = Gamma(),
             #"beta" = beta(),
             "binomial" = binomial(),
             "poisson" = poisson())
    )
  } else {
    return(
      switch(fam,
             "gaussian" = gaussian(link = linkfun),
             "gamma" = Gamma(link = linkfun),
             #"beta" = beta(link = linkfun),
             "binomial" = binomial(link = linkfun),
             "poisson" = poisson(link = linkfun))
    )
  }
}
