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
