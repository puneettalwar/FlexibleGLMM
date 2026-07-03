#**************************************************************
# helpers.R
# nlme helpers (TIME-AWARE) - subject/random-effect/correlation
# structure handling for the nlme::lme engine
#**************************************************************

get_nlme_subject <- function(random_formula) {
  if (!nzchar(random_formula)) return(NULL)
  gsub("^~.*\\|\\s*([^/]+).*$", "\\1", random_formula)
}

prepare_nlme_data <- function(df, subject_var, time_var = NULL) {

  df[[subject_var]] <- as.factor(df[[subject_var]])

  # Order by subject AND time if provided
  if (!is.null(time_var) && nzchar(time_var) && time_var %in% names(df)) {
    df <- df[order(df[[subject_var]], df[[time_var]]), ]
  } else {
    df <- df[order(df[[subject_var]]), ]
  }

  # Fallback index (only used if no time_var)
  df$.trial_index <- ave(seq_len(nrow(df)), df[[subject_var]], FUN = seq_along)

  df
}


nlme_can_use_correlation <- function(df, subject_var, time_var = NULL) {

  grp_sizes <- table(df[[subject_var]])

  enough_repeats <- sum(grp_sizes >= 2) >= 2

  if (!enough_repeats) return(FALSE)

  # If time_var supplied, ensure it varies within subject
  if (!is.null(time_var) && nzchar(time_var) && time_var %in% names(df)) {

    within_var <- tapply(df[[time_var]], df[[subject_var]], function(x)
      length(unique(x)) > 1
    )

    return(any(within_var))
  }

  TRUE
}


suggest_correlation_structure <- function(df, subject_var, time_var = NULL) {

  if (!nlme_can_use_correlation(df, subject_var, time_var)) {
    return("none")
  }

  if (!is.null(time_var) && nzchar(time_var) && time_var %in% names(df)) {
    return("corAR1")
  }

  return("corCompSymm")
}


get_nlme_corStruct <- function(type, groupvar, timevar = NULL) {

  if (is.null(type) || type == "none" || !nzchar(type)) {
    return(NULL)
  }

  switch(
    type,

    # AR(1) — USE time variable if available
    corAR1 = {
      if (!is.null(timevar) && nzchar(timevar)) {
        corAR1(form = as.formula(paste("~", timevar, "|", groupvar)))
      } else {
        corAR1(form = as.formula(paste("~ .trial_index |", groupvar)))
      }
    },

    # Compound symmetry
    corCompSymm = {
      corCompSymm(form = as.formula(paste("~ 1 |", groupvar)))
    },

    # Unstructured symmetric
    corSymm = {
      if (!is.null(timevar) && nzchar(timevar)) {
        corSymm(form = as.formula(paste("~", timevar, "|", groupvar)))
      } else {
        corSymm(form = as.formula(paste("~ .trial_index |", groupvar)))
      }
    },

    # Continuous-time exponential
    corExp = {
      if (is.null(timevar) || !nzchar(timevar)) {
        stop("corExp requires a numeric time variable.")
      }
      corExp(form = as.formula(paste("~", timevar, "|", groupvar)))
    },

    NULL
  )
}
