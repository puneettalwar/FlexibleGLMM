library(shiny)

make_weights_test_df <- function(n_subj = 12, n_per = 4) {
  set.seed(7)
  subj <- rep(seq_len(n_subj), each = n_per)
  sex <- factor(rep(rep(c("M", "F"), each = n_per), length.out = n_subj * n_per))
  days <- rep(seq_len(n_per) - 1, times = n_subj)
  # Deliberately unequal residual variance by Sex, so a model with
  # weights = varIdent(form = ~1|Sex) is a meaningfully different fit
  # from one without it.
  noise_sd <- ifelse(sex == "M", 5, 20)
  y <- 250 + 2 * days + rnorm(length(days), sd = noise_sd)
  data.frame(Reaction_time = y, Days = days, Sex = sex, Subject = factor(subj))
}

test_that("custom-equation mode fits even when input$y/input$random_effects are unset", {
  skip_if_not_installed("nlme")
  df <- make_weights_test_df()
  rv <- reactiveValues(selected_data = df, cleaned_data = NULL,
                        data_no_outliers = NULL, models = NULL)

  testServer(model_fit_server, args = list(rv = rv), {
    # Deliberately NOT setting input$y, input$x, or input$random_effects -
    # the whole point of this mode is that it does not need them.
    session$setInputs(
      custom_eq = "nlme::lme(Reaction_time ~ Days, random = ~1|Subject, data = df, method = \"REML\")",
      run = 1
    )
    results <- runModels()
    expect_length(results, 1)
    res <- results[[1]]
    expect_null(res$error)
    expect_s3_class(res$model, "lme")
    expect_equal(res$engine, "nlme::lme")
  })
})

test_that("custom-equation mode supports weights = varIdent(...) for nlme::lme", {
  skip_if_not_installed("nlme")
  df <- make_weights_test_df()
  rv <- reactiveValues(selected_data = df, cleaned_data = NULL,
                        data_no_outliers = NULL, models = NULL)

  testServer(model_fit_server, args = list(rv = rv), {
    session$setInputs(
      custom_eq = paste0(
        "nlme::lme(Reaction_time ~ Days, random = ~1|Subject, ",
        "weights = varIdent(form = ~1|Sex), data = df, method = \"REML\")"
      ),
      run = 1
    )
    results <- runModels()
    res <- results[[1]]
    expect_null(res$error)
    expect_s3_class(res$model, "lme")
    # A fitted varIdent weights structure should report two distinct
    # variance multipliers (one per Sex level, up to the reference level).
    var_params <- nlme::varWeights(res$model$modelStruct$varStruct)
    expect_true(length(unique(round(var_params, 4))) >= 1)
    expect_false(is.null(res$model$modelStruct$varStruct))
  })
})

test_that("custom equation ignores sidebar family/engine/random-effects selections", {
  skip_if_not_installed("nlme")
  df <- make_weights_test_df()
  rv <- reactiveValues(selected_data = df, cleaned_data = NULL,
                        data_no_outliers = NULL, models = NULL)

  testServer(model_fit_server, args = list(rv = rv), {
    # Sidebar says afex::mixed/gaussian/(1|Subject) - the custom call
    # explicitly fits nlme::lme instead, and that must be what runs.
    session$setInputs(
      engine = "afex::mixed",
      family = "gaussian",
      random_effects = "(1|Subject)",
      custom_eq = "nlme::lme(Reaction_time ~ Days, random = ~1|Subject, data = df)",
      run = 1
    )
    results <- runModels()
    res <- results[[1]]
    expect_equal(res$engine, "nlme::lme")
    expect_s3_class(res$model, "lme")
  })
})

test_that("a malicious custom equation is rejected, not evaluated", {
  df <- make_weights_test_df()
  rv <- reactiveValues(selected_data = df, cleaned_data = NULL,
                        data_no_outliers = NULL, models = NULL)

  marker_file <- tempfile()
  on.exit(unlink(marker_file), add = TRUE)

  testServer(model_fit_server, args = list(rv = rv), {
    session$setInputs(
      custom_eq = sprintf(
        "mixed(Reaction_time ~ Days, data = df, weights = file.create(\"%s\"))",
        marker_file
      ),
      run = 1
    )
    results <- runModels()
    res <- results[[1]]
    expect_true(!is.null(res$error))
    expect_true(grepl("not permitted", res$error))
  })

  # The real assertion: the disallowed function must never have run.
  expect_false(file.exists(marker_file))
})

test_that("a custom equation calling a disallowed top-level function is rejected", {
  df <- make_weights_test_df()
  rv <- reactiveValues(selected_data = df, cleaned_data = NULL,
                        data_no_outliers = NULL, models = NULL)

  testServer(model_fit_server, args = list(rv = rv), {
    session$setInputs(custom_eq = "lm(Reaction_time ~ Days, data = df)", run = 1)
    results <- runModels()
    res <- results[[1]]
    expect_true(!is.null(res$error))
    expect_true(grepl("must call afex::mixed", res$error))
  })
})

test_that("data = is always forced to the app's working dataset even if the user typed something else", {
  df <- make_weights_test_df()
  rv <- reactiveValues(selected_data = df, cleaned_data = NULL,
                        data_no_outliers = NULL, models = NULL)

  testServer(model_fit_server, args = list(rv = rv), {
    session$setInputs(
      custom_eq = "mixed(Reaction_time ~ Days, data = some_dataframe_that_does_not_exist, method = \"KR\")",
      run = 1
    )
    results <- runModels()
    res <- results[[1]]
    # Should succeed using the real df, not error about a missing object.
    expect_null(res$error)
  })
})
