test_that("get_family returns correct family objects for default links", {
  expect_equal(get_family("gaussian", "default")$family, "gaussian")
  expect_equal(get_family("gamma", "default")$family, "Gamma")
  expect_equal(get_family("binomial", "default")$family, "binomial")
  expect_equal(get_family("poisson", "default")$family, "poisson")
})

test_that("get_family applies a custom link when one is given", {
  fam <- get_family("gaussian", "log")
  expect_equal(fam$family, "gaussian")
  expect_equal(fam$link, "log")

  fam2 <- get_family("gamma", "inverse")
  expect_equal(fam2$family, "Gamma")
  expect_equal(fam2$link, "inverse")
})

test_that("strip_lme4_random removes a trailing random-effect term", {
  expect_equal(
    strip_lme4_random("y ~ x1 + x2 + (1|Subject)"),
    "y ~ x1 + x2"
  )
  expect_equal(
    strip_lme4_random("y ~ x1 + (1 + Days | Subject)"),
    "y ~ x1"
  )
})

test_that("strip_lme4_random leaves formulas with no random term unchanged", {
  expect_equal(strip_lme4_random("y ~ x1 + x2"), "y ~ x1 + x2")
})

test_that("check_singularity_flag returns NA for non-lme4 models", {
  expect_true(is.na(check_singularity_flag(list())))
  expect_true(is.na(check_singularity_flag(structure(list(), class = "lme"))))
})

test_that("check_singularity_flag detects a singular lme4 fit", {
  skip_if_not_installed("lme4")
  set.seed(1)
  dat <- data.frame(
    y = rnorm(30),
    x = rnorm(30),
    Subject = factor(rep(1:10, each = 3))
  )
  # A random slope with almost no within-subject variation is a classic
  # way to force a singular fit deterministically for a unit test.
  suppressMessages(suppressWarnings({
    fit <- lme4::lmer(y ~ x + (1 + x | Subject), data = dat,
                       control = lme4::lmerControl(check.nobs.vs.nRE = "ignore"))
  }))
  expect_true(is.logical(check_singularity_flag(fit)))
})

test_that("check_convergence_flag returns 'OK' for a well-behaved lme4 fit", {
  skip_if_not_installed("lme4")
  set.seed(2)
  dat <- data.frame(
    y = rnorm(60),
    x = rnorm(60),
    Subject = factor(rep(1:20, each = 3))
  )
  fit <- lme4::lmer(y ~ x + (1 | Subject), data = dat)
  expect_equal(check_convergence_flag(fit), "OK")
})

test_that("check_convergence_flag reports failure for an nlme model that failed to converge", {
  fake_lme <- structure(list(fail = TRUE), class = "lme")
  expect_equal(check_convergence_flag(fake_lme), "Model failed to converge")

  fake_lme_ok <- structure(list(fail = NULL), class = "lme")
  expect_equal(check_convergence_flag(fake_lme_ok), "OK")
})

test_that("unwrap_model passes lme4/nlme model objects through unchanged", {
  fake_lmer <- structure(list(), class = "lmerMod")
  expect_identical(unwrap_model(fake_lmer), fake_lmer)
})

test_that("unwrap_model extracts the underlying fit from an afex::mixed object", {
  fake_afex_new <- structure(list(full_model = "inner_model_new"), class = "mixed")
  expect_equal(unwrap_model(fake_afex_new), "inner_model_new")

  fake_afex_old <- structure(list(merMod = "inner_model_old"), class = "mixed")
  expect_equal(unwrap_model(fake_afex_old), "inner_model_old")

  fake_afex_fallback <- structure(list(model = "inner_model_fallback"), class = "mixed")
  expect_equal(unwrap_model(fake_afex_fallback), "inner_model_fallback")
})

test_that("fit_with_diagnostics captures warnings without aborting the fit", {
  res <- fit_with_diagnostics({
    warning("first warning")
    warning("second warning")
    42
  })
  expect_equal(res$result, 42)
  expect_equal(res$warnings, c("first warning", "second warning"))
})

test_that("fit_with_diagnostics deduplicates repeated identical warnings", {
  res <- fit_with_diagnostics({
    warning("same message")
    warning("same message")
    1
  })
  expect_equal(res$warnings, "same message")
})

test_that("fit_with_diagnostics converts an error into a FlexibleGLMM_fit_error rather than raising", {
  res <- fit_with_diagnostics(stop("deliberate failure"))
  expect_s3_class(res$result, "FlexibleGLMM_fit_error")
  expect_equal(res$result$error, "deliberate failure")
  expect_length(res$warnings, 0)
})

test_that("check_gamma_identity_positivity is a no-op for non-Gamma or non-identity models", {
  # Should not error and should not attempt to call fitted() on a bare list
  # for combinations that don't need checking.
  expect_silent(check_gamma_identity_positivity(list(), "gaussian", "identity"))
  expect_silent(check_gamma_identity_positivity(list(), "gamma", "log"))
})

test_that("get_n_subjects_obs returns NA gracefully for an unrecognized model class", {
  info <- get_n_subjects_obs(structure(list(), class = "not_a_real_model"))
  expect_true(is.na(info$n_subjects))
})

test_that("get_n_subjects_obs reports correct counts for an lmer fit", {
  skip_if_not_installed("lme4")
  set.seed(3)
  dat <- data.frame(
    y = rnorm(60),
    x = rnorm(60),
    Subject = factor(rep(1:20, each = 3))
  )
  fit <- lme4::lmer(y ~ x + (1 | Subject), data = dat)
  info <- get_n_subjects_obs(fit)
  expect_equal(info$n_subjects, 20)
  expect_equal(info$n_observations, 60)
})

# ----------------------------------------------------------------
# Custom-equation parsing / safety (check_call_safety, parse_custom_model_call)
# ----------------------------------------------------------------

test_that("parse_custom_model_call accepts a well-formed afex::mixed() call", {
  parsed <- parse_custom_model_call(
    "mixed(Reaction_time ~ Days + (1|Subject), data = df, method = \"KR\")"
  )
  expect_equal(parsed$engine, "afex::mixed")
  expect_equal(parsed$fn_name, "mixed")
  expect_equal(parsed$expr$data, quote(df))
})

test_that("parse_custom_model_call detects each supported engine by function name", {
  expect_equal(parse_custom_model_call("mixed(y ~ x, data = df)")$engine, "afex::mixed")
  expect_equal(parse_custom_model_call("glmer(y ~ x, data = df)")$engine, "lme4::glmer")
  expect_equal(parse_custom_model_call("lmer(y ~ x, data = df)")$engine, "lme4::lmer")
  expect_equal(parse_custom_model_call("lme(y ~ x, data = df)")$engine, "nlme::lme")
  expect_equal(parse_custom_model_call("nlme::lme(y ~ x, data = df)")$engine, "nlme::lme")
})

test_that("parse_custom_model_call always overwrites data=, regardless of what the user typed", {
  parsed <- parse_custom_model_call(
    "mixed(y ~ x, data = some_other_object_the_user_typed, method = \"KR\")"
  )
  expect_equal(parsed$expr$data, quote(df))

  # even when data= was omitted entirely
  parsed2 <- parse_custom_model_call("mixed(y ~ x, method = \"KR\")")
  expect_equal(parsed2$expr$data, quote(df))
})

test_that("parse_custom_model_call accepts nlme::lme() with weights = varIdent(...)", {
  parsed <- parse_custom_model_call(
    paste0(
      "nlme::lme(Reaction_time ~ Days, random = ~1|Subject, ",
      "weights = varIdent(form = ~1|Sex), method = \"REML\")"
    )
  )
  expect_equal(parsed$engine, "nlme::lme")
  expect_equal(parsed$expr$data, quote(df))
  # the weights argument should have survived parsing untouched
  expect_true("weights" %in% names(parsed$expr))
})

test_that("parse_custom_model_call rejects an empty string", {
  expect_error(parse_custom_model_call(""), "empty")
  expect_error(parse_custom_model_call("   "), "empty")
})

test_that("parse_custom_model_call rejects text that isn't a full call", {
  expect_error(parse_custom_model_call("y ~ x1 + x2"), "full function call")
  expect_error(parse_custom_model_call("42"), "full function call")
})

test_that("parse_custom_model_call rejects a disallowed outer function", {
  expect_error(
    parse_custom_model_call("system(\"ls\")"),
    "must call afex::mixed"
  )
  expect_error(
    parse_custom_model_call("lm(y ~ x, data = df)"),
    "must call afex::mixed"
  )
})

test_that("check_call_safety rejects a disallowed function nested inside an argument", {
  # The outer call is a permitted engine, but a sub-expression tries to
  # call something outside the whitelist - this must still be rejected.
  expect_error(
    parse_custom_model_call(
      "mixed(y ~ x, data = df, weights = system(\"rm -rf /\"))"
    ),
    "not permitted"
  )
  expect_error(
    parse_custom_model_call(
      "nlme::lme(y ~ x, random = ~1|Subject, weights = file.remove(\"important.csv\"))"
    ),
    "not permitted"
  )
})

test_that("check_call_safety allows whitelisted nlme variance/correlation constructors nested arbitrarily", {
  expect_true(check_call_safety(str2lang(
    "lme(y ~ x, random = ~1|Subject, correlation = corAR1(form = ~Days|Subject), weights = varIdent(form = ~1|Sex))"
  )))
})

test_that("check_call_safety allows namespace-qualified calls like nlme::lme", {
  expect_true(check_call_safety(str2lang("nlme::lme(y ~ x, data = df)")))
})
