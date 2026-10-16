test_that("get_nlme_subject extracts the grouping variable from a random-effects formula", {
  expect_equal(get_nlme_subject("~1|Subject"), "Subject")
  expect_equal(get_nlme_subject("~Days|Subject"), "Subject")
})

test_that("get_nlme_subject returns NULL for an empty string", {
  expect_null(get_nlme_subject(""))
})

test_that("prepare_nlme_data orders by subject and adds a fallback trial index", {
  dat <- data.frame(
    Subject = factor(c(2, 2, 1, 1)),
    y = c(4, 3, 2, 1)
  )
  out <- prepare_nlme_data(dat, "Subject")
  expect_equal(as.character(out$Subject), c("1", "1", "2", "2"))
  expect_equal(out$.trial_index, c(1, 2, 1, 2))
})

test_that("prepare_nlme_data orders by subject then by an explicit time variable", {
  dat <- data.frame(
    Subject = factor(c(1, 1, 1)),
    Days = c(2, 0, 1),
    y = c(30, 10, 20)
  )
  out <- prepare_nlme_data(dat, "Subject", time_var = "Days")
  expect_equal(out$Days, c(0, 1, 2))
  expect_equal(out$y, c(10, 20, 30))
})

test_that("nlme_can_use_correlation requires at least two subjects with >=2 repeats", {
  one_repeat_only <- data.frame(Subject = factor(c(1, 2, 3)))
  expect_false(nlme_can_use_correlation(one_repeat_only, "Subject"))

  enough_repeats <- data.frame(Subject = factor(c(1, 1, 2, 2, 3)))
  expect_true(nlme_can_use_correlation(enough_repeats, "Subject"))
})

test_that("nlme_can_use_correlation with a time_var also requires within-subject variation in time", {
  no_time_variation <- data.frame(
    Subject = factor(c(1, 1, 2, 2)),
    Days = c(0, 0, 0, 0)
  )
  expect_false(nlme_can_use_correlation(no_time_variation, "Subject", time_var = "Days"))

  with_time_variation <- data.frame(
    Subject = factor(c(1, 1, 2, 2)),
    Days = c(0, 1, 0, 1)
  )
  expect_true(nlme_can_use_correlation(with_time_variation, "Subject", time_var = "Days"))
})

test_that("suggest_correlation_structure recommends AR(1) when a time variable is usable", {
  dat <- data.frame(
    Subject = factor(c(1, 1, 2, 2)),
    Days = c(0, 1, 0, 1)
  )
  expect_equal(suggest_correlation_structure(dat, "Subject", time_var = "Days"), "corAR1")
})

test_that("suggest_correlation_structure recommends compound symmetry without a time variable", {
  dat <- data.frame(Subject = factor(c(1, 1, 2, 2)))
  expect_equal(suggest_correlation_structure(dat, "Subject"), "corCompSymm")
})

test_that("suggest_correlation_structure falls back to 'none' when repeats are insufficient", {
  dat <- data.frame(Subject = factor(c(1, 2, 3)))
  expect_equal(suggest_correlation_structure(dat, "Subject"), "none")
})

test_that("get_nlme_corStruct returns NULL for 'none' or an empty type", {
  expect_null(get_nlme_corStruct("none", "Subject"))
  expect_null(get_nlme_corStruct("", "Subject"))
  expect_null(get_nlme_corStruct(NULL, "Subject"))
})

test_that("get_nlme_corStruct builds the requested nlme correlation structure", {
  skip_if_not_installed("nlme")
  expect_s3_class(get_nlme_corStruct("corCompSymm", "Subject"), "corCompSymm")
  expect_s3_class(get_nlme_corStruct("corAR1", "Subject", "Days"), "corAR1")
  expect_s3_class(get_nlme_corStruct("corSymm", "Subject", "Days"), "corSymm")
  expect_s3_class(get_nlme_corStruct("corExp", "Subject", "Days"), "corExp")
})

test_that("get_nlme_corStruct requires a time variable for corExp", {
  expect_error(get_nlme_corStruct("corExp", "Subject"), "requires a numeric time variable")
})
