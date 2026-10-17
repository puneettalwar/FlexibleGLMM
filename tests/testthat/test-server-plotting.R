library(shiny)

make_test_df <- function(n = 30) {
  data.frame(
    Reaction_time = rnorm(n, 250, 20),
    Days = rep(0:2, length.out = n),
    Sex = factor(rep(c("Male", "Female"), length.out = n)),
    Subject = factor(rep(1:10, each = 3, length.out = n))
  )
}

test_that("plotting_server's current_data() falls back to rv$data when nothing else is set", {
  df <- make_test_df()
  rv <- reactiveValues(data = df)
  testServer(as_server(plotting_server, rv), {
    expect_identical(srv$current_data(), df)
  })
})

test_that("plotting_server's current_data() prefers processed_data over everything else", {
  df_processed <- make_test_df()
  df_other <- make_test_df()
  rv <- reactiveValues(
    data = df_other, selected_data = df_other, cleaned_data = df_other,
    data_no_outliers = df_other, processed_data = df_processed
  )
  testServer(as_server(plotting_server, rv), {
    expect_identical(srv$current_data(), df_processed)
  })
})

test_that("plotting_server's current_data() falls back correctly down the precedence chain", {
  df_no_outliers <- make_test_df()
  df_cleaned <- make_test_df()
  df_selected <- make_test_df()

  rv1 <- reactiveValues(cleaned_data = df_cleaned, selected_data = df_selected)
  testServer(as_server(plotting_server, rv1), {
    expect_identical(srv$current_data(), df_cleaned)
  })

  rv2 <- reactiveValues(selected_data = df_selected)
  testServer(as_server(plotting_server, rv2), {
    expect_identical(srv$current_data(), df_selected)
  })

  rv3 <- reactiveValues(data_no_outliers = df_no_outliers, cleaned_data = df_cleaned)
  testServer(as_server(plotting_server, rv3), {
    expect_identical(srv$current_data(), df_no_outliers)
  })
})

test_that("boxplot_var_selector only lists factor columns from the current dataset", {
  df <- make_test_df()
  rv <- reactiveValues(selected_data = df)
  testServer(as_server(plotting_server, rv), {
    session$flushReact()
    ui_html <- paste(unlist(output$boxplot_var_selector), collapse = " ")
    expect_true(grepl("Sex", ui_html))
    expect_true(grepl("Subject", ui_html))
    expect_false(grepl("Reaction_time", ui_html))
    expect_false(grepl("Days", ui_html))
  })
})

test_that("corr_iv_selector only lists numeric columns from the current dataset", {
  df <- make_test_df()
  rv <- reactiveValues(selected_data = df)
  testServer(as_server(plotting_server, rv), {
    session$flushReact()
    ui_html <- paste(unlist(output$corr_iv_selector), collapse = " ")
    expect_true(grepl("Reaction_time", ui_html))
    expect_true(grepl("Days", ui_html))
    expect_false(grepl(">Sex<", ui_html))
  })
})

test_that("t_test_output runs pairwise.t.test on the current dataset without error", {
  set.seed(42)
  df <- make_test_df()
  rv <- reactiveValues(selected_data = df)
  testServer(as_server(plotting_server, rv), {
    session$setInputs(y = "Reaction_time", boxplot_cats = "Sex")
    out <- paste(output$t_test_output, collapse = "\n")
    expect_true(nchar(out) > 0)
    expect_true(grepl("t tests", out))
  })
})

test_that("corr_stats reports both Spearman and regression results", {
  set.seed(42)
  df <- make_test_df()
  rv <- reactiveValues(selected_data = df)
  testServer(as_server(plotting_server, rv), {
    session$setInputs(y = "Reaction_time", corr_iv = "Days")
    out <- paste(output$corr_stats, collapse = "\n")
    expect_true(grepl("Spearman", out))
    expect_true(grepl("Regression", out))
  })
})

test_that("plotting outputs stay inert until y and the paired input are both set", {
  df <- make_test_df()
  rv <- reactiveValues(selected_data = df)
  testServer(as_server(plotting_server, rv), {
    session$setInputs(y = "Reaction_time")  # boxplot_cats intentionally unset
    # req() stops the output silently: depending on the shiny version this
    # surfaces as a shiny.silent.error or as NULL - either is "inert".
    res <- tryCatch(output$t_test_output, error = function(e) e)
    expect_true(is.null(res) || inherits(res, "shiny.silent.error"))
  })
})
