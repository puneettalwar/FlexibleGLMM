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
  testServer(plotting_server, args = list(rv = reactiveValues(data = df)), {
    expect_identical(current_data(), df)
  })
})

test_that("plotting_server's current_data() prefers processed_data over everything else", {
  df_processed <- make_test_df()
  df_other <- make_test_df()
  rv <- reactiveValues(
    data = df_other,
    selected_data = df_other,
    cleaned_data = df_other,
    data_no_outliers = df_other,
    processed_data = df_processed
  )
  testServer(plotting_server, args = list(rv = rv), {
    expect_identical(current_data(), df_processed)
  })
})

test_that("plotting_server's current_data() falls back correctly down the precedence chain", {
  df_no_outliers <- make_test_df()
  df_cleaned <- make_test_df()
  df_selected <- make_test_df()

  # Only cleaned_data and selected_data set -> cleaned_data wins
  rv1 <- reactiveValues(cleaned_data = df_cleaned, selected_data = df_selected)
  testServer(plotting_server, args = list(rv = rv1), {
    expect_identical(current_data(), df_cleaned)
  })

  # Only selected_data set -> selected_data wins
  rv2 <- reactiveValues(selected_data = df_selected)
  testServer(plotting_server, args = list(rv = rv2), {
    expect_identical(current_data(), df_selected)
  })

  # data_no_outliers set alongside cleaned_data -> data_no_outliers wins
  rv3 <- reactiveValues(data_no_outliers = df_no_outliers, cleaned_data = df_cleaned)
  testServer(plotting_server, args = list(rv = rv3), {
    expect_identical(current_data(), df_no_outliers)
  })
})

test_that("boxplot_var_selector only lists factor columns from the current dataset", {
  df <- make_test_df()
  testServer(plotting_server, args = list(rv = reactiveValues(selected_data = df)), {
    session$flushReact()
    ui_html <- output$boxplot_var_selector
    expect_true(grepl("Sex", ui_html))
    expect_true(grepl("Subject", ui_html))
    expect_false(grepl("Reaction_time", ui_html))
    expect_false(grepl("Days", ui_html))
  })
})

test_that("corr_iv_selector only lists numeric columns from the current dataset", {
  df <- make_test_df()
  testServer(plotting_server, args = list(rv = reactiveValues(selected_data = df)), {
    session$flushReact()
    ui_html <- output$corr_iv_selector
    expect_true(grepl("Reaction_time", ui_html))
    expect_true(grepl("Days", ui_html))
    expect_false(grepl(">Sex<", ui_html))
  })
})

test_that("t_test_output runs pairwise.t.test on the current dataset without error", {
  set.seed(42)
  df <- make_test_df()
  testServer(plotting_server, args = list(rv = reactiveValues(selected_data = df)), {
    session$setInputs(y = "Reaction_time", boxplot_cats = "Sex")
    expect_type(output$t_test_output, "character")
    expect_true(nchar(output$t_test_output) > 0)
  })
})

test_that("corr_stats reports both Spearman and regression results", {
  set.seed(42)
  df <- make_test_df()
  testServer(plotting_server, args = list(rv = reactiveValues(selected_data = df)), {
    session$setInputs(y = "Reaction_time", corr_iv = "Days")
    expect_true(grepl("Spearman", output$corr_stats))
    expect_true(grepl("Regression", output$corr_stats))
  })
})

test_that("plotting outputs stay empty/inert until y and the paired input are both set", {
  df <- make_test_df()
  testServer(plotting_server, args = list(rv = reactiveValues(selected_data = df)), {
    session$setInputs(y = "Reaction_time")  # boxplot_cats / corr_iv intentionally unset
    expect_error(output$t_test_output, NA)  # req() silently suspends rather than erroring
  })
})
