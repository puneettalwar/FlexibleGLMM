library(shiny)

make_raw_df <- function() {
  data.frame(
    Subject = rep(1:6, each = 3),
    Reaction_time = c(250, 255, 260, 240, NA, 245, 300, 305, 900, 260, 265, 270, 255, 260, 265, 250, 245, 240),
    Days = rep(0:2, 6),
    Sex = rep(c("M", "F"), each = 3, length.out = 18),
    stringsAsFactors = FALSE
  )
}

test_that("uploading a CSV populates rv$data and rv$selected_data", {
  df <- make_raw_df()
  tmp <- tempfile(fileext = ".csv")
  on.exit(unlink(tmp))
  write.csv(df, tmp, row.names = FALSE)

  rv <- reactiveValues(data = NULL, selected_data = NULL, cleaned_data = NULL,
                        data_no_outliers = NULL, models = NULL)

  testServer(as_server(data_processing_server, rv), {
    session$setInputs(data_file = data.frame(
      name = "test.csv", size = file.size(tmp), type = "text/csv",
      datapath = tmp, stringsAsFactors = FALSE
    ))
    session$flushReact()
    expect_equal(nrow(rv$data), 18)
    expect_true("Reaction_time" %in% names(rv$data))
    expect_equal(nrow(rv$selected_data), 18)
  })
})

test_that("apply_column_selection subsets rv$selected_data to the chosen columns", {
  df <- make_raw_df()
  rv <- reactiveValues(data = df, selected_data = df, cleaned_data = NULL,
                        data_no_outliers = NULL, models = NULL)

  testServer(as_server(data_processing_server, rv), {
    session$setInputs(selected_columns = c("Subject", "Reaction_time"))
    session$setInputs(apply_column_selection = 1)
    expect_equal(names(rv$selected_data), c("Subject", "Reaction_time"))
    expect_equal(nrow(rv$selected_data), nrow(df))
  })
})

test_that("apply_var_types converts requested columns to factor/numeric", {
  df <- make_raw_df()
  rv <- reactiveValues(data = df, selected_data = df, cleaned_data = NULL,
                        data_no_outliers = NULL, models = NULL)

  testServer(as_server(data_processing_server, rv), {
    session$setInputs(factor_vars = c("Sex", "Subject"), numeric_vars = "Reaction_time")
    session$setInputs(apply_var_types = 1)
    expect_s3_class(rv$selected_data$Sex, "factor")
    expect_s3_class(rv$selected_data$Subject, "factor")
    expect_type(rv$selected_data$Reaction_time, "double")
  })
})

test_that("remove_missing drops rows with any NA and reports before/after dimensions", {
  df <- make_raw_df()
  rv <- reactiveValues(data = df, selected_data = df, cleaned_data = NULL,
                        data_no_outliers = NULL, models = NULL)

  testServer(as_server(data_processing_server, rv), {
    session$setInputs(remove_missing = 1)
    expect_equal(nrow(rv$cleaned_data), sum(complete.cases(df)))
    expect_true(grepl("Before: 18", output$dimBefore))
    expect_true(grepl(paste0("After: ", sum(complete.cases(df))), output$dimAfter))
  })
})

test_that("Z-score outlier removal drops rows beyond the chosen SD cutoff", {
  df <- make_raw_df()
  df_complete <- df[complete.cases(df), ]
  rv <- reactiveValues(data = df, selected_data = df_complete, cleaned_data = df_complete,
                        data_no_outliers = NULL, models = NULL)

  testServer(as_server(data_processing_server, rv), {
    session$setInputs(outlier_method = "Z-score (SD cutoff)", n_sigmas = 1.5)
    session$setInputs(remove_outliers = 1)
    expect_true(nrow(rv$data_no_outliers) <= nrow(df_complete))
    # The 900 ms reaction time is a deliberately extreme value in the fixture
    expect_false(900 %in% rv$data_no_outliers$Reaction_time)
  })
})

test_that("standardization with center-and-scale adds a matching _cs column", {
  df <- make_raw_df()
  df_complete <- df[complete.cases(df), ]
  rv <- reactiveValues(data = df, selected_data = df_complete, cleaned_data = df_complete,
                        data_no_outliers = NULL, models = NULL)

  testServer(as_server(data_processing_server, rv), {
    session$setInputs(standardize_vars = "Reaction_time", center_scale_mode = "center_scale")
    session$setInputs(apply_standardization = 1)
    expect_true("Reaction_time_cs" %in% names(rv$selected_data))
    expect_equal(mean(rv$selected_data$Reaction_time_cs), 0, tolerance = 1e-8)
  })
})

test_that("standardization with center-only adds a _c column, not _cs", {
  df <- make_raw_df()
  df_complete <- df[complete.cases(df), ]
  rv <- reactiveValues(data = df, selected_data = df_complete, cleaned_data = df_complete,
                        data_no_outliers = NULL, models = NULL)

  testServer(as_server(data_processing_server, rv), {
    session$setInputs(standardize_vars = "Reaction_time", center_scale_mode = "center_only")
    session$setInputs(apply_standardization = 1)
    expect_true("Reaction_time_c" %in% names(rv$selected_data))
    expect_false("Reaction_time_cs" %in% names(rv$selected_data))
    expect_equal(mean(rv$selected_data$Reaction_time_c), 0, tolerance = 1e-8)
  })
})
