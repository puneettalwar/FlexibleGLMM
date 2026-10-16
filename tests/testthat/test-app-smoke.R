test_that("the full app launches and the main UI elements are present", {
  skip_if_not_installed("shinytest2")
  skip_on_cran()

  # FlexibleGLMM is a function-based app (R/app_ui.R + R/app_server.R,
  # launched via run_app()), not a directory containing app.R/ui.R/server.R,
  # so AppDriver needs an actual app object rather than app_dir =.
  app_obj <- shiny::shinyApp(ui = FlexibleGLMM:::app_ui, server = FlexibleGLMM:::app_server)
  app <- shinytest2::AppDriver$new(
    app_obj,
    name = "flexibleglmm-smoke",
    height = 900, width = 1400,
    load_timeout = 60 * 1000
  )
  on.exit(app$stop(), add = TRUE)

  # Sidebar and key controls should be present on first load.
  app$expect_values(input = c("family", "engine", "linkfun"))

  vals <- app$get_values(input = c("family", "engine"))
  expect_equal(vals$input$family, "gaussian")
  expect_equal(vals$input$engine, "afex::mixed")
})

test_that("uploading sleepstudy and running a model produces non-empty output", {
  skip_if_not_installed("shinytest2")
  skip_on_cran()
  skip_if_not_installed("lme4")

  tmp <- tempfile(fileext = ".csv")
  on.exit(unlink(tmp), add = TRUE)
  dat <- lme4::sleepstudy
  names(dat) <- c("Reaction_time", "Days", "Subject")
  write.csv(dat, tmp, row.names = FALSE)

  app_obj <- shiny::shinyApp(ui = FlexibleGLMM:::app_ui, server = FlexibleGLMM:::app_server)
  app <- shinytest2::AppDriver$new(
    app_obj,
    name = "flexibleglmm-run-model",
    height = 900, width = 1400,
    load_timeout = 60 * 1000
  )
  on.exit(app$stop(), add = TRUE)

  app$upload_file(data_file = tmp)
  app$wait_for_idle()

  app$set_inputs(selected_columns = c("Subject", "Reaction_time", "Days"))
  app$click("apply_column_selection")
  app$wait_for_idle()

  app$set_inputs(factor_vars = "Subject")
  app$click("apply_var_types")
  app$wait_for_idle()

  app$click("remove_missing")
  app$wait_for_idle()

  app$set_inputs(y = "Reaction_time", x = "Days", random_effects = "(1|Subject)")
  app$click("run")
  app$wait_for_idle(timeout = 60 * 1000)

  model_output <- app$get_value(output = "modelOutput")
  expect_true(nchar(model_output) > 0)
  expect_false(grepl("No models fitted yet", model_output))
})
