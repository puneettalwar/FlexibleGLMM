#**************************************************************
# plotting.R
# Descriptive/summary plots: boxplots by categorical IV with
# pairwise t-tests, and Spearman correlation scatter plots with
# a downloadable PNG.
#
# plotting_server() is called from server.R.
#**************************************************************

plotting_server <- function(input, output, session, rv) {

  # Keep this reactive inside plotting_server(), where rv is in scope.
  current_data <- reactive({
    if (!is.null(rv$processed_data)) {
      rv$processed_data
    } else if (!is.null(rv$data_no_outliers)) {
      rv$data_no_outliers
    } else if (!is.null(rv$cleaned_data)) {
      rv$cleaned_data
    } else if (!is.null(rv$selected_data)) {
      rv$selected_data
    } else {
      rv$data
    }
  })

  output$boxplot_var_selector <- renderUI({

    #df <- rv$selected_data
    df <- current_data()
    req(df)
    selectInput("boxplot_cats", "Select Categorical IV:",
                choices = names(df)[sapply(df, is.factor)])
  })

  output$boxplot_output <- renderPlot({
    req(input$y, input$boxplot_cats)
    df <- current_data()
    req(df, input$y %in% names(df), input$boxplot_cats %in% names(df))
    ggplot(df, aes(x = .data[[input$boxplot_cats]],
                   y = .data[[input$y]])) +
      geom_boxplot(fill = "lightblue") +
      theme_bw() +
      labs(title = paste("Boxplot of", input$y, "by", input$boxplot_cats))
  })

  output$t_test_output <- renderPrint({
    req(input$y, input$boxplot_cats)
    df <- current_data()
    req(df, input$y %in% names(df), input$boxplot_cats %in% names(df))
    pairwise.t.test(df[[input$y]], df[[input$boxplot_cats]],
                    p.adjust.method = "none")
  })

  output$corr_iv_selector <- renderUI({
    #df <- rv$selected_data
    df <- current_data()
    req(df)
    selectInput("corr_iv", "Select Numeric IV:",
                choices = names(df)[sapply(df, is.numeric)])
  })

  corr_plot_reactive <- reactive({
    req(input$y, input$corr_iv)
    df <- current_data()
    req(df, input$y %in% names(df), input$corr_iv %in% names(df))
    validate(need(is.numeric(df[[input$y]]),
                  "The dependent variable must be numeric for correlation plots."))
    ggplot(df, aes(x = .data[[input$corr_iv]],
                   y = .data[[input$y]])) +
      geom_point() +
      geom_smooth(method = "lm", se = TRUE) +
      theme_bw() +
      labs(title = paste("Spearman Correlation:", input$y, "vs", input$corr_iv))
  })
  output$corr_plot <- renderPlot({ corr_plot_reactive() })

  output$corr_stats <- renderPrint({
    req(input$y, input$corr_iv)
    df <- current_data()
    req(df, input$y %in% names(df), input$corr_iv %in% names(df))
    validate(need(is.numeric(df[[input$y]]) &&
                    is.numeric(df[[input$corr_iv]]),
                  "Both variables must be numeric for correlation analysis."))
    sp <- cor.test(df[[input$corr_iv]], df[[input$y]], method = "spearman")

    reg <- summary(lm(df[[input$y]] ~ df[[input$corr_iv]]))

    list(
      Spearman = sp,
      Regression = reg$coefficients
    )
  })

  output$download_corr_plot <- downloadHandler(
    filename = function() {
      paste0("correlation_plot_", input$y, "_", input$corr_iv, ".png")
    },
    content = function(file) {
      ggsave(file, corr_plot_reactive())
    }
  )
}
