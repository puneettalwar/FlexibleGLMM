#**************************************************************
# plotting.R
# Descriptive/summary plots: boxplots by categorical IV with
# pairwise t-tests, and Spearman correlation scatter plots with
# a downloadable PNG.
#
# plotting_server() is called from server.R.
#**************************************************************

plotting_server <- function(input, output, session, rv) {

  output$boxplot_var_selector <- renderUI({
    df <- rv$selected_data
    req(df)
    selectInput("boxplot_cats", "Select Categorical IV:",
                choices = names(df)[sapply(df, is.factor)])
  })

  output$boxplot_output <- renderPlot({
    req(input$y, input$boxplot_cats)
    df <- rv$selected_data
    ggplot(df, aes_string(x = input$boxplot_cats, y = input$y)) +
      geom_boxplot(fill = "lightblue") +
      theme_bw() +
      labs(title = paste("Boxplot of", input$y, "by", input$boxplot_cats))
  })

  output$t_test_output <- renderPrint({
    req(input$y, input$boxplot_cats)
    df <- rv$selected_data
    pairwise.t.test(df[[input$y]], df[[input$boxplot_cats]], p.adjust.method = "none")
  })

  output$corr_iv_selector <- renderUI({
    df <- rv$selected_data
    req(df)
    selectInput("corr_iv", "Select Numeric IV:",
                choices = names(df)[sapply(df, is.numeric)])
  })

  corr_plot_reactive <- reactive({
    req(input$y, input$corr_iv)
    df <- rv$selected_data
    ggplot(df, aes_string(x = input$corr_iv, y = input$y)) +
      geom_point() +
      geom_smooth(method = "lm", se = TRUE) +
      theme_bw() +
      labs(title = paste("Spearman Correlation:", input$y, "vs", input$corr_iv))
  })
  output$corr_plot <- renderPlot({ corr_plot_reactive() })

  output$corr_stats <- renderPrint({
    req(input$y, input$corr_iv)
    df <- rv$selected_data

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
