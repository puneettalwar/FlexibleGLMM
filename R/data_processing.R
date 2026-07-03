#**************************************************************
# data_processing.R
# Data upload, column/variable-type selection, missing-value
# handling, outlier removal, standardization, and the dynamic
# variable-selector UI inputs (y / x / covariates / interactors
# / post-hoc factors).
#
# data_processing_server() is called once from server.R and
# wires up all related observers/outputs using the shared `rv`
# reactiveValues object.
#**************************************************************

data_processing_server <- function(input, output, session, rv) {

  #----------------------
  # Upload
  #----------------------

  observe({
    req(input$data_file)
    ext <- tools::file_ext(input$data_file$name)
    rv$data <- if (tolower(ext) == "csv") {
      read_csv(input$data_file$datapath)
    } else if (tolower(ext) == "xlsx") {
      read_excel(input$data_file$datapath)
    } else {
      stop("Invalid file type. Please upload a .csv or .xlsx file.")
    }
    rv$selected_data <- rv$data
  })

  #----------------------
  # Column selector
  #----------------------

  output$select_columns_ui <- renderUI({
    req(rv$data)
    checkboxGroupInput("selected_columns", "Select Columns to Keep:",
                       choices = names(rv$data), selected = names(rv$data))
  })
  observeEvent(input$apply_column_selection, {
    req(rv$data, input$selected_columns)
    rv$selected_data <- rv$data[, input$selected_columns, drop = FALSE]
    showNotification("Column selection applied", type = "message")
  })

  #----------------------
  # Variable types
  #----------------------

  output$factor_vars_ui <- renderUI({
    req(rv$selected_data)
    selectInput(
      "factor_vars",
      "Categorical Variables",
      choices = names(rv$selected_data),
      multiple = TRUE
    )
  })

  output$numeric_vars_ui <- renderUI({
    req(rv$selected_data)
    selectInput(
      "numeric_vars",
      "Numeric Variables",
      choices = names(rv$selected_data),
      multiple = TRUE
    )
  })

  # Apply variable types
  observeEvent(input$apply_var_types, {
    req(rv$selected_data)

    df <- rv$selected_data

    # Convert to factors
    if (!is.null(input$factor_vars)) {
      for (v in input$factor_vars) {
        df[[v]] <- as.factor(df[[v]])
      }
    }

    # Convert to numeric
    if (!is.null(input$numeric_vars)) {
      for (v in input$numeric_vars) {
        df[[v]] <- suppressWarnings(as.numeric(df[[v]]))
      }
    }

    rv$selected_data <- df
    rv$processed_data <- df

    showNotification(
      "Variable types applied successfully",
      type = "message"
    )
  })


  #----------------------
  # Remove missing values
  #----------------------

  observeEvent(input$remove_missing, {
    df <- rv$selected_data
    output$dimBefore <- renderText({ paste("Before:", paste(dim(df), collapse = " x ")) })
    output$missingDataCheck <- renderText({
      paste("Missing per variable:",
            paste(names(df), sapply(df, function(x) sum(is.na(x))), sep=": ", collapse=", "))
    })
    rv$cleaned_data <- na.omit(df)
    output$dimAfter <- renderText({
      paste("After:", paste(dim(rv$cleaned_data), collapse = " x "))
    })
  })

  # # Outlier removal
  # observeEvent(input$remove_outliers, {
  #   df <- if (is.null(rv$cleaned_data)) rv$selected_data else rv$cleaned_data
  #   if (!is.na(input$n_sigmas)) {
  #     z <- scale(df[sapply(df, is.numeric)])
  #     keep <- apply(abs(z) < input$n_sigmas, 1, all)
  #     rv$data_no_outliers <- df[keep, ]
  #     output$dimAfterOutlierRemoval <- renderText({
  #       paste("After outlier removal:", paste(dim(rv$data_no_outliers), collapse = " x "))
  #     })
  #   }
  # })

  #----------------------
  # Outlier removal
  #----------------------

  observeEvent(input$remove_outliers, {

    df <- if (is.null(rv$cleaned_data)) rv$selected_data else rv$cleaned_data
    req(df)

    method <- input$outlier_method
    df_num <- df[sapply(df, is.numeric)]

    keep <- rep(TRUE, nrow(df))  # default: keep all

    # --- 1. Z-score method ---
    if (method == "Z-score (SD cutoff)" && !is.na(input$n_sigmas)) {
      z <- scale(df_num)
      keep <- apply(abs(z) < input$n_sigmas, 1, all)
    }

    # --- 2. Cook's Distance ---
    if (method == "Cook's Distance") {
      try({
        formula <- as.formula(
          #paste(names(df_num)[1], "~", paste(names(df_num)[-1], collapse = "+"))
          paste(names(df_num)[1], "~", paste(names(df_num)[2], collapse = "+"))
        )
        model <- lm(formula, data = df_num)

        cooks_d <- cooks.distance(model)
        cutoff <- ifelse(is.na(input$cook_cutoff), 4 / nrow(df_num), input$cook_cutoff)

        keep <- cooks_d < cutoff
      }, silent = TRUE)
    }

    # --- 3. Mahalanobis Distance ---
    if (method == "Mahalanobis Distance") {
      try({
        center <- colMeans(df_num, na.rm = TRUE)
        cov_mat <- cov(df_num, use = "complete.obs")

        md <- mahalanobis(df_num, center, cov_mat)

        cutoff <- qchisq(input$mahal_cutoff, df = ncol(df_num))
        keep <- md < cutoff
      }, silent = TRUE)
    }

    # --- 4. Lookout (LOOCV-based Outlier Detection) ---
    if (method == "LOOCV KDE (lookout)") {
      try({
        # lookout expects a numeric matrix
        df_mat <- as.matrix(na.omit(df_num))

        # Run lookout
        res <- lookout(df_mat)

        # res$rank gives ordering (higher = more outlier)
        scores <- res$rank

        # Define threshold (top 5% most outlying points)
        cutoff <- quantile(scores, probs = 0.95, na.rm = TRUE)

        keep_sub <- scores < cutoff  # keep non-outliers

        # Map back to original rows (handle NA rows properly)
        keep <- rep(TRUE, nrow(df_num))
        keep[complete.cases(df_num)] <- keep_sub

      }, silent = TRUE)
    }

    # --- 5. DHARMa Outliers ---
    if (method == "DHARMa Outliers") {
      try({
        req(rv$models)  # assuming models already exist

        # Use first model (or adapt to selected model)
        model_obj <- unwrap_model(rv$models[[1]]$model)

        if (!inherits(model_obj, c("glmerMod","lmerMod"))) {
          warning("DHARMa requires mixed models")
        } else {

          sim_res <- simulateResiduals(model_obj, n = 500)

          # Run outlier test
          out_test <- testOutliers(sim_res)

          # Extract outlier flags
          # DHARMa stores them internally as logical vector
          outlier_flags <- out_test$outliers

          # Fallback if structure differs
          if (is.null(outlier_flags)) {
            # alternative extraction (robust fallback)
            res_vals <- sim_res$scaledResiduals
            outlier_flags <- (res_vals < 0.025 | res_vals > 0.975)
          }

          # keep <- !outlier_flags

          keep <- rep(TRUE, nrow(df))

          model_rows <- as.numeric(rownames(model.frame(model_obj)))
          keep[model_rows] <- !outlier_flags
        }

      }, silent = TRUE)
    }
    # Apply filtering
    rv$data_no_outliers <- df[keep, , drop = FALSE]

    output$dimAfterOutlierRemoval <- renderText({
      paste("After outlier removal:",
            paste(dim(rv$data_no_outliers), collapse = " x "),
            "| Removed:", sum(!keep))
    })
  })


  output$download_no_outliers <- downloadHandler(
    filename = function() {
      paste0("cleaned_no_outliers_", Sys.Date(), ".csv")
    },
    content = function(file) {
      req(rv$data_no_outliers)   # ensure data exists
      write.csv(rv$data_no_outliers, file, row.names = FALSE)
    }
  )


  #----------------------
  # Data standardization
  #----------------------

  output$standardize_vars_ui <- renderUI({
    req(rv$selected_data)

    numeric_cols <- names(rv$selected_data)[
      sapply(rv$selected_data, is.numeric)
    ]

    selectInput(
      "standardize_vars",
      "Select Variables to Standardize",
      choices = numeric_cols,
      multiple = TRUE
    )
  })

  # Apply preprocessing
  observeEvent(input$apply_standardization, {
    req(rv$selected_data)

    #df <- rv$selected_data
    df <- if (!is.null(rv$data_no_outliers)) rv$data_no_outliers else
      if (!is.null(rv$cleaned_data)) rv$cleaned_data else rv$selected_data
    req(df)


    if (!is.null(input$standardize_vars)) {

      for (v in input$standardize_vars) {

        if (input$center_scale_mode == "center_scale") {

          df[[paste0(v, "_cs")]] <-
            as.numeric(scale(df[[v]],
                             center = TRUE,
                             scale = TRUE))

        } else if (input$center_scale_mode == "center_only") {

          df[[paste0(v, "_c")]] <-
            df[[v]] - mean(df[[v]], na.rm = TRUE)

        } else if (input$center_scale_mode == "scale_only") {

          df[[paste0(v, "_s")]] <-
            df[[v]] / sd(df[[v]], na.rm = TRUE)
        }
      }
    }

    rv$selected_data <- df
    rv$processed_data <- df

    showNotification(
      "Data standardization applied successfully",
      type = "message"
    )
  })


  #----------------------
  # Inputs (dependent/independent/covariate/interaction/post-hoc)
  #----------------------

  observe({
    req(rv$selected_data)
    df <- if (!is.null(rv$data_no_outliers)) rv$data_no_outliers else
      if (!is.null(rv$cleaned_data)) rv$cleaned_data else rv$selected_data

    output$yInput <- renderUI({
      selectInput("y", "Dependent Variable (y)", choices = names(df))
    })
    output$xInput <- renderUI({
      selectInput("x", "Independent Variables (x)", choices = names(df), multiple = TRUE)
    })
    output$covariatesInput <- renderUI({
      selectInput("covariates", "Covariates (main effects)",
                  choices = names(df), multiple = TRUE)
    })
    output$interactorsInput <- renderUI({
      selectInput("interaction_vars", "Interaction Variables",
                  choices = names(df), multiple = TRUE)
    })
    output$posthoc_vars_ui <- renderUI({
      selectInput("posthoc_vars", "Factors for Post-hoc (EMMs)",
                  choices = names(Filter(is.factor, df)), multiple = TRUE)
    })
  })

  #----------------------
  # Data table + processed-data modal
  #----------------------

  output$dataTable <- renderDT({
    req(rv$data)
    datatable(rv$data, options = list(scrollX = TRUE, pageLength = 25))
  })

  observeEvent(input$open_data_view, {
    req(rv$selected_data)
    rv$processed_data

    showModal(modalDialog(
      title = "Processed Data",
      size = "l",
      easyClose = TRUE,
      DT::DTOutput("processed_table"),
      footer = modalButton("Close")
    ))
  })

  # output$processed_table <- DT::renderDT({
  #   rv$processed_data
  # })

  output$processed_table <- DT::renderDT({
    DT::datatable(
      rv$processed_data,
      options = list(
        pageLength = 25,
        scrollX = TRUE,
        scrollY = "500px",
        lengthMenu = c(10,25,50,100),
        paging = TRUE
      ),
      class = "display nowrap"
    )
  })
}
