# Load libraries
library(shiny)
library(readr)
library(readxl)
library(DT)
library(shinyjs)
library(afex)
library(lme4)
library(emmeans)
library(SuppDists)
library(goft)

# UI -----------------------------
ui <- fluidPage(
  useShinyjs(),
  titlePanel("Flexible GLMM Toolbox"),
  
  sidebarLayout(
    sidebarPanel(
      fileInput("data_file","Upload Data Frame (CSV or Excel)", 
                accept = c(".csv", ".xlsx")),
      
      hr(),
      p("Select columns to include or exclude from analysis:"),
      uiOutput("select_columns_ui"),
      actionButton("apply_column_selection", "Apply Column Selection"),
      hr(),
      
      uiOutput("factor_vars_ui"),
      uiOutput("numeric_vars_ui"),
      radioButtons("center_scale_mode", "Numeric preprocessing",
                   choices = c("Center and scale" = "center_scale",
                               "Center only" = "center_only",
                               "Scale only" = "scale_only",
                               "None" = "none"),
                   selected = "center_scale"),
      actionButton("apply_var_types", "Apply Variable Types"),
      hr(),
      
      radioButtons("missing_value", "Missing Value Treatment", 
                   choices = c("Remove rows with NA" = "NA"),
                   selected = "NA"),
      actionButton("remove_missing", "Remove Missing Values"),
      verbatimTextOutput("dimBefore"),
      verbatimTextOutput("dimAfter"),
      verbatimTextOutput("missingDataCheck"),
      hr(),
      
      numericInput("n_sigmas", "SD cutoff for Outlier Removal (leave empty to skip)", value = NA, min = 1),
      actionButton("remove_outliers", "Remove Outliers"),
      verbatimTextOutput("dimAfterOutlierRemoval"),
      hr(),
      p("Multiple y and x inputs are allowed"),
      uiOutput("yInput"),
      hr(),
      p("For fit distribution check dependent variable must be a numeric"),
      actionButton("fit_distribution", "Fit Distribution"),
      verbatimTextOutput("fitStatus"),
      uiOutput("xInput"),
      hr(),
      # Covariates and interactors (separate roles)
      
      uiOutput("interactorsInput"),
      radioButtons("interaction_mode", "Interaction Mode",
                   choices = c("None" = "none",
                               "IV * Interactor1" = "iv_x_one",
                               "IV * Interactor1 * Interactor2 (3-way)" = "iv_x_two"),
                   selected = "none"),
      hr(),
      uiOutput("covariatesInput"),
      selectInput("family", "GLMM Family", 
                  choices = c("gaussian","gamma","beta","binomial","poisson"), 
                  selected = "gaussian"),
      selectInput("linkfun", "Link Function",
                  choices = c("default",
                              "identity","log","logit","probit","cloglog",
                              "sqrt","inverse"),
                  selected = "default"),
      selectInput("engine", "Modeling Engine",
                  choices = c("afex::mixed", "lme4::glmer"),
                  selected = "afex::mixed"),
      textInput("random_effects", "Random Effects Formula",
                value = "(1|Subj)"),
      hr(),
      
      uiOutput("posthoc_vars_ui"),
      textInput("custom_eq", "Custom model equation (overrides auto)", value = ""),
      actionButton("run", "Run Models")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Instructions", 
                 
                 div(style="text-align:left; font-size: 16px",br(),
                     br(),
                     tags$b("The current version of Flexible GLMM toolbox can be used to"),br(),
                     br(),
                     "- fit GLMM models using afex and glmer R packages",br(),
                     "- get results similar to SAS outputs" ,br(),
                     "- identify fit distribution family for the dependent variables" ,br(),
                     "- run analysis for a multiple dependent and independent variables simultaneously",br(),
                     br(),
                     tags$b("Usage:"),br(),
                     br(),
                     "- Data input format: xlsx or csv file with header row containing variable names.",br(),
                     "- By default first sheet will be used as the input. Ex. mtcars, sleepstudy (lme4)",br(),
                     "- Missing values are blank/empty cells in the data",br(),
                     
                     "- For outlier removal specify the standard deviation value (ex. 3)",br(),
                     
                     "- Multiple covariates can be selected from the input data (age sex bmi)",br(), 
                     
                     "- For family of distributions Refer- https://www.rdocumentation.org/packages/stats/versions/3.6.2/topics/family",br(),
                     
                     "- Custom equation format : y ~ x1 + x2",br(),
                     br(),
                     
                     tags$b("Note:"),br(),
                     br(),
                     "- Afex mixed - Refer to https://cran.r-project.org/web/packages/afex/afex.pdf",br(),
                     
                     "- For using complex models use custom equation option.",br(),
                     
                     "- For feedback/queries, please send an email to ptalwar@uliege.be; talwar.puneet@gmail.com."
                     
                 )
        ),
        tabPanel("Data", DTOutput("dataTable")),
        tabPanel("FitDist Output",
                 verbatimTextOutput("fitDistLogs") # Logs displayed here
        ),
        tabPanel("Fit Distribution Plots",
                 h4("Distribution Fitting for Selected Dependent Variable"),
                 plotOutput("dist_descriptive"),
                 hr(),
                 plotOutput("dist_denscomp"),
                 plotOutput("dist_qqcomp"),
                 plotOutput("dist_cdfcomp"),
                 plotOutput("dist_ppcomp"),
                 hr(),
                 verbatimTextOutput("dist_gof")
        ),        
        tabPanel("Model Output", verbatimTextOutput("modelOutput")),
        tabPanel("ANOVA", verbatimTextOutput("anovaOutput")),
        tabPanel("Post-hoc (EMMs)", verbatimTextOutput("emmeansOutput"))
       )
      )
    )
  )


# SERVER -------------------------
server <- function(input, output, session) {
  
  rv <- reactiveValues(
    data = NULL, selected_data = NULL, 
    cleaned_data = NULL, data_no_outliers = NULL,
    models = NULL
  )
  
  # Upload
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
  
  # Column selector
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
  
  # Variable types
  output$factor_vars_ui <- renderUI({
    req(rv$selected_data)
    selectInput("factor_vars", "Categorical Variables",
                choices = names(rv$selected_data), multiple = TRUE)
  })
  output$numeric_vars_ui <- renderUI({
    req(rv$selected_data)
    selectInput("numeric_vars", "Numeric Variables",
                choices = names(rv$selected_data), multiple = TRUE)
  })
  
  # Apply types and correct centering/scaling
  observeEvent(input$apply_var_types, {
    req(rv$selected_data)
    df <- rv$selected_data
    if (!is.null(input$factor_vars)) {
      for (v in input$factor_vars) {
        df[[v]] <- as.factor(df[[v]])
      }
    }
    if (!is.null(input$numeric_vars)) {
      for (v in input$numeric_vars) {
        df[[v]] <- suppressWarnings(as.numeric(df[[v]]))
        if (input$center_scale_mode == "center_scale") {
          df[[v]] <- as.numeric(scale(df[[v]], center = TRUE, scale = TRUE))
        } else if (input$center_scale_mode == "center_only") {
          df[[v]] <- df[[v]] - mean(df[[v]], na.rm = TRUE)
        } else if (input$center_scale_mode == "scale_only") {
          df[[v]] <- df[[v]] / sd(df[[v]], na.rm = TRUE)
        }
      }
    }
    rv$selected_data <- df
    showNotification('Variable types & preprocessing applied', type = 'message')
  })
  
  # Remove missing values
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
  
  # Outlier removal
  observeEvent(input$remove_outliers, {
    df <- if (is.null(rv$cleaned_data)) rv$selected_data else rv$cleaned_data
    if (!is.na(input$n_sigmas)) {
      z <- scale(df[sapply(df, is.numeric)])
      keep <- apply(abs(z) < input$n_sigmas, 1, all)
      rv$data_no_outliers <- df[keep, ]
      output$dimAfterOutlierRemoval <- renderText({
        paste("After outlier removal:", paste(dim(rv$data_no_outliers), collapse = " x "))
      })
    }
  })
  
  # Inputs
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
  
  # Run models
  runModels <- eventReactive(input$run, {
    #req(rv$selected_data, input$y, input$x, input$random_effects)
    req(rv$selected_data, input$y)
    df <- if (!is.null(rv$data_no_outliers)) rv$data_no_outliers else 
      if (!is.null(rv$cleaned_data)) rv$cleaned_data else rv$selected_data
    
    # y <- input$y
    # x_vars <- input$x
    # covs <- input$covariates
    # inters <- input$interaction_vars
    # rand_terms <- input$random_effects
    # family <- switch(input$family,
    #                  "gaussian" = gaussian,
    #                  "gamma" = gamma,
    #                  #"beta" = beta,
    #                  "binomial" = binomial,
    #                  "poisson"  = poisson)
    
    # Family + link function builder
    get_family <- function(fam, linkfun) {
      if (linkfun == "default") {
        return(
          switch(fam,
                 "gaussian" = gaussian(),
                 "gamma" = Gamma(),
                 #"beta" = beta(),   
                 "binomial" = binomial(),
                 "poisson" = poisson())
        )
      } else {
        return(
          switch(fam,
                 "gaussian" = gaussian(link = linkfun),
                 "gamma" = Gamma(link = linkfun),
                 #"beta" = beta(link = linkfun),
                 "binomial" = binomial(link = linkfun),
                 "poisson" = poisson(link = linkfun))
        )
      }
    }
    
    family <- get_family(input$family, input$linkfun)
    
    
    custom_eq <- trimws(input$custom_eq)
    #mode <- input$interaction_mode
    
    results <- list()
    
    # --- CASE 1: Custom equation provided ---
    if (nzchar(custom_eq)) {
      f_str <- if (grepl("~", custom_eq)) custom_eq else paste(input$y, "~", custom_eq)
      if (!grepl("\\|", f_str) && nzchar(input$random_effects)) {
        f_str <- paste(f_str, "+", input$random_effects)
      }
      f <- as.formula(f_str)
      
      tryCatch({
        if (input$engine == "afex::mixed") {
          if (identical(family, gaussian)) {
            model <- mixed(f, data = df, method = "KR")
            anova_tab <- anova(model, ddf = "Kenward-Roger", type = 3)
          } else {
            model <- mixed(f, data = df, family = family, method = "LRT")
                           # ,control = lmerControl(optCtrl = list(maxfun = 1e6)), 
                           # expand_re = TRUE)
            anova_tab <- anova(model)
          }
        } else if (input$engine == "lme4::glmer") {
          model <- glmer(f, data = df, family = family,
                         control = glmerControl(optimizer = "bobyqa",
                                                optCtrl = list(maxfun = 2e5)))
          anova_tab <- anova(model)
        }
        results[[custom_eq]] <- list(formula = f_str, model = model, anova = anova_tab)
      }, error = function(e) {
        results[[custom_eq]] <- list(formula = f_str, error = e$message)
      })
    }
    
    # --- CASE 2: No custom equation, loop through IVs ---
    else {
      x_vars <- input$x 
      covs <- input$covariates 
      inters <- input$interaction_vars 
      rand_terms <- ifelse(nzchar(input$random_effects), input$random_effects, "1")
      mode <- input$interaction_mode
      
      for (iv in x_vars) {
        rhs <- iv
        if (length(covs) > 0) rhs <- paste(rhs, "+", paste(covs, collapse = " + "))
        
        if (length(inters) > 0 && mode != "none") {
          if (mode == "iv_x_one") {
            rhs <- paste(rhs, "+", paste(paste0(iv, "*", inters), collapse = " + "))
          } else if (mode == "iv_x_two" && length(inters) >= 2) {
            rhs <- paste(rhs, "+", paste0(iv, "*", inters[1], "*", inters[2]))
          }
        }
        
        f_str <- paste(input$y, "~", rhs, "+", rand_terms)
        f <- as.formula(f_str)
        
        tryCatch({
          if (input$engine == "afex::mixed") {
            
            # AFEX rule:
            # gaussian  → method = "KR"
            # non-gaussian → family only (no link), method = "LRT"
            
            fam_name <- input$family
            
            if (fam_name == "gaussian") {
              model <- mixed(f, data = df, method = "KR")
              anova_tab <- anova(model, ddf = "Kenward-Roger", type = 3)
            } else {
              # Remove link (AFEX does NOT support custom links)
              base_family <- switch(
                fam_name,
                "gamma" = Gamma(),
                "binomial" = binomial(),
                "poisson" = poisson()
              )
              model <- mixed(f, data = df, family = base_family, method = "LRT")
              anova_tab <- anova(model)
            }
          } else if (input$engine == "lme4::glmer") {
            model <- glmer(f, data = df, family = family,
                           control = glmerControl(optimizer = "bobyqa",
                                                  optCtrl = list(maxfun = 2e5)))
            anova_tab <- anova(model)
          }
          results[[iv]] <- list(formula = f_str, model = model, anova = anova_tab)
        }, error = function(e) {
          results[[iv]] <- list(formula = f_str, error = e$message)
        })
      }
    }
    
    rv$models <- results
    results
})
  
  
  # Outputs
  output$dataTable <- renderDT({
    req(rv$selected_data)
    datatable(rv$selected_data, options = list(scrollX = TRUE, pageLength = 15))
  })
  
  output$modelOutput <- renderPrint({
    results <- runModels()
    if (length(results) == 0) return("No models fitted yet.Check input variables and distribution")
    
    for (nm in names(results)) {
      cat("\n--- Model:", nm, "---\n")
      res <- results[[nm]]
      cat("Formula:", res$formula, "\n")
      if (!is.null(res$model)) {
        print(summary(res$model))
      } else {
        cat("Error:", res$error, "\n")
      }
    }
  })
  
  output$anovaOutput <- renderPrint({
    results <- runModels()
    if (length(results) == 0) return("No ANOVA results available.")
    
    for (nm in names(results)) {
      cat("\n--- ANOVA for:", nm, "---\n")
      res <- results[[nm]]
      cat("Formula:", res$formula, "\n")
      if (!is.null(res$anova)) {
        print(res$anova)
      } else {
        cat("Error:", res$error, "\n")
      }
    }
  })
  
  output$emmeansOutput <- renderPrint({
    results <- runModels()
    ph_vars <- input$posthoc_vars
    for (nm in names(results)) {
      cat("\n--- EMMs for:", nm, "---\n")
      res <- results[[nm]]
      if (is.list(res) && length(ph_vars) > 0) {
        for (fac in ph_vars) {
          cat("\nFactor:", fac, "\n")
          print(emmeans(res$model, specs = fac))
        }
      } else {
        cat("No post-hoc factors selected or available.\n")
      }
    }
  })
  
  # # ------------------------------------------------------------------
  # # Distribution fitting that runs ONLY for the selected dependent variable
  # # ------------------------------------------------------------------

  
  observeEvent(input$fit_distribution, {
    
    df <- isolate(
      if (is.null(rv$data_no_outliers))
        if (is.null(rv$cleaned_data)) rv$selected_data else rv$cleaned_data
      else rv$data_no_outliers
    )
    
    varname <- input$y
    z <- df[[varname]]
    
    if (!is.numeric(z)) {
      output$fitStatus <- renderText("Dependent variable must be numeric.")
      return(NULL)
    }
    
    z1 <- na.exclude(z)
    if (length(z1) < 5) {
      output$fitStatus <- renderText("Not enough numeric data to fit distributions.")
      return(NULL)
    }
    
    # ---- Fit 4 Standard Distributions ----
    fn  <- fitdist(z1, "norm",   method = "mme")
    fg  <- fitdist(z1, "gamma",  method = "mme")
    fln <- fitdist(z1, "lnorm",  method = "mme")
    fw  <- fitdist(z1, "weibull", method = "mle")
    
    rv$dist_fits <- list(
      Normal    = fn,
      Lognormal = fln,
      Gamma     = fg,
      Weibull   = fw
    )
    
    # ▶ Log output for FitDist Output tab
    logs <- c(
      paste0("===== Dependent Variable: ", varname, " ====="),
      "",
      "---- Normal ----",
      paste(capture.output(summary(fn)), collapse = "\n"),
      "",
      "---- Lognormal ----",
      paste(capture.output(summary(fln)), collapse = "\n"),
      "",
      "---- Gamma ----",
      paste(capture.output(summary(fg)), collapse = "\n"),
      "",
      "---- Weibull ----",
      paste(capture.output(summary(fw)), collapse = "\n"),
      "",
      "---- Goodness-of-fit ----",
      paste(capture.output(gofstat(rv$dist_fits)), collapse = "\n")
    )
    
    output$fitDistLogs <- renderText(paste(logs, collapse="\n"))
    output$fitStatus   <- renderText("Distribution fitting complete.")
    
  })
  
  
  
  output$dist_descriptive <- renderPlot({
    req(rv$dist_fits, input$y)
    
    # Get currently selected y from the same data source
    df <- if (!is.null(rv$data_no_outliers)) rv$data_no_outliers else 
      if (!is.null(rv$cleaned_data)) rv$cleaned_data else rv$selected_data
    
    z <- suppressWarnings(as.numeric(df[[input$y]]))
    z <- na.exclude(z)
    
    if (length(z) < 5) return(NULL)
    
    descdist(z, boot = 500)
  })
  
  
  output$dist_denscomp <- renderPlot({
    req(rv$dist_fits)
    denscomp(rv$dist_fits, legendtext=c("normal","lognormal","gamma","weibull"))
  })
  
  output$dist_qqcomp <- renderPlot({
    req(rv$dist_fits)
    qqcomp(rv$dist_fits, legendtext=c("normal","lognormal","gamma","weibull"))
  })
  
  output$dist_cdfcomp <- renderPlot({
    req(rv$dist_fits)
    cdfcomp(rv$dist_fits, legendtext=c("normal","lognormal","gamma","weibull"))
  })
  
  output$dist_ppcomp <- renderPlot({
    req(rv$dist_fits)
    ppcomp(rv$dist_fits, legendtext=c("normal","lognormal","gamma","weibull"))
  })
  
  output$dist_gof <- renderPrint({
    req(rv$dist_fits)
    gofstat(rv$dist_fits)
  })
}

# Run
shinyApp(ui = ui, server = server)
