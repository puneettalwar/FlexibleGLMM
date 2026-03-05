#**************************************************************
# by Puneet Talwar
# Dec 2025

# R 4.5.0
#**************************************************************
# A shiny app to fit GLMM models using afex and glmer packages.
# It provides results similar to SAS outputs.
# It provides diagnostic plots and post-hoc comparisons
#**************************************************************

#**************************************************************
# Issue with performance package (check_model) deployment (due to patchwork package) on shiny server [Didn't work]
#**************************************************************
# Run when getwd() is shinyapp folder (Project folder)
# renv::load()
# renv::init()
# renv::snapshot()

# renv::deactivate()

# Deploy with (if there is package version issue):
# renv.lock included
# rsconnect::deployApp()

# ---- FORCE renv activation (Shiny Server safe) ----
# if (file.exists("renv/activate.R")) {
#   source("renv/activate.R")
# }

# activate(project = NULL, profile = NULL)
# deactivate(project = NULL, clean = FALSE)
#**************************************************************

# Load libraries
suppressPackageStartupMessages({
library(shiny)
#library(shinylogs) #  does not work on shinyapps.io
library(readr)
library(readxl)
library(DT)
library(shinyjs)
library(afex)
library(lme4)
library(emmeans)
library(SuppDists)
library(goft)
library(parameters)
library(modelsummary)
library(mctest)
library(dataPreparation)
library(ggplot2)
library(nlme)
library(knitr)
library(future)
library(performance)
library(predictmeans)
#library(HLMdiag)
#library(sjPlot)
#library(JWileymisc)
#library(multilevelTools)
})


options(width = 200)
options(shiny.maxRequestSize=30*1024^2) # Maximum upload file size 30 MB


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
      
      actionButton("open_data_view", "View Processed Data"),
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
      downloadButton("download_no_outliers", "Download Cleaned CSV"),
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
                  choices = c("afex::mixed","lme4::glmer","nlme::lme"),
                  selected = "afex::mixed"),
      
      selectInput("corStruct","Correlation structure (nlme)",
                  choices = c(
                    "None" = "none",
                    "Auto (recommended)" = "auto",
                    "AR(1)" = "corAR1",
                    "Compound Symmetry" = "corCompSymm",
                    "Unstructured (Symmetric)" = "corSymm",
                    "Exponential"="corExp"
                  )),
      
      textInput("random_effects",
                "Random Effects (lme4 / afex syntax)",
                value = "(1|Subject)"),
      
      textInput("nlme_random",
                "Random Effects (nlme syntax)",
                value = "~1|Subject"),
      
      textInput("group_var","Optional Random Grouping variable for Auto correlation (e.g., Subject)"),
      textInput("time_var","Optional Time variable for AR structures (e.g., trial, block, day, session)"),
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
        tabPanel(
          "Summary Table",
          uiOutput("summary_tables_ui")
        ),
        tabPanel(
          "Performance",
          uiOutput("performance_ui")
        ),
        tabPanel("Post-hoc (EMMs)", verbatimTextOutput("emmeansOutput")),
        tabPanel("Summary Plots",
                 h4("Boxplots with Pairwise t-tests"),
                 uiOutput("boxplot_var_selector"),
                 plotOutput("boxplot_output"),
                 verbatimTextOutput("t_test_output"),
                 hr(),
                 h4("Spearman Correlation Plots"),
                 uiOutput("corr_iv_selector"),
                 plotOutput("corr_plot"),
                 verbatimTextOutput("corr_stats"),
                 downloadButton("download_corr_plot", "Download Correlation Plot")
        )
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
  
  observeEvent(input$apply_var_types, {
    req(rv$selected_data)
    df <- rv$selected_data
    
    # Factors
    if (!is.null(input$factor_vars)) {
      for (v in input$factor_vars) 
        df[[v]] <- as.factor(df[[v]])
    }
    
    # Numeric
    if (!is.null(input$numeric_vars)) {
      for (v in input$numeric_vars) {
        df[[v]] <- suppressWarnings(as.numeric(df[[v]]))
        
        if (input$center_scale_mode == "center_scale") {
          df[[paste0(v, "_cs")]] <- as.numeric(scale(df[[v]], center = TRUE, scale = TRUE))
        }
        else if (input$center_scale_mode == "center_only") {
          df[[paste0(v, "_c")]] <- df[[v]] - mean(df[[v]], na.rm = TRUE)
        }
        else if (input$center_scale_mode == "scale_only") {
          df[[paste0(v, "_s")]] <- df[[v]] / sd(df[[v]], na.rm = TRUE)
        }
      }
    }
    
    rv$processed_data <- df      # <-- store the augmented data
    rv$selected_data <- df
    
    showNotification("Variable types & preprocessing applied", type = "message")
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
  
  output$download_no_outliers <- downloadHandler(
    filename = function() {
      paste0("cleaned_no_outliers_", Sys.Date(), ".csv")
    },
    content = function(file) {
      req(rv$data_no_outliers)   # ensure data exists
      write.csv(rv$data_no_outliers, file, row.names = FALSE)
    }
  )
  
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
  
  # -------------------------------
  # nlme helpers (TIME-AWARE FINAL)
  # -------------------------------
  
  get_nlme_subject <- function(random_formula) {
    if (!nzchar(random_formula)) return(NULL)
    gsub("^~.*\\|\\s*([^/]+).*$", "\\1", random_formula)
  }
  
  prepare_nlme_data <- function(df, subject_var, time_var = NULL) {
    
    df[[subject_var]] <- as.factor(df[[subject_var]])
    
    # Order by subject AND time if provided
    if (!is.null(time_var) && nzchar(time_var) && time_var %in% names(df)) {
      df <- df[order(df[[subject_var]], df[[time_var]]), ]
    } else {
      df <- df[order(df[[subject_var]]), ]
    }
    
    # Fallback index (only used if no time_var)
    df$.trial_index <- ave(seq_len(nrow(df)), df[[subject_var]], FUN = seq_along)
    
    df
  }
  
  
  nlme_can_use_correlation <- function(df, subject_var, time_var = NULL) {
    
    grp_sizes <- table(df[[subject_var]])
    
    enough_repeats <- sum(grp_sizes >= 2) >= 2
    
    if (!enough_repeats) return(FALSE)
    
    # If time_var supplied, ensure it varies within subject
    if (!is.null(time_var) && nzchar(time_var) && time_var %in% names(df)) {
      
      within_var <- tapply(df[[time_var]], df[[subject_var]], function(x)
        length(unique(x)) > 1
      )
      
      return(any(within_var))
    }
    
    TRUE
  }
  
  
  suggest_correlation_structure <- function(df, subject_var, time_var = NULL) {
    
    if (!nlme_can_use_correlation(df, subject_var, time_var)) {
      return("none")
    }
    
    if (!is.null(time_var) && nzchar(time_var) && time_var %in% names(df)) {
      return("corAR1")
    }
    
    return("corCompSymm")
  }
  
  
  get_nlme_corStruct <- function(type, groupvar, timevar = NULL) {
    
    if (is.null(type) || type == "none" || !nzchar(type)) {
      return(NULL)
    }
    
    switch(
      type,
      
      # AR(1) — USE time variable if available
      corAR1 = {
        if (!is.null(timevar) && nzchar(timevar)) {
          corAR1(form = as.formula(paste("~", timevar, "|", groupvar)))
        } else {
          corAR1(form = as.formula(paste("~ .trial_index |", groupvar)))
        }
      },
      
      # Compound symmetry
      corCompSymm = {
        corCompSymm(form = as.formula(paste("~ 1 |", groupvar)))
      },
      
      # Unstructured symmetric
      corSymm = {
        if (!is.null(timevar) && nzchar(timevar)) {
          corSymm(form = as.formula(paste("~", timevar, "|", groupvar)))
        } else {
          corSymm(form = as.formula(paste("~ .trial_index |", groupvar)))
        }
      },
      
      # Continuous-time exponential
      corExp = {
        if (is.null(timevar) || !nzchar(timevar)) {
          stop("corExp requires a numeric time variable.")
        }
        corExp(form = as.formula(paste("~", timevar, "|", groupvar)))
      },
      
      NULL
    )
  }
  
  
  strip_lme4_random <- function(formula_string) {
    gsub("\\+?\\s*\\([^\\)]*\\|[^\\)]*\\)", "", formula_string)
  }
  
  
  # Run models
  runModels <- eventReactive(input$run, {
    #req(rv$selected_data, input$y, input$x, input$random_effects)
    req(rv$selected_data, input$y)
    df <- if (!is.null(rv$data_no_outliers)) rv$data_no_outliers else 
      if (!is.null(rv$cleaned_data)) rv$cleaned_data else rv$selected_data
    

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
      # if (!grepl("\\|", f_str) && nzchar(input$random_effects)) {
      #   f_str <- paste(f_str, "+", input$random_effects)
      # }
      if (input$engine != "nlme::lme") {
        if (!grepl("\\|", f_str) && nzchar(input$random_effects)) {
          f_str <- paste(f_str, "+", input$random_effects)
        }
      }
      f <- as.formula(f_str)
      
      tryCatch({
        if (input$engine == "afex::mixed") {
          if (input$family == "gaussian") {
            model <- mixed(f, data = df, method = "KR")
            anova_tab <- anova(model, ddf = "Kenward-Roger", type = 3)
          }        
        } else if (input$engine == "lme4::glmer") {
          base_family <- switch(
            input$family,
            "gamma"    = Gamma(),
            "binomial" = binomial(),
            "poisson"  = poisson()
          )
          model <- mixed(f, data = df, family = base_family, method = "LRT")
          anova_tab <- anova(model)
        }else if(input$engine == "nlme::lme") {
          if (input$family != "gaussian")
            stop("nlme::lme only supports Gaussian models.")
          
          # --- Random effects: nlme ONLY ---
          nlme_random <- trimws(input$nlme_random)
          
          random_formula <- if (nzchar(nlme_random)) {
            as.formula(nlme_random)
          } else {
            as.formula(paste0("~1|", input$group_var))
          }
          
          # --- Extract top-level subject ---
          subject_var <- get_nlme_subject(
            if (nzchar(nlme_random)) nlme_random else paste0("~1|", input$group_var)
          )
          
          df <- prepare_nlme_data(df, subject_var)
          
          # --- Correlation handling ---
          can_use_corr <- nlme_can_use_correlation(df, subject_var)
          
          suggested_corr <- suggest_correlation_structure(
            df,
            subject_var = subject_var,
            time_var = input$time_var
          )
          
          cor_choice <- input$corStruct
          if (cor_choice == "auto") cor_choice <- suggested_corr
          
          correlation <- NULL
          corr_used <- "none"
          
          if (can_use_corr && cor_choice != "none") {
            correlation <- get_nlme_corStruct(
              cor_choice,
              subject_var,
              input$time_var
            )
            corr_used <- cor_choice
          }
          
          if (!can_use_corr && cor_choice != "none") {
            showNotification(
              "Residual correlation disabled: insufficient repeated measures.",
              type = "warning"
            )
          }
          
          showNotification(
            paste("Correlation used:", corr_used),
            type = "message",
            duration = 4
          )
          
          # --- FIXED formula ONLY (no +1, no random terms) ---
          fixed_str <- strip_lme4_random(f_str)
          #fixed_str <- gsub("\\+\\s*1$", "", strip_lme4_random(f_str))
          
          
          model <- nlme::lme(
            fixed = as.formula(fixed_str),
            random = random_formula,
            correlation = correlation,
            data = df,
            method = "REML"
          )
          
          anova_tab <- anova(model)
        }
        
        results[[custom_eq]] <- list(engine  = input$engine,formula = f_str, model = model, anova = anova_tab)
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
        
        #f_str <- paste(input$y, "~", rhs, "+", rand_terms)
        if (input$engine == "nlme::lme") {
          f_str <- paste(input$y, "~", rhs)
        } else {
          #rand_terms <- ifelse(nzchar(input$random_effects), input$random_effects, "1")
          f_str <- paste(input$y, "~", rhs, "+", rand_terms)
        }
        f <- as.formula(f_str)
        
        tryCatch({
          if (input$engine == "afex::mixed") {
            
            # AFEX rule:
            # gaussian method = "KR"
            # non-gaussian family only (no link), method = "LRT"
            
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
          } 
          else if (input$engine == "lme4::glmer") {
            model <- glmer(f, data = df, family = family,
                           control = glmerControl(optimizer = "bobyqa",
                                                  optCtrl = list(maxfun = 2e5)))
            anova_tab <- anova(model)
          } else if (input$engine == "nlme::lme") {
            
            if (input$family != "gaussian")
              stop("nlme::lme only supports Gaussian models.")
            
            # --- Random effects: nlme ONLY ---
            nlme_random <- trimws(input$nlme_random)
            
            random_formula <- if (nzchar(nlme_random)) {
              as.formula(nlme_random)
            } else {
              as.formula(paste0("~1|", input$group_var))
            }
            
            # --- Extract top-level subject ---
            subject_var <- get_nlme_subject(
              if (nzchar(nlme_random)) nlme_random else paste0("~1|", input$group_var)
            )
            
            df <- prepare_nlme_data(df, subject_var)
            
            # --- Correlation handling ---
            can_use_corr <- nlme_can_use_correlation(df, subject_var,input$time_var)
            
            suggested_corr <- suggest_correlation_structure(
              df,
              subject_var = subject_var,
              time_var = input$time_var
            )
            
            cor_choice <- input$corStruct
            if (cor_choice == "auto") cor_choice <- suggested_corr
            
            correlation <- NULL
            corr_used <- "none"
            
            if (can_use_corr && cor_choice != "none") {
              correlation <- get_nlme_corStruct(
                cor_choice,
                subject_var,
                input$time_var
              )
              corr_used <- cor_choice
            }
            
            if (!can_use_corr && cor_choice != "none") {
              showNotification(
                "Residual correlation disabled: insufficient repeated measures.",
                type = "warning"
              )
            }
            
            showNotification(
              paste("Correlation used:", corr_used),
              type = "message",
              duration = 4
            )
            
            # --- FIXED formula ONLY (no +1, no random terms) ---
            fixed_str <- strip_lme4_random(f_str)
            #fixed_str <- gsub("\\+\\s*1$", "", strip_lme4_random(f_str))
            
            model <- nlme::lme(
              fixed = as.formula(fixed_str),
              random = random_formula,
              correlation = correlation,
              data = df,
              method = "REML"
            )
            
            anova_tab <- anova(model)
          }
          
          results[[iv]] <- list(engine  = input$engine,formula = f_str, model = model, anova = anova_tab)

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
  
  output$processed_table <- DT::renderDT({
    rv$processed_data    
  })
  
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
  
  #datatable(df, options = list(scrollX = TRUE, pageLength = 20))
  
  output$modelOutput <- renderPrint({
    results <- runModels()
    if (length(results) == 0) return("No models fitted yet.Check input variables and distribution")
    
    for (nm in names(results)) {
      res <- results[[nm]]
      
      cat("\n--- Model:", nm, "---\n")
      #cat("Formula:", res$formula, "\n")
      if (!is.null(res$formula)) {
        cat("Formula:", res$formula, "\n")
      }
      if (!is.null(res$fixed)) {
        cat("Engine:", res$engine, "\n")
        cat("Fixed:", res$fixed, "\n")
        cat("Random:", res$random, "\n")
        cat("Correlation:", res$correlation, "\n")
        
      }
      
      
      if (!is.null(res$model)) {
        
        print(summary(res$model))
        #vcov(summary(res$model)))
        
        if (inherits(res$model, "lme")) {
          cat("\nCorrelation structure:\n")
          print(res$model$modelStruct$corStruct)
        }
        
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
      #cat("Formula:", res$formula, "\n")
      if (!is.null(res$formula)) {
        cat("Formula:", res$formula, "\n")
      }
      if (!is.null(res$fixed)) {
        cat("Engine:", res$engine, "\n")
        cat("Fixed:", res$fixed, "\n")
        cat("Random:", res$random, "\n")
        cat("Correlation:", res$correlation, "\n")
        print(res$anova)
        
      }
      
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
        
        ## --- Main effects + pairwise contrasts ---
        for (fac in ph_vars) {
          cat("\nFactor:", fac, "\n")
          
          # EMMs
          emmeans_model <- emmeans(res$model, specs = fac)
          print(emmeans_model)
          
          # Pairwise contrasts
          cat("\nPairwise contrasts for:", fac, "\n")
          contrast_results <- contrast(emmeans_model, method = "pairwise")
          contrast_results_tukey <- contrast(emmeans_model, method = "pairwise",adjust="tukey")
          print(summary(contrast_results))
          print(summary(contrast_results_tukey))
        }
        
        ## --- Conditional pairwise comparisons for ALL combinations ---
        if (length(ph_vars) >= 2) {
          combs <- combn(ph_vars, 2, simplify = FALSE)
          
          for (pair in combs) {
            facA <- pair[1]
            facB <- pair[2]
            
            ## A | B
            cat(
              "\nConditional pairwise comparisons (",
              facA, " | ", facB, "):\n", sep = ""
            )
            
            form1 <- as.formula(paste("pairwise ~", facA, "|", facB))
            pw1 <- emmeans(res$model, form1)
            print(pw1)
            cat("\nTukey adjustment\n")
            cat("\n############## Note:Tukey will be changed to Sidak for one set of pairwise comparisons #############\n")
            print(pw1,adjust="tukey")
            
            ## B | A
            cat(
              "\nConditional pairwise comparisons (",
              facB, " | ", facA, "):\n", sep = ""
            )
            
            form2 <- as.formula(paste("pairwise ~", facB, "|", facA))
            pw2 <- emmeans(res$model, form2)
            print(pw2)
            cat("\nTukey adjustment\n")
            cat("\n############## Note:Tukey will be changed to Sidak for one set of pairwise comparisons #############\n")
            print(summary(pw2, adjust = "tukey"))
          }
        }
      } else {
        cat("No post-hoc factors selected or available.\n")
      }
    }
  })
  
  extract_table <- function(model) {
    
    if (inherits(model, "mixed")) {
      out <- as.data.frame(anova(model))
      out$Effect <- rownames(out)
      rownames(out) <- NULL
      out
      
    } else if (inherits(model, "glmerMod") || inherits(model, "lme")) {
      broom.mixed::tidy(model, effects = "fixed")
      
    } else {
      NULL
    }
  }
  
  clean_names <- function(df) {
    dplyr::rename_with(
      df,
      ~ gsub("\\.", " ", tools::toTitleCase(.x))
    )
  }
  
  
  
  format_table <- function(df) {
    df |>
      dplyr::mutate(
        dplyr::across(where(is.numeric), ~ round(.x, 4))
      )
  }
  
  
  observe({
    results <- runModels()
    req(results)
    
    for (nm in names(results)) {
      local({
        name <- nm
        res  <- results[[name]]
        
        if (is.null(res$model)) return()
        
        output[[paste0("summary_", make.names(name))]] <- renderUI({
          
          tab <- extract_table(res$model)
          req(tab, nrow(tab) > 0)
          
          tab <- dplyr::mutate(
            tab,
            dplyr::across(where(is.numeric), ~ round(.x, 4))
          )
          
          HTML(
            knitr::kable(
              tab,
              format  = "html",
              caption = paste("Model results:", name),
              align   = "l"
            ) |>
              kableExtra::kable_styling(
                bootstrap_options = c("striped", "hover", "condensed"),
                full_width = TRUE,
                font_size = 16
              )
          )
        })
      })
    }
  })
  
  output$summary_tables_ui <- renderUI({
    results <- runModels()
    req(results)
    
    tabs <- lapply(names(results), function(nm) {
      res <- results[[nm]]
      if (is.null(res$model)) return(NULL)
      
      tabPanel(
        nm,
        uiOutput(paste0("summary_", make.names(nm)))
      )
    })
    
    do.call(tabsetPanel, tabs)
  })
  
  unwrap_model <- function(model) {
    
    # afex::mixed
    if (inherits(model, "mixed")) {
      
      # Newer afex
      if (!is.null(model$full_model)) {
        return(model$full_model)
      }
      
      # Older afex
      if (!is.null(model$merMod)) {
        return(model$merMod)
      }
      
      # Fallback
      if (!is.null(model$model)) {
        return(model$model)
      }
    }
    
    # lme4 / nlme models pass through
    model
  }
  
  # ------------------------------------------------------------------
  # Model Diagnostics
  # ------------------------------------------------------------------
  
  check_singularity_flag <- function(model) {
    if (inherits(model, c("lmerMod", "glmerMod"))) {
      isSingular(model, tol = 1e-5)
    } else {
      NA
    }
  }
  
  check_convergence_flag <- function(model) {
    
    # lme4 / glmer
    if (inherits(model, c("lmerMod", "glmerMod"))) {
      optinfo <- model@optinfo
      if (!is.null(optinfo$conv$lme4$messages)) {
        return(paste(optinfo$conv$lme4$messages, collapse = "; "))
      }
      return("OK")
    }
    
    # nlme
    if (inherits(model, "lme")) {
      if (!is.null(model$fail) && model$fail) {
        return("Model failed to converge")
      }
      return("OK")
    }
    
    "OK"
  }
  
  observe({
    results <- runModels()
    req(results)
    
    for (nm in names(results)) {
      local({
        
        safe_name <- make.names(nm)
        res <- results[[nm]]
        
        if (is.null(res$model)) return()
        
        model_obj <- unwrap_model(res$model)
        
        # ---- Diagnostic flags ----
        output[[paste0("diagnostic_flags_", safe_name)]] <- renderPrint({
          
          singular_flag <- check_singularity_flag(model_obj)
          convergence   <- check_convergence_flag(model_obj)
          
          cat("=== Diagnostic Flags ===\n")
          
          if (!is.na(singular_flag)) {
            cat(
              "Singular fit:",
              ifelse(singular_flag, "YES ⚠️", "NO ✅"),
              "\n"
            )
          } else {
            cat("Singular fit: N/A (nlme)\n")
          }
          
          cat("Convergence:", convergence, "\n")
        })
    
        # ---- Performance metrics ----
        output[[paste0("perf_metrics_", safe_name)]] <- renderPrint({
          
          if (inherits(model_obj, "lme")) {
            data.frame(
              AIC    = AIC(model_obj),
              BIC    = BIC(model_obj),
              logLik = as.numeric(logLik(model_obj)),
              sigma  = model_obj$sigma,
              nobs   = nobs(model_obj)
            )
          } else {
            performance::model_performance(model_obj)
          }
        })
        
        output[[paste0("check_model_", safe_name)]] <- renderPlot({

          if (inherits(model_obj, "lme")) {
            #nlme::plot.lme(model_obj, resid(., type = "p") ~ fitted(.))
            #multilevelTools::modelDiagnostics(model_obj)
            
            predictmeans::residplot(
              model_obj,
              level = 1,
              id = FALSE,
              newwd = FALSE,
              ask = FALSE
            )
            #invisible(NULL)
            
          } else {
            #performance::check_model(model_obj) #Update packages "patchwork" and "parameters" if there is plot window error
            #HLMdiag::residplot(model_obj, type = "all")
            predictmeans::residplot(
              model_obj,
              level = 1,
              id = FALSE,
              newwd = FALSE,
              ask = FALSE
            )
            
          }
        })
      })
    }
  })     
        
        
  output$performance_ui <- renderUI({
    results <- runModels()
    req(results)
    
    tabs <- lapply(names(results), function(nm) {
      
      tabPanel(
        nm,
        
        h4("Diagnostic flags"),
        verbatimTextOutput(
          outputId = paste0("diagnostic_flags_", make.names(nm))
        ),
        hr(),
        
        h4("Performance metrics"),
        verbatimTextOutput(
          outputId = paste0("perf_metrics_", make.names(nm))
        ),
        hr(),
        
        h4("Model checks"),
        plotOutput(
          outputId = paste0("check_model_", make.names(nm)),
          height = "900px"
        )
      )
    })
    
    do.call(tabsetPanel, tabs)
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
    
    # â¶ Log output for FitDist Output tab
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
