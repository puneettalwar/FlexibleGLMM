#**************************************************************
# app_ui.R
# Flexible GLMM Toolbox - UI definition
#
#**************************************************************

app_ui <- function(request) {
  fluidPage(
    useShinyjs(),
    titlePanel("Flexible GLMM Toolbox"),

    sidebarLayout(
      sidebarPanel(
        fileInput("data_file","Upload Data Frame (CSV or Excel)",
                  accept = c(".csv", ".xlsx")),
        p("Select columns to include or exclude from analysis:"),
        uiOutput("select_columns_ui"),
        actionButton("apply_column_selection", "Apply Column Selection"),
        hr(),
        uiOutput("factor_vars_ui"),
        uiOutput("numeric_vars_ui"),
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
        selectInput("outlier_method", "Outlier Detection Method",
                    choices = c("None",
                                "Z-score (SD cutoff)",
                                "Cook's Distance",
                                "Mahalanobis Distance",
                                "LOOCV KDE (lookout)",
                                "DHARMa")
        ),
        numericInput("n_sigmas", "SD cutoff (Z-score)", value = NA, min = 1),
        numericInput("cook_cutoff", "Cook's Distance cutoff (default = 4/n)", value = NA),
        numericInput("mahal_cutoff", "Mahalanobis cutoff (Chi-square quantile, e.g. 0.99)", value = 0.99, min = 0.5, max = 0.999),
        actionButton("remove_outliers", "Remove Outliers"),
        verbatimTextOutput("dimAfterOutlierRemoval"),
        downloadButton("download_no_outliers", "Download Cleaned CSV"),
        hr(),
        uiOutput("standardize_vars_ui"),
        uiOutput("standardize_vars"),
        radioButtons("center_scale_mode", "Numeric preprocessing",
                     choices = c("Center and scale" = "center_scale",
                                 "Center only" = "center_only",
                                 "Scale only" = "scale_only",
                                 "None" = "none"),
                     selected = "center_scale"),
        actionButton("apply_standardization", "Apply data standardization"),
        actionButton("open_data_view", "View Processed Data"),
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

                   ),
                   br(),

                   # Image now served from inst/app/www via addResourcePath("www", ...)
                   # in run_app(), so the src is "www/<file>", not a bare filename.
                   tags$div(
                     style = "text-align:left;",
                     tags$img(src = "www/GLMM_Table.jpg",
                              width = "80%",
                              style = "border-radius:10px; box-shadow: 2px 2px 10px #ccc;")
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
          ),
          tabPanel("Diagnostics",
                   uiOutput("diagnostics_ui")
          ),
          tabPanel("Auto Report",
                   downloadButton("download_report", "Download HTML Report")
          )
        )
      )
    )
  )
}
