#**************************************************************
# app_server.R
# Flexible GLMM Toolbox - main server function.
#
# All actual reactive logic lives in the module files below
# (data_processing.R, model_fit.R, model_summary.R, diagnostics.R,
# distributions.R, emmeans.R, plotting.R). This file just creates
# the shared reactiveValues store, builds the shared `runModels`
# reactive, and wires every module together. Since these are all
# in R/, they're available automatically once the package is
# loaded/attached - no source() calls needed.
#**************************************************************

app_server <- function(input, output, session) {

  # Shared reactive state used across every module
  rv <- reactiveValues(
    data = NULL, selected_data = NULL,
    cleaned_data = NULL, data_no_outliers = NULL,
    models = NULL
  )

  # Upload, column/variable selection, missing values, outliers,
  # standardization, dynamic y/x/covariate/interaction/post-hoc
  # selectors, data table + processed-data modal
  data_processing_server(input, output, session, rv)

  # Core model fitting -> shared `runModels` eventReactive
  runModels <- model_fit_server(input, output, session, rv)

  # Model/ANOVA text output, per-model summary tables, HTML report
  model_summary_server(input, output, session, rv, runModels)

  # DHARMa diagnostics, performance metrics, Cook's/Mahalanobis plots
  diagnostics_server(input, output, session, rv, runModels)

  # Distribution fitting for the dependent variable
  distributions_server(input, output, session, rv)

  # Post-hoc EMMs / pairwise comparisons
  emmeans_server(input, output, session, rv, runModels)

  # Boxplots, t-tests, correlation plots
  plotting_server(input, output, session, rv)
}
