#**************************************************************
# flexibleglmm-package.R
# Package-level documentation and NAMESPACE imports.
#
#
# After editing this file (or any roxygen tags elsewhere), run:
#   devtools::document()
# to regenerate NAMESPACE and man/.
#**************************************************************

#' flexibleglmm: Flexible GLMM Toolbox
#'
#' A Shiny app to fit GLMM models using afex, lme4::glmer, and
#' nlme::lme, with SAS/SPSS-like output, distribution fitting,
#' diagnostics, and post-hoc (EMMs) comparisons.
#'
#' @keywords internal
"_PACKAGE"

## usethis namespace: start
#' @import shiny
#' @import ggplot2
#' @import fitdistrplus
#' @importFrom shinyjs useShinyjs
#' @importFrom DT DTOutput renderDT datatable
#' @importFrom lme4 glmer glmerControl isSingular
#' @importFrom nlme corAR1 corCompSymm corSymm corExp
#' @importFrom DHARMa simulateResiduals testUniformity testDispersion testOutliers testZeroInflation
#' @importFrom emmeans emmeans contrast
#' @importFrom afex mixed
#' @importFrom readr read_csv
#' @importFrom readxl read_excel
#' @importFrom tools file_ext
#' @importFrom performance r2 model_performance
#' @importFrom predictmeans residplot
#' @importFrom lookout lookout
#' @importFrom effectsize eta_squared
#' @importFrom parameters standardize_parameters
#' @importFrom r2glmm r2beta
#' @importFrom broom.mixed tidy
#' @importFrom rmarkdown render
#' @importFrom knitr kable
#' @importFrom kableExtra kable_styling
#' @importFrom dplyr mutate across left_join where
#' @importFrom stats as.formula lm cooks.distance mahalanobis qchisq cor.test pairwise.t.test na.omit na.exclude sd cov quantile ave gaussian Gamma binomial poisson anova AIC BIC logLik nobs complete.cases family model.frame
#' @importFrom graphics abline plot.new points text
#' @importFrom utils capture.output combn write.csv
## usethis namespace: end
NULL
