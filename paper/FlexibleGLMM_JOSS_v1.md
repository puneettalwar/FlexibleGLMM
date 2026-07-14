
---
title: 'FlexibleGLMM: An R Shiny Application for Flexible Generalized Linear Mixed
  Model Analysis'
tags:
- R
- Shiny
- statistics
- GLMM
- visualization
- dashboard
date: "24 June 2026"
output:
  html_document:
    df_print: paged
  word_document: default
  pdf_document: default
authors:
- name: Puneet Talwar
  orcid: 0000-0001-7631-7926
  corresponding: yes
  affiliation: 1
- name: Fermin Balda Aizpurua
  orcid: 0000-0002-0943-8928
  affiliation: 1
- name: Christophe Phillips
  orcid: 0000-0002-4990-425X
  affiliation: 1
- name: Gilles Vandewalle
  orcid: 0000-0003-2483-2752
  affiliation: 1
bibliography: paper.bib
affiliations:
- name: "GIGA-Institute, CRC-Human Imaging, University of Liège, Liège, Belgium"
  index: 1
---

## Summary

Generalized Linear Mixed Models (GLMMs) are widely used in biomedical, psychological, ecological, and social science research to analyze clustered, longitudinal, and repeated-measures data [@wiley2019advanced; @meteyard2020best; @bolker2009generalized]. Although numerous software implementations exist, researchers often face challenges selecting appropriate modeling frameworks, specifying model structures, handling missing data, comparing estimation methods, and reproducing analyses across statistical software platforms.

**FlexibleGLMM** is an open-source R Shiny application that provides a graphical user interface (GUI) for fitting, evaluating, and reporting generalized linear mixed models without requiring programming expertise. The application integrates multiple established R packages for mixed-effects modeling and provides functionality for model diagnostics, effect estimation, visualization, and automated reporting. In addition, **FlexibleGLMM** facilitates replication of analyses performed in other commonly available statistical tools such as SAS (PROC MIXED and PROC GLIMMIX) and SPSS, allowing researchers to compare results across software ecosystems [@stroup2018sas; @west2011overview; @bolker2020glmm; @hopkins2016spss].

The application is intended for researchers, clinicians, students, and statisticians seeking a unified and reproducible framework for GLMM analysis through an interactive web-based environment.

---

## Statement of Need

Generalized linear mixed models have become a standard analytical framework for handling correlated observations arising from repeated measurements, longitudinal studies, multicenter trials, and hierarchical datasets. While powerful, GLMM implementation often requires familiarity with statistical programming and knowledge of multiple software packages.

Several R packages provide complementary capabilities for mixed-effects modeling. For example, lme4 [@lme4] provides efficient estimation of linear and generalized linear mixed models, afex [@afex] extends mixed-model analyses through ANOVA-based inference and Type III tests similar to those commonly used in SAS, while nlme [@nlme] offers flexible covariance structures for longitudinal data. Packages such as emmeans [@emmeans], performance [@performance], DHARMa [@DHARMa], and effectsize [@effectsize] further support post-hoc inference, model diagnostics, and effect size estimation. Researchers frequently need to combine outputs from multiple packages to complete a single analysis workflow.

Furthermore, many clinical and biomedical researchers continue to rely on different and at times multiple statistical tools. Translating analyses between them and R often requires substantial programming effort and familiarity with differences in covariance structures, denominator degrees of freedom approximations, hypothesis testing procedures, and least-squares means estimation.

**FlexibleGLMM** was developed to address these challenges by:

- Providing a unified graphical interface for GLMM analyses.
- Integrating multiple R packages into a single workflow.
- Supporting repeated-measures and hierarchical study designs.
- Facilitating replication of analyses commonly performed in SAS (PROC MIXED and PROC GLIMMIX) and SPSS.
- Providing Type III tests, estimated marginal means, effect sizes, and diagnostic procedures through a user-friendly interface.
- Reducing programming barriers for researchers with limited coding experience.
- Promoting reproducible statistical analysis through automated reporting and export functions.

---

## Methods

**FlexibleGLMM** is implemented in R and follows a modular architecture that separates data management, model fitting, diagnostics, visualization, and reporting components.

The application integrates functionality from multiple statistical and visualization packages including:

- **Shiny Framework and User Interface:** shiny [@shiny], shinyjs [@shinyjs], DT [@DT]
- **Data Import and Preparation:** readr [@readr], readxl [@readxl], dplyr [@dplyr]
- **Mixed-Effects Modeling and Statistical Analysis:** fitdistrplus [@fitdistrplus],afex [@afex], lme4 [@lme4], nlme [@nlme],emmeans [@emmeans], predictmeans [@predictmeans], parameters [@parameters], effectsize [@effectsize], r2glmm [@r2glmm], and stats [@R]
- **Model Diagnostics and Assumption Testing:** DHARMa [@DHARMa], performance [@performance],lookout [@lookout]
- **Reporting and Result Summarization:** broom.mixed [@broommixed], knitr [@knitr2025; @knitr2014],kableExtra [@kableExtra], and rmarkdown [@rmarkdown2025]
- **Visualization:** ggplot2 [@ggplot2]

**FlexibleGLMM** provides an integrated workflow for data preparation, mixed-model specification, Type III hypothesis testing, covariance structure selection, estimated marginal means computation, effect size estimation, model diagnostics, influence analysis, and automated reporting. The application leverages afex, lme4, and nlme as the primary modeling engines while utilizing complementary packages for post-hoc inference, diagnostic evaluation, and reproducible reporting.

---

## Deployment

**FlexibleGLMM** is deployed through the cloud-hosted **shinyapps.io** platform, allowing users to perform generalized linear mixed-model analyses directly from a web browser without local installation of R or statistical software. The web deployment ensures accessibility across operating systems while simplifying software updates and maintenance. The application is publicly available at:

[FlexibleGLMM Web Application](https://puneet-talwar.shinyapps.io/FlexibleGLMM/)

The deployment leverages the Shiny framework and the shinyapps.io hosting infrastructure for interactive statistical computing and visualization.

---

## Application Features

The **FlexibleGLMM** interface is organized into a left sidebar for data preprocessing, model specification, and analysis configuration, and a main workspace displaying model outputs, diagnostics, visualizations, and reports. The sidebar provides a sequential workflow that guides users from data import to final model estimation (**Figure 1**, **Table 1**).

![](figure1.png)
Figure 1: Screenshot of FlexibleGLMM


### Data Import and Variable Selection

Users can upload input datasets in CSV or Microsoft Excel (.xlsx) formats. The application automatically detects variable names and allows users to select variables to include or exclude from subsequent analyses. Variable types (e.g., continuous or categorical variables) can be manually specified to ensure correct model formulation.

### Missing Data Handling

**FlexibleGLMM** provides options to inspect and remove observations containing missing values. This preprocessing step allows users to create a complete dataset suitable for mixed-model analyses and download the cleaned dataset for external use.

### Outlier Detection and Data Cleaning

The application supports multiple methods for identifying potential outliers, including:

- **Standard deviation (Z-score) thresholding**, where observations exceeding a user-defined number of standard deviations from the mean can be identified.
- **Cook's distance criterion**, with a default cutoff of (4/n), for detecting influential observations.
- **Mahalanobis distance**, based on a user-selected chi-square quantile threshold, for identifying multivariate outliers.
- **Lookout**, provides an alternative approach for detecting unusual observations using leave-one-out kernel density estimates and extreme value theory.
- **DHARMa**, detects outliers using simulated model residuals; therefore, a fitted model is required before diagnostic plots can be generated.

Identified observations can be removed interactively, and the processed dataset can be downloaded as a CSV file.

**Figure 1:** Screenshot of **FlexibleGLMM**

### Numerical Data Preprocessing

Continuous variables can be transformed through different standardization approaches, including:

- Centering and scaling (Z-score standardization)
- Centering only
- Scaling only
- No transformation

The processed data can be reviewed within the application before model fitting (in the Data tab).

### Distribution Assessment

For continuous dependent variables, **FlexibleGLMM** provides a distribution fitting module that evaluates candidate probability distributions. This feature assists users in selecting an appropriate GLMM family by comparing goodness-of-fit across multiple distributions.

### Interaction Specification

The application supports automatic construction of interaction terms, including:

- No interaction
- Two-way interactions between independent variables
- Three-way interactions involving an additional moderator variable

This allows researchers to investigate complex relationships between predictors.

### GLMM Family and Link Function Selection

Users can specify the response distribution and associated link function for generalized linear mixed models. Supported families include common distributions available through R modeling frameworks, allowing analyses of continuous and non-Gaussian outcome variables.

### Modeling Engine Selection

**FlexibleGLMM** integrates multiple mixed-model estimation engines, including:

- **afex::mixed** for SAS-like Type III analysis of variance tables and mixed-model inference,
- **lme4** for linear and generalized linear mixed-effects model estimation,
- **nlme** for linear mixed models with flexible residual covariance structures.

The availability of multiple engines enables users to compare modeling approaches and reproduce analyses commonly performed using SAS PROC MIXED and PROC GLIMMIX.

### Covariance and Correlation Structures

For models estimated using **nlme**, users can specify within-subject correlation structures, such as compound symmetry and autoregressive structures. Additional inputs allow specification of:

- Random grouping variables
- Time or ordering variables required for autoregressive correlations

This functionality facilitates the analysis of longitudinal and repeated-measures datasets.

### Random Effects Specification

**FlexibleGLMM** allows flexible specification of random effects using both lme4/afex syntax and nlme syntax. Users can define random intercepts, random slopes, nested effects, crossed effects, and correlated or uncorrelated random-effect structures. A reference table within the interface provides examples of common random-effect formulations.

### Custom Model Formula

Advanced users can override the automatically generated model specification by entering a custom model equation. This enables implementation of complex models that may not be achievable through the graphical interface alone.

### Model Execution

After all preprocessing and model options have been selected, users can run one or multiple GLMM analyses simultaneously. The application supports multiple dependent and  independent variables within a single analysis workflow, thereby reducing the need for repetitive model specification.

---

## Application Tabs and Outputs

**Table 1: Description of application tabs and primary outputs**

| Tab | Purpose | Primary Outputs |
|-----|---------|-----------------|
| **Instructions** | Provides an overview of the FlexibleGLMM workflow, supported modeling approaches, usage guidelines, and examples of random-effect structures for lme4, afex, and nlme syntax. | Application instructions, modeling guidance, and random-effects reference table. |
| **Data** | Displays the uploaded dataset. | Interactive data table, variable information. |
| **FitDist Output** | Performs distribution fitting for continuous dependent variables to assist in selecting an appropriate response distribution for GLMM analysis. | Distribution comparison statistics and goodness-of-fit measures. |
| **Fit Distribution Plots** | Provides graphical assessment of candidate probability distributions fitted to the response variable. | Cullen-Frey graph, histograms and density plots, Q–Q plots, and distribution comparison figures along with goodness-of-fit-statistics. |
| **Model Output** | Presents the fitted mixed-model results from the selected modeling engine (afex, lme4, or nlme). Supports analysis of multiple dependent and independent variables. | Model formula, parameter estimates, fixed-effect coefficients, random-effect estimates, standard errors, t-statistics, degrees of freedom, p-values along with effect size estimates (partial eta squared). |
| **ANOVA** | Provides analysis of variance tables for evaluating the significance of fixed effects and interactions. In afex, this includes SAS-like Type III tests with appropriate denominator degrees of freedom approximations. | Type I/II/III ANOVA tables, F-statistics, degrees of freedom, p-values, and significance tests. |
| **Summary Table** | Generates summaries of fitted models and statistical estimates across one or multiple analyses. | Formatted model summary tables with F statistics, degree of freedom, and p-values. |
| **Performance** | Evaluates model quality, explanatory power, and assumptions using integrated model-performance metrics. | R² measures, information criteria, model fit indices, residual summaries, and performance statistics. |
| **Post-hoc (EMMs)** | Computes estimated marginal means (least-square means) and pairwise comparisons for significant main effects and interactions. | Estimated marginal means, contrasts, pairwise comparisons, confidence intervals, and p-values. |
| **Summary Plots** | Provides exploratory data analysis outputs with plots and basic statistical tests. | Box plots, pairwise t-test comparisons, Spearman correlation plots and statistics. |
| **Outlier Diagnostics** | Performs outlier diagnostic procedures to identify influential observations, and potential outliers. | Cook's distance plot, Mahalanobis distance plot and DHARMa residual plot and diagnostics. |
| **Auto Report** | Automatically generates a reproducible analysis report summarizing model specification and statistical results. | Downloadable reports containing analysis summaries. |

---

## Availability and Community Guidelines

The **FlexibleGLMM** source code is openly available under an open-source license (GPL-3) through GitHub:

[FlexibleGLMM GitHub Repository](https://github.com/puneettalwar/FlexibleGLMM)

Users are encouraged to report bugs, request features, and contribute improvements through the GitHub repository or by contacting the developer directly. Documentation, example datasets, and updates are maintained through the project repository to facilitate reproducible analyses and community-driven development.

---

## Funding support

FB was supported by the European Union’s Horizon 2020 research and innovation program under the Marie Skłodowska-Curie grant agreement No 860613. CP and GV are supported by the FRS-FNRS. 
PT is/was supported by the EU Joint Programme Neurodegenerative Disease Research (JPND) IRONSLEEP and HISTOPARK projects, respectively – FNRS references: R.8006.20,R.8001.25). 

---

## Acknowledgments

**FlexibleGLMM** acknowledges the open-source R community for creating and maintaining the packages that
support the application’s statistical and graphical functionality.

---

## AI usage disclosure

ChatGPT and Claude were used for troubleshooting and formatting of the code. 

---

## References


