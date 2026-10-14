# Getting Started: FlexibleGLMM Usage Guide

## Overview

This guide demonstrates a complete Generalized Linear Mixed Model (GLMM) workflow in two parts –

1. General usage of app features
2. Reproducible examples using 6 open access datasets for direct-R comparison and comparable SAS outputs

## PART-I: General usage of app features

### 1\. Upload and inspect the dataset

1. Click Browse under the Upload Data Frame (CSV or Excel) section.
2. Select the sleep study dataset (sleepstudy.csv or .xlsx).
3. Once uploaded, navigate to the Data tab and scroll through the dataset.

The classical lme4::sleepstudy data contain reaction time measured repeatedly for 18 subjects over 10 days (180 observations).

The dataset should appear in long format, where:

- Each row represents a repeated observation.
- Subject identifies individual participants.
- Days represent the repeated measurement/time variable.
- Reaction_time is the dependent outcome variable.
- Long-format data are required for most mixed-effects modeling approaches.

### 2\. Select Variables for Analysis

1. Under Select columns to include or exclude from analysis, select the variables needed for analysis.
2. Click Apply Column Selection.

The working dataset now contains only the variables used for model fitting and preprocessing.

### 3\. Specify Variable Types

1. Select categorical or factor variables and continuous or numerical variables.

2. Click Apply Variable Types.

	- Subject → categorical/grouping variable
	- Days → numeric time variable
	- Reaction_time → numeric response

**Note:** FlexibleGLMM relies on R's default treatment contrasts (contr.treatment) throughout for categorical predictors

### 4\. Missing values

1. Click **Remove Missing Values**.
2. FlexibleGLMM currently performs complete-case preprocessing using na.omit(). This removes rows containing missing values; it does not automatically delete an entire subject. Consequently, a balanced repeated-measures dataset may become unbalanced.
3. Review & record:

	- dimensions before preprocessing,
	- missing values per variable,
	- dimensions after preprocessing,
	- number of rows removed.

**Important:**

Complete-case preprocessing is a software choice in the current release, not a general recommendation for longitudinal missing-data analysis.

### 5\. Outlier screening (Exploratory)

FlexibleGLMM provides five complementary approaches.

#### 5.1 Z-score method

1. Select standard deviation- based outlier detection.
2. Use 3 as a conventional exploratory threshold

This identifies unusually extreme values on the selected numeric variables. It does not establish that an observation is erroneous.

#### 5.2 Cook's Distance

1. Select Cook's Distance.
2. Leave the cutoff field blank to use the default threshold: 4/n (a heuristic, not a universal statistical criterion).
3. Alternatively, a cutoff value of 1 may be used as a less stringent criterion.

**Important:** This pre-model option fits a simple linear model to numeric variables. It is therefore an exploratory influence screen and is **not** equivalent to mixed-model Cook's distance.

For model-based influence assessment, use the fitted model in the Diagnostics stage.

#### 5.3 Mahalanobis Distance (MD)

1. Select Mahalanobis Distance.
2. Use the default chi-square quantile threshold (e.g., 0.99).

A threshold of 0.95 is also commonly used depending on the desired sensitivity.

The current implementation uses the numeric variables in the working dataset and a pooled sample covariance matrix. A pooled Mahalanobis distance does not account for the longitudinal clustering of repeated observations within subjects.

Mahalanobis distance is theoretically invariant to nonsingular linear rescaling, but the variables included, missing-data handling, covariance estimation, and numerical conditioning remain important.

#### 5.4 Lookout-Based Outlier Detection

1. Select the Lookout method.
2. Review the anomalous data points carefully.

#### 5.5 DHARMa

**Do not treat DHARMa as a pre-modeling outlier test.**

DHARMa requires a fitted model because its residuals are simulated from the fitted model. Therefore, use DHARMa primarily after the initial model has been fitted.

If you want to demonstrate the DHARMa workflow, first fit a simple mixed model and then inspect the **Diagnostics** tab — see Step 11.

### 6\. Decide whether observations should be removed

Do not use agreement among the five methods as a formal "consensus."

The methods answer different questions:

- Z-score → extreme univariate value
- Mahalanobis → multivariate unusualness
- Cook's distance → influence on model estimates
- Lookout → anomaly ranking
- DHARMa → model-based residual behavior

For the tutorial, retain all observations unless there is a documented scientific or data-quality reason for removal.

If an observation is removed, document the criterion and download the resulting dataset.

### 7\. Perform Data Standardization

1. Select the numerical variables to be standardized.
2. Select numerical processing steps to perform.
3. Choose either None or Center only (if afex is used) for this tutorial.
4. Click View Processed Data and check for additional columns with suffix: `_cs` (center & scale), `_c` (center only), `_s` (scale only).

**Note:** Numerical standardization (centering or scaling) can influence certain distance-based outlier detection methods. Therefore, outlier assessment should be performed on the original scale of the variables whenever possible.

### 8\. Specify an initial Gaussian LMM

Select:

- Response: Reaction_time
- Predictor: Days
- Random effect: (1 | Subject)
- Family: Gaussian
- Link: identity/default
- Engine: afex::mixed
- Interaction: none

A random-intercept model allows subjects to have different baseline reaction times while estimating a common effect of Days.

Conceptually:

**Reaction_time = fixed effects + subject-specific baseline deviation + residual error**

The residuals are conditionally independent under the basic Gaussian random-intercept model unless an additional residual correlation structure is specified.

The equivalent direct-R call for the model configured above (Gaussian family, afex engine) is:

````
library(afex)  
model <- afex::mixed(  
Reaction_time ~ Days + (1 | Subject),  
data = dat,  
method = "KR" # Kenward-Roger denominator df  
)  
anova(model, ddf = "Kenward-Roger", type = 3)
````

**Note:** Users can input several dependent variables to fit a separate univariate model per requested combination. This is not equivalent to a joint multivariate mixed model.

### 9\. Compare a random-slope model

Now fit:

`Reaction_time ~ Days + (1 + Days | Subject)`

This model allows subjects to differ both in baseline reaction time and in their temporal change across Days.

Compare the random-intercept and random-slope models using: convergence, singularity, variance components, AIC/BIC when the models are fitted under comparable likelihood conventions, residual diagnostics and scientific plausibility.

Do not select a random-effects structure solely because it produces a smaller numerical criterion.

**Note:** In FlexibleGLMM the models **-** afex::mixed (Gaussian) and nlme::lme default to REML. However, models with different fixed-effects structures should not generally be compared via REML likelihood. For comparison model can be fit using ML through custom equation option.

### 10\. Review Model Outputs

Inspect the following tabs:

- Model Output — Fixed and random effect estimates; standard errors, random-effect estimates, and model information; also reports the number of subjects and observations used.
- ANOVA — Type III tests; Gaussian afex models use the Kenward-Roger denominator degrees-of-freedom approximation.
- Summary Table — Review the formatted model summaries.
- Performance — Model fit statistics (AIC, BIC, marginal/conditional R²) and explanatory measures.
- Diagnostics — Residual behavior, influential observations, assumption checks, including lme4 singularity and convergence flags and DHARMa simulated residuals.

### 11\. Residual Diagnostics with DHARMa

DHARMa evaluates simulated residuals from a fitted model, so it can only be run after model fitting, not as a preprocessing/outlier-screening step (Step 5).

1. Open the Diagnostics tab for the fitted model.
2. Review the DHARMa residual plot (uniformity of scaled residuals, dispersion) and the accompanying test outputs.
3. Treat flagged points as candidates for review, not automatic removal — see Step 6.

### 12\. Assess the Response Distribution (Exploratory)

1\. Select Reaction_time and click **Fit Distribution**.

The application currently compares:

- Normal
- Gamma
- Lognormal
- Weibull

The FitDist Output and Fit Distribution Plots tabs provide goodness-of-fit information and visual comparisons.

**Important caveat**

This module describes the marginal (pooled) distribution of the observed response values. A GLMM's family assumption instead concerns the conditional distribution of the response given the random effects, which is generally not the same as the pooled marginal distribution. Treat this step as an exploratory aid for narrowing candidate families, not as a formal or sufficient basis for selecting the GLMM family.

### **Important interpretation**

If Gamma appears to fit the pooled marginal response better than Normal, this does **not** prove that Gamma is the correct conditional distribution for a GLMM.

Use the result as exploratory information only.

### 13\. Gamma mixed model

Select:

- Family: Gamma
- Engine: lme4::glmer
- Link: inverse or identity

#### Positivity constraint

A Gamma response requires a strictly positive conditional mean. Under an identity link, the linear predictor is not automatically constrained to be positive. FlexibleGLMM checks the fitted values after estimation and displays a warning if any fitted mean is non-positive. If this warning appears, reconsider the link function or model specification rather than proceeding with an invalid fit.

#### Inverse link (Default)

The default link for Gamma models is the inverse link. The inverse link changes the relationship between predictors and the conditional mean. The inverse link may improve numerical convergence in some cases, but this is not a substitute for choosing the link on statistical grounds. The link choice should primarily reflect the assumed mean structure and scientific interpretation, not convergence behavior alone. It should not be selected merely because it converges more easily; scientific/modeling considerations should determine the link.

#### Identity link

With an identity link, the conditional mean is modeled directly on the response scale. This can make coefficients easier to interpret, but the fitted mean must remain positive.

#### Important

Because these two models differ in link function (not just parameterization), fit statistics such as AIC should be compared cautiously and alongside diagnostic checks, not as the sole deciding criterion.

### 14\. Gaussian model with log link

Fit:

- Family: Gaussian
- Link: log
- Engine: lme4::glmer or the supported Gaussian pathway

A Gaussian model with a log link is **not the same as a log-normal response model**.

In a Gaussian-with-log-link model, the response remains Gaussian conditional on the model structure while the mean is linked logarithmically to the linear predictor.

A log-normal model instead models the logarithm of the response as normally distributed

### 15\. Post-hoc Analysis (Estimated marginal means)**

**1\.** Select categorical variables to perform Tukey's post-hoc analysis.

Post-hoc EMMs are based on the fitted model and should be driven by the scientific question, not automatically whether an omnibus p-value is below a threshold.

For pairwise comparisons, FlexibleGLMM uses the emmeans framework. The module reports both the unadjusted pairwise contrasts and a Tukey-adjusted version for each selected factor (and Tukey/Sidak-relevant conditional comparisons).

### 16\. Summary Plots (Exploratory)

The Summary Plots tab currently provides conventional pairwise t-tests and pooled Spearman correlations.

For repeated-measures data, these are exploratory summaries and should not be interpreted as substitutes for mixed-model inference.

A pooled Spearman correlation treats all rows as observations and does not estimate a within-subject association. Similarly, conventional pairwise t-tests do not model the subject-level correlation structure.

### 17\. Auto Report

Download the HTML report after fitting the model.

The report includes:

- number of subjects,
- number of observations,
- model formula,
- family and link,
- engine,
- estimation method,
- optimizer where available,
- random-effects structure,
- residual correlation structure,

- model estimates,
- ANOVA/inference,
- EMMs,
- performance measures,
- diagnostics,
- convergence warnings,
- singularity status.

### 18\. Key interpretation

FlexibleGLMM is a workflow and accessibility tool rather than an automated model-selection system. The user remains responsible for specifying a scientifically defensible model, checking assumptions, evaluating diagnostics, and documenting preprocessing and model selection decisions.

# Part II. Reproducible examples using open-access datasets

## Direct R comparisons and comparable SAS/SPSS-style outputs

### Purpose and scope

This part of the FlexibleGLMM tutorial presents six worked examples
using open-access or package-provided datasets. Together, the examples
illustrate generalized and linear mixed-effects models, repeated
observations, crossed grouping factors, random intercepts and slopes,
and comparisons with reference analyses in SAS, SPSS, or jamovi's GAMLj
module.

The examples use the following model-fitting functions:

-   `lme4::glmer()` for generalized linear mixed-effects models (GLMMs);
-   `afex::mixed()` for mixed-model analyses;
-   `lme4::lmer()` for linear mixed-effects models (LMMs); and
-   `nlme::lme()` for linear mixed-effects models, including models with
    alternative residual variance or correlation structures.

**Reproducibility note.** The code below records the model
specifications described in the source tutorial. Before running an
example, ensure that the relevant package is installed, the dataset has
been loaded or imported, and variable names match the supplied data.


## At a glance

  --------------------------------------------------------------------------------------
  Example        Dataset        Main structure      Engine /          Main learning
                                                    comparison        objective
  -------------- -------------- ------------------- ----------------- ------------------
  1              CAMIS Scenario Binary outcome;     `lme4::glmer()`   Binary GLMM with
                 1              site-level random   and SAS           random intercept
                                intercept           `PROC GLIMMIX`    

  2              CAMIS Scenario Binary outcome;     `lme4::glmer()`   Binary GLMM with
                 2              participant-level   and SAS           an uncorrelated
                                random intercept    `PROC GLIMMIX`    random intercept
                                and slope                             and slope

  3              `lexdec`       Crossed subject and `afex::mixed()`   Interactions and
                                word grouping                         crossed random
                                factors                               intercepts

  4              `sleepstudy`   Repeated            `lme4::lmer()` /  Compare
                                reaction-time       FlexibleGLMM      random-intercept
                                measurements within custom equation;  and random-slope
                                subject             also usable with  models
                                                    `afex`            

  5              `ratpup`       Pup weights grouped `nlme::lme()` and Compare residual
                                by litter           `lme4::lmer()`    variance modelling
                                                                      and estimation
                                                                      settings

  6              `sleepstudy`   Repeated measures   R compared with   Cross-software
                 and Beers  and clustered data  SPSS or jamovi    output comparisons
                                                    GAMLj             
  --------------------------------------------------------------------------------------

> The `sleepstudy` dataset is used in two separate examples: one for
> basic random-effects specifications and another for R/SPSS
> comparisons. The Beers example is numbered separately to avoid the
> duplicate numbering in the source document.

------------------------------------------------------------------------

## Example 1. `lme4::glmer()` --- CAMIS Scenario 1: binary GLMM with a random intercept

### Data source

The example is based on the Comparative Analysis of Mixed Models in SAS
and R (CAMIS) project. The project provides examples designed to compare
mixed-model analyses across statistical software.

-   CAMIS comparison page:
    <https://psiaims.github.io/CAMIS/Comp/r-sas_glmm.html>
-   Data and simulation scripts:
    <https://github.com/Franosei/Camis-data-simulation>

### Data characteristics

The analysis uses a binary response, `AVAL`, and fixed-effect predictors
`ARMCD`, `AGE`, `SEX`, `RACE`, `COMORBIDX`, and `PRIORADM`. The random
intercept `(1 | SITEID)` accounts for clustering by site. The model uses
a binomial distribution with a logit link.

### R model

``` r
fit_r <- lme4::glmer(
  AVAL ~ ARMCD + AGE + SEX + RACE + COMORBIDX + PRIORADM +
    (1 | SITEID),
  data = scenario_01,
  family = binomial(link = "logit"),
  control = lme4::glmerControl(optimizer = "bobyqa")
)

summary(fit_r)
```

### SAS comparison

``` sas
proc glimmix data=scenario_01 method=laplace;
  where PARAMCD = "READM30D"
    and ITTFL = "Y"
    and SAFFL = "Y"
    and ANL01FL = "Y";
  class SITEID ARMCD(ref="STDCARE") SEX RACE;
  model AVAL(event="1") =
    ARMCD AGE SEX RACE COMORBIDX PRIORADM
    / dist=binary link=logit solution oddsratio;
  random intercept / subject=SITEID;
run;
```

### Reference outputs

-   FlexibleGLMM output link: <https://github.com/puneettalwar/FlexibleGLMM/blob/main/inst/extdata/Output_FlexibleGLMM/CAMIS_Scenario1_output1.html>
-   Reference PDF: <https://github.com/puneettalwar/FlexibleGLMM/blob/main/inst/extdata/Output_reference_source/CAMIS_glmer_R_vs_SAS.pdf>
-   Reference output location: page 9 of the reference PDF.


**Interpretation focus:** compare the fixed-effect estimates, odds
ratios, uncertainty estimates, and site-level random-intercept results
across R and SAS. 

## Example 2. `lme4::glmer()` --- CAMIS Scenario 2: binary GLMM with a random intercept and slope

### Data source

Use the CAMIS project resources listed in Example 1:

-   <https://psiaims.github.io/CAMIS/Comp/r-sas_glmm.html>
-   <https://github.com/Franosei/Camis-data-simulation>

### Data characteristics

This example models binary `AVAL` using `ARMCD`, `TIMEYEAR_CS`, `AGE_CS`,
`SEX`, `RACE`, `BSEVRITY`, and `ADHERE_CS`. The grouping factor is
`USUBJID`. The term `(1 + TIMEYEAR_CS || USUBJID)` specifies
participant-specific random intercepts and slopes while omitting their
estimated correlation in `lme4`.

### R model

``` r
fit_r <- lme4::glmer(
  AVAL ~ ARMCD + TIMEYEAR_CS + AGE_CS + SEX + RACE +
    BSEVRITY + ADHERE_CS + (1 + TIMEYEAR_CS || USUBJID),
  data = scenario_02,
  family = binomial(link = "logit"),
  control = lme4::glmerControl(
    optimizer = "bobyqa",
    optCtrl = list(maxfun = 100000)
  )
)

summary(fit_r)
```

### SAS comparison

``` sas
proc glimmix data=scenario_02_scaled method=laplace;
  class USUBJID ARMCD(ref="CTRLTHX") SEX RACE;
  model AVAL(event="1") =
    ARMCD TIMEYEAR_C AGE_C SEX RACE BSEVRITY ADHERE_C
    / dist=binary link=logit solution oddsratio;
  random intercept TIMEYEAR_C / subject=USUBJID type=vc;
run;
```

### Reference outputs

-   FlexibleGLMM output link: <https://github.com/puneettalwar/FlexibleGLMM/blob/main/inst/extdata/Output_FlexibleGLMM/CAMIS_Scenario2_output.html>
-   Reference PDF:<https://github.com/puneettalwar/FlexibleGLMM/blob/main/inst/extdata/Output_reference_source/CAMIS_glmer_R_vs_SAS.pdf>
-   Reference output location: page 11 of the reference PDF.

**Important comparison check:** Note that the model uses centering 
and scaling for 3 numerical variables. Verify that the datasets apply 
equivalent scaling before comparing estimates. 

Additional R/SAS comparison resources:

-   <https://stat.ethz.ch/pipermail/r-help/2007-June/133383.html>
-   <https://mirrors.sustech.edu.cn/CRAN/web/packages/SASmixed/vignettes/Usinglmer.pdf>

## Example 3. `afex::mixed()` --- Lexical decision data (`languageR::lexdec`)

### Data source

The `lexdec` dataset is provided by the R package `languageR`. Export it
for use in FlexibleGLMM as follows:

``` r
library(languageR)
write.csv(lexdec, file = "lexdec_data.csv", row.names = FALSE)
```

### Data characteristics

The dataset comes from a lexical decision task and contains repeated
observations associated with subjects and words. Subjects and words are
crossed grouping factors, so the model can account for variability
attributable to both participants and lexical items.

The model includes the interaction `NativeLanguage * Length`, as well as
the interaction `PrevType * meanWeight`, and random intercepts for
`Subject` and `Word`. In R formula syntax, `A * B` expands to
`A + B + A:B`, so the corresponding lower-order terms are included
automatically.

### Model

``` r
m1 <- afex::mixed(
  RT ~ Correct + Trial + PrevType * meanWeight + Frequency +
    NativeLanguage * Length + (1 | Subject) + (1 | Word),
  data = lexdec
)

summary(m1)
```

### Practical note

The model fitting may take several minutes.Allow sufficient time for 
estimation, particularly when using the full model and the selected 
estimation settings. The analysis can also be reproduced using 
FlexibleGLMM's custom-equation functionality.

### Reference outputs

-   FlexibleGLMM output link: <https://github.com/puneettalwar/FlexibleGLMM/blob/main/inst/extdata/Output_FlexibleGLMM/lexdec_afex_mixed_output.html>
-   Reference PDF:<https://github.com/puneettalwar/FlexibleGLMM/blob/main/inst/extdata/Output_reference_source/lexdec_afex_page59.pdf>
-   Reference output location: page 59 of the reference PDF.

**Interpretation focus:** review fixed effects and interaction terms
alongside the subject- and word-level random effects. Comparisons are
meaningful only when the same data, model terms, estimation method, and
inferential settings are used.

## Example 4. `lme4::lmer()` --- Sleepstudy data: random-intercept and random-slope models

### Data source

The `sleepstudy` dataset is included in the R package `lme4`. Export it
for use in FlexibleGLMM with:

``` r
library(lme4)
write.csv(sleepstudy, file = "sleepstudy.csv", row.names = FALSE)
```

A related tutorial is available at
<https://cdsbasel.github.io/dataanalytics_rsessions/_sessions/CausalInference/intro_lme4.html>.

### Data characteristics

The dataset contains repeated reaction-time measurements for subjects
across days. `Reaction` is the continuous outcome, `Days` is the
within-subject predictor, and `Subject` identifies the repeated-measures
grouping factor.

### Model 3: random intercept

``` r
model3 <- lme4::lmer(
  Reaction ~ 1 + Days + (1 | Subject),
  data = sleepstudy
)
summary(model3)
```

This model allows subjects to have different baseline reaction times but
assumes a common Days slope across subjects.

### Model 4: random intercept and slope

``` r
model4 <- lme4::lmer(
  Reaction ~ 1 + Days + (1 + Days | Subject),
  data = sleepstudy
)
summary(model4)
```

This model allows both baseline reaction time and the effect of Days to
vary by subject, and estimates the intercept--slope correlation.

The dataset can also be analysed using `afex::mixed()` or reproduced
through FlexibleGLMM's custom-equation functionality.

### Reference outputs

-   FlexibleGLMM output links (Model 3): <https://github.com/puneettalwar/FlexibleGLMM/blob/main/inst/extdata/Output_FlexibleGLMM/sleepstudy_afex_lmer_model3_output.html>
-   FlexibleGLMM output links (Model 4): <https://github.com/puneettalwar/FlexibleGLMM/blob/main/inst/extdata/Output_FlexibleGLMM/sleepstudy_afex_lmer_model4_output.html>
-   Reference PDF: <https://github.com/puneettalwar/FlexibleGLMM/blob/main/inst/extdata/Output_reference_source/sleepstudy_afex_lmer.pdf>
-   Model 3 reference output: page 4 of the reference PDF.
-   Model 4 reference output: page 5 of the reference PDF.


**Interpretation focus:** compare the fixed Days effect, random-effect
variances, model fit, and the implications of allowing the Days slope to
vary by subject.

## Example 5. `nlme::lme()` and `lme4::lmer()` --- Rat pup data (`WWGbook::ratpup`)

### Data source

The `ratpup` dataset is provided by the `WWGbook` R package.

-   Package information:
    <https://www.rdocumentation.org/packages/WWGbook/versions/1.0.2>
-   Dataset documentation:
    <https://www.rdocumentation.org/packages/WWGbook/versions/1.0.2/topics/ratpup>
-   Additional example: <https://www.rpubs.com/HeatWave2019/578896>

Export the data with:

``` r
library(WWGbook)
write.csv(ratpup, file = "ratpup.csv", row.names = FALSE)
```

### Data characteristics

The example models `weight` using `treatment`, `sex`, `litsize`, and the
`treatment * sex` interaction, with a random intercept for `litter`. The
`varIdent()` model additionally allows residual variance to differ by
treatment group.

### Model 1: `nlme::lme()` with a random litter intercept

``` r
Model.01 <- nlme::lme(
  weight ~ treatment + sex + litsize + treatment * sex,
  random = ~ 1 | litter,
  data = ratpup,
  method = "REML"
)
summary(Model.01)
```

### Model 2: treatment-specific residual variance

``` r
Model.02 <- nlme::lme(
  weight ~ treatment + sex + litsize + treatment * sex,
  random = ~ 1 | litter,
  data = ratpup,
  method = "REML",
  weights = nlme::varIdent(form = ~ 1 | treatment)
)
summary(Model.02)
```

### Comparable `lme4::lmer()` fits

### Model 3 

```
model1.fit.lmer <- lme4::lmer(
  weight ~ treatment + sex + litsize + treatment * sex + (1 | litter),
  data = ratpup,
  REML = TRUE
)
summary(model1.fit.lmer)
```

### Reference outputs

-   FlexibleGLMM output links (Model 1): <https://github.com/puneettalwar/FlexibleGLMM/blob/main/inst/extdata/Output_FlexibleGLMM/ratpup_nlme_Model.01_output.html>
-   FlexibleGLMM output links (Model 2): <https://github.com/puneettalwar/FlexibleGLMM/blob/main/inst/extdata/Output_FlexibleGLMM/ratpup_nlme_custom_equation_output.html>
-   FlexibleGLMM output links (Model 3): <https://github.com/puneettalwar/FlexibleGLMM/blob/main/inst/extdata/Output_FlexibleGLMM/ratpup_lmer_custom_eq_output.html>
-   Reference PDF: <https://github.com/puneettalwar/FlexibleGLMM/blob/main/inst/extdata/Output_reference_source/ratpup_nlme_lmer.pdf>
-   Model 1 reference output: page 6 of the reference PDF.
-   Model 2 reference output: page 9 of the reference PDF.
-   Model 3 reference output: page 23 of the reference PDF.

**Interpretation focus:** compare the common fixed- and random-effects
structure first. Note that `nlme::lme()` with `varIdent()` permits
treatment-specific residual variances, whereas the displayed
`lme4::lmer()` models do not reproduce that variance function. REML
settings also differ between the two `lmer()` fits, so compare
estimation settings deliberately.

## Example 6. Cross-software comparisons --- `sleepstudy` (R and SPSS) and Beer Bars (R and jamovi GAMLj)

This section contains two comparison cases. They are kept together
because their primary purpose is to compare outputs across software
rather than introduce a new model family.

### 6A. Sleepstudy: R and SPSS comparison

#### Data source and characteristics

Use the `sleepstudy` dataset described in Example 4. A worked R/SPSS
comparison is available at:

<https://library.virginia.edu/data/articles/comparing-mixed-effect-models-in-r-and-spss>

#### R models

``` r
m1 <- lme4::lmer(Reaction ~ Days + (Days | Subject), data = sleepstudy)

m2 <- afex::mixed(Reaction ~ Days + (Days || Subject), data = sleepstudy)

m3 <- lme(Reaction ~ Days, random = list(Subject = pdIdent(~ Days)), data = sleepstudy)

```

The first model estimates a correlated random intercept and slope. The
second specifies uncorrelated random effects using `||` in `lme4`. 

#### Reference outputs

The source tutorial identifies the following reference pages:

-   FlexibleGLMM output links (Model 1): <https://github.com/puneettalwar/FlexibleGLMM/blob/main/inst/extdata/Output_FlexibleGLMM/sleepstudy_lmer_m1_outout.html>
-   FlexibleGLMM output links (Model 2): <https://github.com/puneettalwar/FlexibleGLMM/blob/main/inst/extdata/Output_FlexibleGLMM/sleepstudy_afex_m2_output.html>
-   FlexibleGLMM output links (Model 3): <https://github.com/puneettalwar/FlexibleGLMM/blob/main/inst/extdata/Output_FlexibleGLMM/sleepstudy_lme_m3_output.html>
-   Reference PDF: <https://github.com/puneettalwar/FlexibleGLMM/blob/main/inst/extdata/Output_reference_source/sleepstudy_lmer_nlme_R_vs_SPSS.pdf>
-   Model 1: page 3 (R) and page 7 (SPSS) of the reference PDF.
-   Model 2: page 8 (R) and page 10 (SPSS) of the reference PDF.
-   Model 3: page 11 (R) and 13 (SPSS) of the reference PDF.


### 6B. Beers dataset: FlexibleGLMM and jamovi GAMLj comparison

#### Data source

The Beers example is documented in the GAMLj module materials for jamovi:

<https://gamlj.github.io/mixed_example1.html>

The source tutorial describes it as a built-in example dataset in the
jamovi Data Library for clustered-data and multilevel modelling.

#### Model specifications

``` r
# Model 1: random intercept for bar
model1 <- lme4::lmer(
  smile ~ 1 + beer + (1 | bar),
  data = beers,
  control = lme4::lmerControl(optimizer = "bobyqa")
)

# Model 2: random intercept and slope for beer by bar
model2 <- lme4::lmer(
  smile ~ 1 + beer + (1 + beer | bar),
  data = beers,
  control = lme4::lmerControl(optimizer = "bobyqa")
)
```

#### Reference outputs

-   FlexibleGLMM output links (Model 1): <https://github.com/puneettalwar/FlexibleGLMM/blob/main/inst/extdata/Output_FlexibleGLMM/beers_lmer_R_vs_gamlj_model1_output.html>
-   FlexibleGLMM output links (Model 2): <https://github.com/puneettalwar/FlexibleGLMM/blob/main/inst/extdata/Output_FlexibleGLMM/beers_lmer_R_vs_gamlj_model2_output.html>
-   Reference PDF: <https://github.com/puneettalwar/FlexibleGLMM/blob/main/inst/extdata/Output_reference_source/beers_lmer_R_vs_gamlj.pdf>
-   Model 1: page 10 (jamovi GAMLj) of the reference PDF.
-   Model 2: page 13 (jamovi GAMLj) of the reference PDF.

**Interpretation focus:** compare the estimated fixed effect of `beer`,
between-bar variation in baseline `smile`, and---under Model
2---between-bar variation in the `beer` slope.

------------------------------------------------------------------------

## References and supporting resources

-   CAMIS R/SAS comparison:
    <https://psiaims.github.io/CAMIS/Comp/r-sas_glmm.html>
-   CAMIS data and simulation scripts:
    <https://github.com/Franosei/Camis-data-simulation>
-   Additional R/SAS comparison:
    <https://stat.ethz.ch/pipermail/r-help/2007-June/133383.html>
-   `SASmixed` vignette:
    <https://mirrors.sustech.edu.cn/CRAN/web/packages/SASmixed/vignettes/Usinglmer.pdf>
-   Sleepstudy modelling example:
    <https://cdsbasel.github.io/dataanalytics_rsessions/_sessions/CausalInference/intro_lme4.html>
-   Rat pup data documentation:
    <https://www.rdocumentation.org/packages/WWGbook/versions/1.0.2/topics/ratpup>
-   Rat pup example analysis:
    <https://www.rpubs.com/HeatWave2019/578896>
-   R/SPSS mixed-model comparison:
    <https://library.virginia.edu/data/articles/comparing-mixed-effect-models-in-r-and-spss>
-   Beer Bars / GAMLj example:
    <https://gamlj.github.io/mixed_example1.html>
-   Ford, C. (2020). "Understanding Robust Standard Errors." UVA Library StatLab.
    <https://library.virginia.edu/data/articles/understanding-robust-standard-errors/>
    (accessed October 14, 2026).

