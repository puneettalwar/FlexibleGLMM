**FlexibleGLMM**

*** Warning! This Shiny App has not been validated enough! There is no warranty for the app! ***

**Link to the Shiny App -**
https://puneet-talwar.shinyapps.io/FlexibleGLMM/

**The current version of Flexible GLMM Shiny App can be used to**

- fit GLMM models using afex and glmer R packages
- get results similar to SAS outputs
- identify fit distribution family for the dependent variables
- run analysis for a multiple dependent and independent variables simultaneously

**Usage:**

- Data input format: xlsx or csv file with header row containing variable names.
- By default first sheet will be used as the input. Ex. mtcars, sleepstudy (lme4)
- Missing values are blank/empty cells in the data
- For outlier removal specify the standard deviation value (ex. 3)
- Multiple covariates can be selected from the input data (age sex bmi)
- For family of distributions Refer- https://www.rdocumentation.org/packages/stats/versions/3.6.2/topics/family
- Custom equation format : y ~ x1 + x2

Note:

- Afex mixed - Refer to https://cran.r-project.org/web/packages/afex/afex.pdf
- For using complex models use custom equation option.
- For feedback/queries, please send an email to ptalwar@uliege.be; talwar.puneet@gmail.com.
