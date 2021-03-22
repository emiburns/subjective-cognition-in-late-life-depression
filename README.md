Differential Predictors of Self and Study Partner-Assessed Subjective
Cognition in Late Life Depression
================

### Aims

This project explores the how determinants of subjectively rated
cognition may change according to rater in a late life depression (LLD)
sample, as well as how clinical status may negatively impact accurate
interpretation of cognition independent of objective cognition.

Analyses ran for **“Differential Predictors of Self and
Informant-Assessed Subjective Cognition in Late Life Depression”
abstract submission** poster (accepted by the American Psychiatric Association
(APA) 2021 conference, lead author Emily Burns).

### Data

Data is from the Alzheimer’s Disease Neuroimaging Initiative’s
Depression Project (ADNI-D, PI Dr. Scott Mackin). Dataset is not
currently open source. Only relevant scripts are provided in current
repository. For ADNI-D data sharing policy or more insight regarding the
project, visit the [Laboratory of Neuroimaging
(LONI)](https://ida.loni.usc.edu/login.jsp) Image & Data Archive (IDA)
portal or the [ADNI-D Project
website](https://latelifedepression.ucsf.edu/alzheimer%E2%80%99s-disease-neuroimaging-initiative%E2%80%99s-depression-project-adni-d).

### Files

  - **data\_dictionary.md:** Comprehensive description of included
    dataset variables

  - **data\_cleaning.R:** Covers data cleaning and feature engineering.
    Clean csv written to directory for use in following analyses (not
    included in repo)

  - **eda.md:** Markdown file of exploratory data analysis results

  - **eda\_functions.R:** Functions used for exploratory data analysis

  - **participant\_linear\_regressions.R:** includes univariate analyses
    and feature selection, followed by linear regression models for
    patient ECog version

  - **informant\_linear\_regressions.R:** includes univariate analyses
    and feature selection, followed by linear regression models for
    study partner ECog version

  - **linear\_regression\_functions.R:** Functions used for linear
    regression analyses

  - **compare\_ecog\_versions.R:** Chi-squared and ANOVA tests directly
    comparing objective cognition and both ECog participant and study
    partner versions
    
  - **demos_and_plots.R:** includes sample demographics, clinical characteristics, and plots utilized for poster presentation
