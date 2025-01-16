# Analysis of the Australian Longitudinal Study of Women's Health (ALSWH)

This repository contains R code used in articles using the ALSWH.

## Physical activity across midlife and health-related quality of life in Australian women: A target trial emulation using a longitudinal cohort
Code for all analysis in the article examining the causal effects of meeting physical activity on health-related quality of life (Nguyen-duy et al 2024) published in Plos Medicine: https://doi.org/10.1371/journal.pmed.1004384.

| Description | R Code |
| --- | --- |
| 1 - Data extraction - pull relevant variables from each wave | [Data extraction](Code/2024/1_Data_Extraction.R) |
| 2 - Merge data - merge waves and create derived variables | [Merge data](Code/2024/2_Data_Merge.R) |
| 3 - Multiple imputation - impute intermittent missing data | [Imputation](Code/2024/3_Multiple_Imputation.R) |
| 4 - Final data creation - finalise imputed data and structure for analysis | [Finalise data](Code/2024/4_Data_Finalise.R) |
| 5 - LTMLE analysis - using dynamic regimes based on age, using the package 'ltmle' (1). | [LTMLE analysis](Code/2024/5_Dynamic_Regimes.R) |
| 6 - Sensitivty analysis using lower physical activity cut-point. | [Sensitivity 1](Code/2024/6_Dynamic_Regimes_Sensitivity1.R) |
| 7 - Sensitivty analysis excluding variables wholly missing in some waves. | [Sensitivity 1](Code/2024/7_Dynamic_Regimes_Sensitivity2.R) |
| 8 - Pool results across imputations and create analysis figures | [Pool results](Code/2024/8_Pool_Results.R) |
| 9 - Create plots of results using ggplot | [Create plots](Code/2024/9_Create_Plots.R) |
| 10 - Generate 'table 1' of baseline descriptive statistics | [Descriptives](Code/2024/10_Descriptive_Statistics.R) |
| 11 - E-value analysis to test sensitivity to unmeasured confounding | [E-value analysis](Code/2024/11_EValue_Analysis.R) |
| 12 - Create summary of missing data | [Missing data](Code/2024/12_Missing_data_summary.R) |

## Causal effects of physical activity on mortality
Code for all analysis in the article examining the causal effects of meeting physical activity on mortality (Nguyen-duy et al 2025, under review).

| Description | R Code |
| --- | --- |
| 1 - Data extraction - pull relevant variables from each wave | [Data extraction](Code/2025a/1_Data_Extraction.R) |
| 2 - Merge data - merge waves and create derived variables | [Merge data](Code/2025a/2_Data_Merge.R) |
| 3 - Multiple imputation - impute intermittent missing data | [Imputation](Code/2025a/3_Multiple_Imputation.R) |
| 4 - Final data creation - finalise imputed data and structure for analysis | [Finalise data](Code/2025a/4_Data_Finalise.R) |
| 5 - Analysis of all-cause mortality - using dynamic regimes based on age, using the package 'ltmle' (1). | [All-cause analysis](Code/2025a/5_All_cause_analysis.R) |
| 6 - Analysis of CVD mortality - using dynamic regimes based on age, using the package 'ltmle' (1). | [CVD analysis](Code/2025a/6_CVD_analysis.R) |
| 7 - Analysis of Cancer mortality - using dynamic regimes based on age, using the package 'ltmle' (1). | [Cancer analysis](Code/2025a/7_cancer_analysis.R) |
| 8 - Pool results across imputations and create analysis figures | [Pool results](Code/2025a/8_Pool_Results.R) |
| 9 - Create plots to graphically report the analysis findings | [Create plots](Code/2025a/9_Create_Plots.R) |

## Causal effects of loneliness on all-cause mortality
Code for all analysis in the article examining the causal effects of loneliness on mortality (HaGani et al 2025, under review).

| Description | R Code |
| --- | --- |
| 1 - Data extraction - pull relevant variables from each wave | [Data extraction](Code/2025b/1_ALSWH_Extraction.R) |
| 2 - Merge data - merge waves and create derived variables | [Merge data](Code/2025b/2_Merge_code.R) |
| 3 - Multiple imputation - impute intermittent missing data | [Imputation](Code/2025b/3_Multiple_Imputation.R) |
| 4 - Final data creation - finalise imputed data and structure for analysis | [Finalise data](Code/2025b/4_Data_Finalise.R) |
| 5 - Analysis of all-cause mortality - using dynamic regimes based on age, using the package 'ltmle' (1). | [All-cause analysis](Code/2025b/5_All_cause_analysis.R) |
| 6 - Post-hoc sensitivity analysis adjusting for baseline conditions rather than excluding. | [Sensitivity](Code/2025b/6_Post-hoc_sensitivity.R) |
| 7 - Pool results across imputations and create analysis figures | [Pool results](Code/2025b/7_Pool_Results.R) |
| 8 - Create plots to graphically report the analysis findings | [Create plots](Code/2025b/8_Create_Plots.R) |
| 9 - E-Value analysis of unmeasured confounding | [EValue analysis](Code/2025b/9_EValue_Analysis.R) |
| 10 - Missing data summary for appendix | [Missing data](Code/2025b/10_Missing_data_summary.R) |
| 11 - Descriptive statistics on unadjusted mortality incidence. | [Mortality descriptives](Code/2025b/11_Mortality_descriptives.R) |
| 12 - Socio-demographic descriptives for Table 1. | [Sociodemographic descriptives](Code/2025b/12_Socio_demographics.R) |

1. Lendle SD, Schwab J, Petersen ML, van der Laan MJ. ltmle: An R Package Implementing Targeted Minimum Loss-Based Estimation for Longitudinal Data. Journal of Statistical Software. 2017;81(1):1-21.
2. Vermunt JK. Latent Class Modeling with Covariates: Two Improved Three-Step Approaches. Political Analysis. 2010;18(4):450-469.

<!---
## Physical activity trajectories and associations with health-related quality of life
Code for all analysis in the article by Nguyen-duy et al 2024b.

| Description | R Code |
| --- | --- |
| 1 - Data extraction - pull relevant variables from each wave | [Data extraction](Code/2024b/1_Data_Extraction.R) |
| 2 - Multiple imputation - impute intermittent missing data | [Imputation](Code/2024b/2_Multiple_Imputation.R) |
| 3 - Final data creation - finalise imputed data and structure for analysis | [Finalise data](Code/2024b/3_Data_Finalise.R) |
| 4 - Data import - import imputed data into Stata for analysis | [Stata import](Code/2024b/4_Data_Import.do) |
| 5 - Assess model fit - check number of classes using information criteria | [Model fit](Code/2024b/5_Model_Fit.do) |
| 6 - Class probabilities - estimate class probabilities from best fitting model | [Class probabilities](Code/2024b/6_Class_Probabilities.do) |
| 7 - BCH Weights - estimate BCH weights based on BCH method (2). | [BCH Weights](Code/2024b/7_Calculate_BCH_Weights.do) |
| 8 - ML models - regress class membership on baseline covariates using ML method (2). | [ML Models](Code/2024b/8_Latent_Class_Regressions_ML.do) |
| 9 - BCH models - distal outcome models and regression of class membership on baseline covariates using BCH method (2). | [BCH Models](Code/2024b/9_Distal_Models_BCH.do) |
-->

