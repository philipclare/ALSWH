# ALSWH Project Physical activity trajectories and associations with health-related quality of life and mortality outcomes

This repository contains R code used in articles using the Australian Longitudinal Study of Women's Health (ALSWH).

## Causal effects of physical activity patterns on health-related quality of life
Code for all analysis in the article by Nguyen-duy et al 2023a.

| Description | R Code |
| --- | --- |
| 1 - Data extraction - pull relevant variables from each wave | [Data extraction](Code/2022a/1_Data_Extraction.R) |
| 2 - Merge data - merge waves and create derived variables | [Merge data](Code/2022a/2_Data_Merge.R) |
| 3 - Multiple imputation - impute intermittent missing data | [Imputation](Code/2022a/3_Multiple_Imputation.R) |
| 4 - Final data creation - finalise imputed data and structure for analysis | [Finalise data](Code/2022a/4_Data_Finalise.R) |
| 5 - LTMLE analysis - using dynamic regimes based on age, using the package 'ltmle' (1). | [Naive analysis](Code/2022a/5_Dynamic_Regimes.R) |
| 6 - Sensitivty analysis using lower physical activity cut-point. | [Sensitivity 1](Code/2022a/6_Dynamic_Regimes_Sensitivity1.R) |
| 7 - Sensitivty analysis excluding variables wholly missing in some waves. | [Sensitivity 1](Code/2022a/7_Dynamic_Regimes_Sensitivity2.R) |
| 8 - Pool results across imputations and create analysis figures | [Pool results](Code/2022a/8_Pool_Results.R) |

## Physical activity trajectories and associations with health-related quality of life
Code for all analysis in the article by Nguyen-duy et al 2023b.

| Description | R Code |
| --- | --- |
| 1 - Data extraction - pull relevant variables from each wave | [Data extraction]](Code/2022b/1_Data_Extraction.R) |
| 2 - Multiple imputation - impute intermittent missing data | [Imputation](Code/2022b/2_Multiple_Imputation.R) |
| 3 - Final data creation - finalise imputed data and structure for analysis | [Finalise data](Code/2022b/3_Data_Finalise.R) |
| 4 - Data import - import imputed data into Stata for analysis | [Stata import](Code/2022b/4_Data_Import.do) |
| 5 - Assess model fit - check number of classes using information criteria | [Model fit](Code/2022b/5_Model_Fit.do) |
| 6 - Class probabilities - estimate class probabilities from best fitting model | [Class probabilities](Code/2022b/6_Class_Probabilities.do) |
| 7 - BCH Weights - estimate BCH weights based on BCH method (2). | [BCH Weights](Code/2022b/7_Calculate_BCH_Weights.do) |
| 8 - ML models - regress class membership on baseline covariates using ML method (2). | [ML Models](Code/2022b/8_Latent_Class_Regressions_ML.do) |
| 9 - BCH models - distal outcome models and regression of class membership on baseline covariates using BCH method (2). | [BCH Models](Code/2022b/9_Distal_Models_BCH.do) |

1. Lendle SD, Schwab J, Petersen ML, van der Laan MJ. ltmle: An R Package Implementing Targeted Minimum Loss-Based Estimation for Longitudinal Data. Journal of Statistical Software. 2017;81(1):1-21.
2. Vermunt JK. Latent Class Modeling with Covariates: Two Improved Three-Step Approaches. Political Analysis. 2010;18(4):450-469.



