# ALSWH Project Physical activity trajectories and associations with health-related quality of life and mortality outcomes

This repository contains R code used in articles using the Australian Longitudinal Study of Women's Health (ALSWH).


## Physical activity trajectories and associations with health-related quality of life
Code for all analysis in the article by Nguyen-duy et al 2022.

| Description | R Code |
| --- | --- |
| A1 - Data extraction - pull relevant variables from each wave | [Multiple imputation](Code/2022a/1_Data_Extraction.R) |
| A2 - Merge data - merge waves and create derived variables | [Final data creation](Code/2022a/2_Data_Merge.R) |
| A3 - Multiple imputation - impute intermittent missing data | [LTMLE analysis](Code/2022a/3_Multiple_Imputation.R) |
| A4 - Final data creation - finalise imputed data and structure for analysis | [LTMLE MSM analysis](Code/2022a/4_Data_Finalise.R) |
| A5 - LTMLE analysis - using dynamic regimes based on age, using the package 'ltmle' (1). | [Naive analysis](Code/2022a/5_Dynamic_Regimes.R) |

1. Lendle SD, Schwab J, Petersen ML, van der Laan MJ. ltmle: An R Package Implementing Targeted Minimum Loss-Based Estimation for Longitudinal Data. Journal of Statistical Software. 2017;81(1):1-21.




