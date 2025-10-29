# Analysis of the Household Income and Labour Dynamics Australia (HILDA) survey

This repository contains R code used in articles using the HILDA cohort.

## Trends in loneliness in Australia
Code for all analysis in the article examining trends in loneliness and social isolation in Australia (Ding et al 2025).

| Description | R Code |
| --- | --- |
| 1 - Data extraction - pull relevant variables from HILDA data and combine into single dataset. | [Data extraction](Code/2025/1_Data_combine.do) |
| 2 - Create summary of missing data. | [Missing data](Code/2025/2_Missing_data.R) |
| 3 - Test linearity of trends. | [Linearity](Code/2025/3_Linearity_tests.do) |
| 4 - Calculate observed, unadjusted prevalence. | [Raw trends](Code/2025/4_Raw_trends.do) |
| 5 - Model trends in prevalence over time. | [Modeled trends](Code/2025/5_Modeled_trends.do) |
| 6 - Create overall plots of prevalence. | [Create plots](Code/2025/6_Create_plots.R) |
| 7 - Create plots stratified by levels of sociodemographic variables. | [Demographic plots](Code/2025/7_Create_plots_by_demographics.R) |
| 8 - Calculate p-values of interaction terms. | [Interaction p-values](Code/2025/8_Demographic_interaction_pvalues.do) |
| 9 - Test linearity, allowing for trend to change due to COVID. | [COVID linearity](Code/2025/9_Linearity_tests_of_COVID_impact.do) |
| 10 - Model trends, allowing for trend to change due to COVID. | [COVID models](Code/2025/10_Modeled_trends_of_COVID_impact.do) |
| 11 - Create plots for trends, allowing for trend to change due to COVID. | [COVID plots](Code/2025/11_Test_COVID_impact_figure.R) |


