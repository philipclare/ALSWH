# Loneliness analysis using international datasets.

This repository contains R and Stata code used in various analyses related to loneliness and social isolation, for the Lancet invited series on loneliness.

## European Social Survey (ESS).

| Description | Code |
| --- | --- |
| 1 - Data combine - pull and combine relevant variables from each wave | [Data combine](Code/ESS/ESS_1_Data_combine.do) |
| 2 - Carry out analysis of regional trends | [Primary analysis](Code/ESS/ESS_2_Analysis.do) |
| 3 - Create primary analysis plots in R | [Create plots](Code/ESS/ESS_3_Create_figure.R) |
| 4 - Estimate prevalence by country | [Prevalence by country](Code/ESS/ESS_4_Prevalence_by_country.do) |
| 5 - Estimate country-level trends | [Country-level trends](Code/ESS/ESS_5_Country_trends.do) |

## Gallup World Poll

| Description | Code |
| --- | --- |
| 1 - Analysis of social support |  |
| 2 - Analysis of living alone | [Living alone](Code/Gallup/GALLUP_2_Livesalone.R) |
| 3 - Create plots | [Create plots](Code/Gallup/GALLUP_3_Create final figure.R) |

## Global School-based Student Health Survey (GSHS)

| Description | Code |
| --- | --- |
| 1 - Data extraction and processing | [Data extraction](Code/GSHS/GSHS_1_Data_processing.R) |
| 2 - Run analysis and create plots | [Analysis](Code/GSHS/GSHS_2_Analysis.R) |

## Programme for International Student Assessment (PISA).

| Description | Code |
| --- | --- |
| 1 - Data extraction - pull relevant variables from each wave | [Data extraction](Code/PISA/PISA_1_Data_combine.do) |
| 2 - Run analysis and create plots | [Analysis](Code/PISA/PISA_2_Create_figure.R) |
| 3 - Analysis of contact with friends | [Merge data](Code/PISA/PISA_3_Friends analysis.do) |

## Survey of Health, Ageing and Retirement in Europe (SHARE).

| Description | Code |
| --- | --- |
| 1 - Data extraction - pull relevant variables from each wave | [Data extraction](Code/SHARE/SHARE_1_Data_merge.do) |
| 2 - Primary analysis of linear trends at region level | [Analysis](Code/SHARE/SHARE_2_Linear_analysis.do) |
| 3 - Secondary analysis of sex-specific trends | [Sex analysis](Code/SHARE/SHARE_3_Linear_analysis_by_sex.do) |
| 4 - Estimation of country-level prevalence | [Country-level](Code/SHARE/SHARE_4_Analysis_by_country.do) |
| 5 - Create plots | [Create plots](Code/SHARE/SHARE_5_Create_linear_figures.R) |

## Social Cohesion analysis using ELSI, HILDA and HRS.

| Description | Code |
| --- | --- |
| 1 - Analysis of ELSI data | [ELSI](Code/SHARE/Cohesion_1_ELSI_analysis.do) |
| 2 - Analysis of HILDA data | [HILDA](Code/SHARE/Cohesion_2_HILDA_analysis.do) |
| 3 - Analysis of HRS data | [HRS](Code/SHARE/Cohesion_3_HRS_analysis.do) |
| 4 - Combine results and create plot | [Create plots](Code/SHARE/Cohesion_4_Create_figures.R) |

