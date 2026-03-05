####################################
##    
##
####################################
## Author: Dena Javadi
##
## Date Created: 2024-09-11
##
## ---------------------------
##
## Notes:
##   
##
## ---------------------------

## set working directory 

setwd("")

## ---------------------------

options(scipen = 6, digits = 4) 

## ---------------------------

## load libraries  

library(tidyverse)
library(dplyr)
library(tidyr)
library(ggplot2)
library(gtsummary)
library(lubridate)
library(naniar)
library(haven)
library(sjPlot)
library(sjmisc)
library(data.table)
library(table1)
library(sandwich)

# Rename GWP variables

GWP <- dat %>% dplyr::rename("CE_Help_stranger"= "WP110", "Volunteer" = "WP109", "CE_Donate"= "WP108", "Country_Born" = "WP9048", "Nativity" = "WP4657", "Age"= "WP1220", "Education" ="WP3117", "Gender" = "WP1219", "DE_Anger" = "WP74", "DE_Stress" = "WP71", "PH_Sadness" = "WP70", "DE_Enjoyment"="WP67", "DE_Learn" = "WP65" , "DE_Smile" = "WP63", "DE_Respect" = "WP61", "PH_Worry" = "WP69","PH_Pain" = "WP68" , "PH_Rested"= "WP60",  "PH_HealthProb"= "WP23", "CantrilNow" = "WP16", "Cantril5" ="WP18", "Stand_of_Living" = "WP31", "ID"="wpid", "Country"="WP5", "Year"="YEAR_WAVE", "Employment_"="EMP_2010","Emp_Hours"="EMP_WORK_HOURS", "Income"="INCOME_2", "CB_Transportation"="WP91", "CB_Roads"="WP92", "CB_Air"="WP94","CB_Water"="WP95", "CB_Housing"="WP98","CB_Schools"= "WP93", "CB_Healthcare"= "WP97", "SL_Social_Support"= "WP27", "SL_Make_Friends"= "WP10248", "JC_EconomyBetter" = "WP88", "JC_Job_Market" = "WP89", "DI_Race" = "WP103", "DI_Sexuality"= "WP105", "DI_Immigrants" = "WP106", "FS_Food" ="WP40", "FS_Shelter" = "WP43", "Marital_Stat_" = "WP1223", "SRH" = "WP22", "Religiosity" = "WP119", "Year" = "YEAR_WAVE", "Emp_Engage" = "WP9608", "Outside_country_help" = "WP3333", "Covid_less_hrs" = "WP21760", "Covid_temp_stop_work" = "WP21758", "Covid_lost_job" = "WP21759", "Covid_less_money" = "WP21761")


library(dplyr)
library(tidyr)

# --- 1. Map numeric country codes in GWP to GWP country names -----------------

GWP$Country <- as.factor(GWP$Country)

country_names <- c(
  "United States", "Egypt", "Morocco", "Lebanon", "Saudi Arabia", "Jordan", "Syria", 
  "Turkey", "Pakistan", "Indonesia", "Bangladesh", "United Kingdom", "France", 
  "Germany", "Netherlands", "Belgium", "Spain", "Italy", "Poland", "Hungary", 
  "Czech Republic", "Romania", "Sweden", "Greece", "Denmark", "Iran", "Hong Kong", 
  "Singapore", "Japan", "China", "India", "Venezuela", "Brazil", "Mexico", "Nigeria", 
  "Kenya", "Tanzania", "Israel", "Palestinian Territories", "Ghana", "Uganda", 
  "Benin", "Madagascar", "Malawi", "South Africa", "Canada", "Australia", 
  "Philippines", "Sri Lanka", "Vietnam", "Thailand", "Cambodia", "Laos", "Myanmar", 
  "New Zealand", "Angola", "Botswana", "HOLD", "HOLD", "Ethiopia", "Mali", 
  "Mauritania", "Mozambique", "Niger", "Rwanda", "Senegal", "Zambia", "South Korea", 
  "Taiwan", "Afghanistan", "Belarus", "Georgia", "Kazakhstan", "Kyrgyzstan", 
  "Moldova", "Russia", "Ukraine", "Burkina Faso", "Cameroon", "Sierra Leone", 
  "Zimbabwe", "Costa Rica", "Albania", "Algeria", "Andorra", "Antigua & Barbuda", 
  "Argentina", "Armenia", "Austria", "Azerbaijan", "Bahamas", "Bahrain", "Barbados", 
  "Belize", "Bhutan", "Bolivia", "Bosnia and Herzegovina", "Brunei", "Bulgaria", 
  "Burundi", "Cape Verde", "Central African Republic", "Chad", "Chile", "Colombia", 
  "Comoros", "Congo (Kinshasa)", "Congo Brazzaville", "Croatia", "Cuba", "Cyprus", 
  "Djibouti", "Dominica", "Dominican Republic", "Ecuador", "El Salvador", 
  "Equatorial Guinea", "Eritrea", "Estonia", "Fiji", "Finland", "Gabon", "Grenada", 
  "Guatemala", "Guinea", "Guinea-Bissau", "Guyana", "Haiti", "Honduras", "Iceland", 
  "Iraq", "Ireland", "Island Nations (11)", "Ivory Coast", "Jamaica", "Kiribati", 
  "Kuwait", "Latvia", "Lesotho", "Liberia", "Libya", "Liechtenstein", "Lithuania", 
  "Luxembourg", "North Macedonia", "Malaysia", "Maldives", "Malta", "Marshall Islands", 
  "Mauritius", "Micronesia", "Monaco", "Mongolia", "Montenegro", "Namibia", "Nauru", 
  "Nepal", "Nicaragua", "North Korea", "Norway", "Oman", "Palau", "Panama", 
  "Paraguay", "Peru", "Portugal", "Puerto Rico", "Qatar", "Saint Lucia", "Samoa", 
  "San Marino", "Sao Tome & Principe", "Serbia", "Seychelles", "Slovakia", "Slovenia", 
  "Solomon Islands", "Somalia", "St. Kitts & Nevis", "St. Vincent & Grenadines", 
  "Sudan", "Suriname", "Eswatini", "Switzerland", "Tajikistan", "The Gambia", 
  "Togo", "Tonga", "Trinidad & Tobago", "Tunisia", "Turkmenistan", "Tuvalu", 
  "United Arab Emirates", "Uruguay", "Uzbekistan", "Vanuatu", "Yemen", "Kosovo", 
  "Somaliland region", "US Hispanic", "Timor Leste", "Northern Cyprus", 
  "Nagorno-Karabakh Region", "Papua New Guinea", "South Sudan", "Macau", 
  "Reunion Island"
)

names(country_names) <- 1:207

GWP$Country <- country_names[as.character(GWP$Country)]


# --- 2. Label REG_GLOBAL into broad regions -----------------------------------

GWP <- GWP %>%
  mutate(
    Region = case_when(
      REG_GLOBAL == 1  ~ "European Union",
      REG_GLOBAL == 3  ~ "Europe-Other",
      REG_GLOBAL == 4  ~ "Commonwealth of Independent States",
      REG_GLOBAL == 5  ~ "Australia-New Zealand",
      REG_GLOBAL == 6  ~ "Southeast Asia",
      REG_GLOBAL == 7  ~ "South Asia",
      REG_GLOBAL == 8  ~ "East Asia",
      REG_GLOBAL == 9  ~ "Latin America and the Caribbean",
      REG_GLOBAL == 10 ~ "Northern America",
      REG_GLOBAL == 11 ~ "Middle East and North Africa",
      REG_GLOBAL == 12 ~ "Sub-Saharan Africa",
      TRUE             ~ as.character(REG_GLOBAL)
    )
  )


# --- 3. Merge UN regions -----------------------------------------------------

GWP <- GWP %>%
  left_join(UN, by = "Country")


# --- 4. Gender + social support recodes --------------------------------------

GWP <- GWP %>%
  mutate(
    Gender = ifelse(Gender == "1", "Male",
                    ifelse(Gender == "2", "Female", NA))
  )

GWP$SL_Social_Support <- as.factor(GWP$SL_Social_Support)

GWP <- GWP %>%
  mutate(
    Social_Support = ifelse(SL_Social_Support == "2", "0",
                            ifelse(SL_Social_Support == "1", "1", NA_character_))
  )


# --- 5. Country name harmonization for HDI & GDP -----------------------------

# Map HDI/GDP country names to GWP country names
country_mapping <- data.frame(
  hdi_country = c(
    "Czechia",
    "Iran (Islamic Republic of)",
    "Hong Kong, China (SAR)",
    "Venezuela (Bolivarian Republic of)",
    "Tanzania (United Republic of)",
    "Viet Nam",
    "Lao People's Democratic Republic",
    "Korea (Republic of)",
    "Moldova (Republic of)",
    "Russian Federation",
    "Bolivia (Plurinational State of)",
    "Slovak Republic",
    "Congo",
    "Congo (Democratic Republic of the)",
    "Trinidad and Tobago",
    "Gambia",
    "Syrian Arab Republic",
    "T\xfcrkiye",            
    "C\xf4te d'Ivoire",       
    "Palestine, State of"     
  ),
  gwp_country = c(
    "Czech Republic",
    "Iran",
    "Hong Kong",
    "Venezuela",
    "Tanzania",
    "Vietnam",
    "Laos",
    "South Korea",
    "Moldova",
    "Russia",
    "Bolivia",
    "Slovakia",
    "Congo Brazzaville",
    "Congo (Kinshasa)",
    "Trinidad & Tobago",
    "The Gambia",
    "Syria",
    "Turkey",
    "Ivory Coast",
    "Palestinian Territories"
  ),
  stringsAsFactors = FALSE
)

# Clean HDI country names and make HDI numeric
HDI <- HDI %>%
  left_join(country_mapping, by = c("country" = "hdi_country")) %>%
  mutate(
    country = coalesce(gwp_country, country)
  ) %>%
  select(-gwp_country) %>%
  mutate(
    across(starts_with("hdi_"), as.numeric)
  )

# Clean GDP country names and make GDP numeric
GDP <- GDP %>%
  left_join(country_mapping, by = c("country" = "hdi_country")) %>%
  mutate(
    country = coalesce(gwp_country, country)
  ) %>%
  select(-gwp_country) %>%
  mutate(
    across(starts_with("gdp_"), as.numeric)
  )


# --- 6. Pivot HDI & GDP to long for 2006–2022 --------------------------------

HDI_long <- HDI %>%
  pivot_longer(
    cols      = starts_with("hdi_"),
    names_to  = "Year",
    names_prefix = "hdi_",
    values_to = "HDI"
  ) %>%
  mutate(Year = as.integer(Year)) %>%
  filter(Year >= 2006, Year <= 2022)

GDP_long <- GDP %>%
  pivot_longer(
    cols      = starts_with("gdp_"),
    names_to  = "Year",
    names_prefix = "gdp_",
    values_to = "GDP"
  ) %>%
  mutate(Year = as.integer(Year)) %>%
  filter(Year >= 2006, Year <= 2022)


# --- 7. Merge HDI & GDP onto GWP ---------------------------------------------

GWP_enhanced <- GWP %>%
  left_join(HDI_long, by = c("Country" = "country", "Year" = "Year")) %>%
  left_join(GDP_long, by = c("Country" = "country", "Year" = "Year"))


# FOR LANCET ------------------------------------------------------------------

# set up dataset
GWP_SL <- GWP_enhanced   %>%
   group_by(YEAR_CALENDAR)   %>%
   mutate(
     Age_centered = Age - mean(Age, na.rm = TRUE),
     hhsize_centered = hhsize - mean(hhsize, na.rm = TRUE)
     )   %>%
   ungroup()   %>%
   filter(!is.na(Social_Support))


GWP_SL$Social_Support <- as.integer(as.character(GWP_SL$Social_Support))

# drop na for social support dataset
GWP_SL_NA <- GWP_SL %>%
  dplyr::select(Social_Support,UN.Region, Region2, YEAR_CALENDAR, Gender, Age_centered, Age, hhsize_centered, Marital_Stat_, HDI, GDP, wgt, Country) %>% drop_na() %>%
  filter(YEAR_CALENDAR != "2023")

# drop na for lives alone dataset (fyi it's a lot: 2559883-1705435 = 854,448 obs)

GWP_LA_NA <- GWP_SL %>%
  dplyr::select(hhsize,UN.Region, Region2, YEAR_CALENDAR, Gender, Age_centered, Age, Marital_Stat_, HDI, GDP, wgt, Country) %>% drop_na() %>%
  filter(YEAR_CALENDAR != "2023") %>%
  filter(YEAR_CALENDAR != "2024")

# create lives alone variable from hhsize

GWP_LA_NA <- GWP_LA_NA %>%
  mutate(lives_alone = ifelse(hhsize == 1, 1, 0))

library(parameters)
# scale weights
GWP_SL_NA <- rescale_weights(data =GWP_SL_NA,
                             group = c("Country"),
                             probability_weights= "wgt")

GWP_LA_NA <- rescale_weights(data =GWP_LA_NA,
                             group = c("Country"),
                             probability_weights= "wgt")

# models 4/25 repeated 11/21 for lives alone

GWP_LA_NA$lives_alone <- factor(GWP_LA_NA$lives_alone, levels = c(0,1))


model_glm_LA <- glm(lives_alone ~ Gender + Age_centered + Marital_Stat_ + HDI + GDP + UN.Region * YEAR_CALENDAR,
                 data = GWP_LA_NA,
                 family = binomial(link = "logit"),
                 weights = pweights_a) 

library(sandwich)
library(lmtest)

# Create robust variance-covariance matrix clustered by Country

robust_se_LA <- vcovCL(model_glm_LA, cluster = ~Country)

# Coefficients with robust SEs

coeftest(model_glm_LA, vcov = robust_se_LA)

# graph it
library(marginaleffects)

# Average predicted probability by Region2

margins_region_year_LA <- avg_predictions(
  model_glm_LA,
  by   = c("UN.Region", "YEAR_CALENDAR"),
  vcov = robust_se_LA,
  type = "response"
)

# Merge back Region2 for faceting
region_lookup <- GWP_LA_NA %>%
  distinct(UN.Region, Region2) %>%
  filter(!is.na(UN.Region) & !is.na(Region2))

margins_region_year_LA <- margins_region_year_LA %>%
  left_join(region_lookup, by = "UN.Region")

un_region_colors <- c(
  "Australia and New Zealand" = "darkgrey",     # bold red
  "Central Asia" = "#1C9099",                 # teal
  "Eastern Asia" = "darkred",                 # green
  "Eastern Europe" = "#cf2382",               # deep purple
  "Latin America/Caribbean" = "#FDAE61",      # orange
  "North America" = "#4575B4",                # blue
  "Northern Africa" = "forestgreen",              # light yellow-green
  "Northern Europe" = "darkblue",              #
  "South-eastern Asia" = "#918f10",           # cyan blue
  "Southern Asia" = "#23cfcf",                # coral
  "Southern Europe" = "#292445",              # lime green
  "Sub-Saharan Africa" = "#5E4FA2",           # dark indigo
  "Western Asia" = "#754413",                 # warm yellow
  "Western Europe" = "#2c7513"                # soft turquoise
)

ggplot(margins_region_year_LA, aes(x = YEAR_CALENDAR, y = estimate, color = UN.Region)) +
  geom_line(size = 1) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = UN.Region), alpha = 0.2, color = NA) +
  facet_wrap(~ Region2, ncol = 1, scales = "free_y") + 
  labs(
    title = "Predicted Living Alone Over Time",
    x = "Year",
    y = "Predicted Probability of Living Alone"
  ) +
  theme_minimal() +
  scale_color_manual(values = un_region_colors) +
  scale_fill_manual(values = un_region_colors) +
  theme(legend.position = "bottom")

ggsave(
  filename = "GWP_lives_alone_region.png",
  plot = last_plot(),        
  width = 10,
  height = 8,
  dpi = 300,
  bg = "white"           
)


# add age for lives alone
# For three categories
GWP_LA_NA <- GWP_LA_NA %>%
  mutate(
    Age_Group_3 = case_when(
      Age >= 15 & Age <= 24 ~ "Youth (15-24)",
      Age >= 25 & Age <= 59 ~ "Adult (25-59)",
      Age >= 60 ~ "Older Adult (60+)"
    ),
    
    # For binary split at 65
    Age_Group_2 = case_when(
      Age < 60 ~ "Younger (<60)",
      Age >= 60 ~ "Older (60+)"
    )
  )


GWP_LA_YA <- GWP_LA_NA %>% filter(Age_Group_3 == "Youth (15-24)")
GWP_LA_MA<- GWP_LA_NA %>% filter(Age_Group_3 == "Adult (25-59)")
GWP_LA_OA<- GWP_LA_NA %>% filter(Age_Group_3 == "Older Adult (60+)")

GWP_LA_Young <- GWP_LA_NA %>% filter(Age_Group_2 == "Younger (<60)")
GWP_LA_Old <- GWP_LA_NA %>% filter(Age_Group_2 == "Older (60+)")

# run stratified models
run_model_and_get_margins <- function(data, group_label) {
  # Fit weighted logistic regression
  model <- glm(
    lives_alone ~ Gender + Marital_Stat_ +
      HDI + GDP + UN.Region * YEAR_CALENDAR,
    data   = data,
    family = binomial(link = "logit"),
    weights = pweights_a   # make sure this column exists in `data`
  )
  
  # Cluster-robust VCOV by Country
  se <- vcovCL(model, cluster = ~ Country)
  
  # Average predicted probability by UN.Region × YEAR_CALENDAR
  margins <- avg_predictions(
    model,
    by   = c("UN.Region", "YEAR_CALENDAR"),
    vcov = se,
    type = "response"
  ) %>%
    mutate(Age_Group = group_label)
  
  return(margins)
}
 
# 3-category age group
margins_YA <- run_model_and_get_margins(GWP_LA_YA, "Youth (15-24)")
margins_MA <- run_model_and_get_margins(GWP_LA_MA, "Adult (25-59)")
margins_OA <- run_model_and_get_margins(GWP_LA_OA, "Older Adult (60+)")

margins_3cat <- bind_rows(margins_YA, margins_MA, margins_OA)

# 2-category age group
margins_Young <- run_model_and_get_margins(GWP_LA_Young, "Younger (<60)")
margins_Old <- run_model_and_get_margins(GWP_LA_Old, "Older (60+)")

margins_2cat <- bind_rows(margins_Young, margins_Old)

margins_2cat <- margins_2cat %>%
  left_join(region_lookup, by = "UN.Region")

margins_3cat <- margins_3cat %>%
  left_join(region_lookup, by = "UN.Region")

margins_3cat <- margins_3cat %>% mutate(Age_Group = factor(Age_Group, levels = c("Youth (15-24)", "Adult (25-59)", "Older Adult (60+)")))

ggplot(margins_3cat, aes(x = YEAR_CALENDAR, y = estimate, color = UN.Region, fill = UN.Region)) +
  geom_line(size = 1) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2, color = NA) +
  facet_grid(Age_Group ~ Region2) +
  scale_color_manual(values = un_region_colors) +
  scale_fill_manual(values = un_region_colors) +
  labs(
    title = "Predicted living alone over time by age (3 category) and region",
    x = "Year",
    y = "Predicted Probability",
    color = "UN Region",
    fill = "UN Region"
  ) +
  theme_minimal(base_size = 13) +
  theme(legend.position = "bottom")

ggsave(
  filename = "GWP_lives_alone_region_age3.png",
  plot = last_plot(),        
  width = 10,
  height = 8,
  dpi = 300,
  bg = "white"           
)


ggplot(margins_2cat, aes(x = YEAR_CALENDAR, y = estimate, color = UN.Region, fill = UN.Region)) +
  geom_line(size = 1) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2, color = NA) +
  facet_grid(Age_Group ~ Region2) +
  scale_color_manual(values = un_region_colors) +
  scale_fill_manual(values = un_region_colors) +
  labs(
    title = "Predicted living alone over time by age (60+ split) and region",
    x = "Year",
    y = "Predicted Probability"
  ) +
  theme_minimal()

ggsave(
  filename = "GWP_lives_alone_region_age2.png",
  plot = last_plot(),        
  width = 10,
  height = 8,
  dpi = 300,
  bg = "white"           
)

library(writexl)
write_xlsx(margins_region_year_LA, "Lives_alone_region.xlsx")
write_xlsx(margins_2cat, "Lives_alone_2agecats.xlsx")
write_xlsx(margins_3cat, "Lives_alone_3agecats.xlsx")
