# Feature tranformation and descriptive stats 



# Importing our libraries
if (!require("dplyr")) install.packages("dplyr") 
library(dplyr)

if (!require("readxl")) install.packages("readxl") 
library(readxl)

if (!require("lubridate")) install.packages("lubridate") 
library(lubridate)

if (!require("ggplot2")) install.packages("ggplot2")
library(ggplot2)

if (!require("data.table")) install.packages("data.table")
library(data.table)

if (!require("stringi")) install.packages("stringi")
library(stringi)

if (!require("stringr")) install.packages("stringr")
library(stringr)

if (!require("tidyr")) install.packages("tidyr")
library(tidyr)

if (!require("ellipsis")) install.packages("ellipsis")
library(ellipsis)


final_happy_git <- read.csv('https://raw.githubusercontent.com/annalenahellerbse/DataScience_FinallyHappy/refs/heads/main/finally_happy.csv')
colnames(final_happy_git)

# Drop the year 2023
finally_happy <- final_happy_git[final_happy_git$year != 2023, ]

#####################################################
#### 1. unemploymemt_rate
#####################################################
 
#correlation of unemploymemt_rate is high with perc corr and generosity 

# Step 1: Create a dataset with year, country, unemployment_rate, perc_corr, and generosity
unemployment_rate_data <- finally_happy %>%
  select(year, country, unemployment_rate, perc_corr, generosity)

# Step 2: Assign GDP and Poverty Categories Independently
country_stats <- unemployment_rate_data %>%
  group_by(country) %>%
  summarise(
    mean_perc_corr = mean(perc_corr, na.rm = TRUE),
    mean_generosity = mean(generosity, na.rm = TRUE)
  ) %>%
  mutate(
    perc_corr_category = ntile(mean_perc_corr, 4),
    generosity_category = ntile(mean_generosity, 4)
  )

# Merge GDP and Poverty categories
unemployment_rate_data <- unemployment_rate_data %>%
  left_join(country_stats, by = "country")

# Replace NAs using calculated medians
unemployment_rate_data <- unemployment_rate_data %>%
  group_by(perc_corr_category, generosity_category, year) %>%
  mutate(
    median_unemployment_rate = median(unemployment_rate, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  mutate(
    unemployment_rate = ifelse(is.na(unemployment_rate), median_unemployment_rate, unemployment_rate)
  ) %>%
  select(-median_unemployment_rate)

# Step 4: Handle cases where one variable is missing
perc_corr_only_medians <- unemployment_rate_data %>%
  group_by(perc_corr_category, year) %>%
  summarise(median_perc_corr_only = median(unemployment_rate, na.rm = TRUE))

generosity_only_medians <- unemployment_rate_data %>%
  group_by(generosity_category, year) %>%
  summarise(median_generosity_only = median(unemployment_rate, na.rm = TRUE))

unemployment_rate_data <- unemployment_rate_data %>%
  left_join(perc_corr_only_medians, by = c("perc_corr_category", "year")) %>%
  left_join(generosity_only_medians, by = c("generosity_category", "year")) %>%
  mutate(
    unemployment_rate = ifelse(
      is.na(unemployment_rate),
      ifelse(!is.na(median_perc_corr_only), median_perc_corr_only, median_generosity_only),
      unemployment_rate
    )
  ) %>%
  select(-median_perc_corr_only, -median_generosity_only)

# Update unemployment_rate directly in finally_happy
finally_happy <- finally_happy %>%
  left_join(unemployment_rate_data %>% select(year, country, unemployment_rate), 
            by = c("year", "country")) %>%
  mutate(unemployment_rate = coalesce(unemployment_rate.y, unemployment_rate.x)) %>%
  select(-unemployment_rate.x, -unemployment_rate.y)


#####################################################
#### 2. participation_rate
#####################################################

# Correlation of participation_rate is high with savings and happiness_score

# Step 1: Create a dataset with year, country, participation_rate, savings, and happiness_score
participation_rate_data <- finally_happy %>%
  select(year, country, participation_rate, savings, happiness_score)

# Step 2: Assign Categories for savings and happiness_score Independently
country_stats <- participation_rate_data %>%
  group_by(country) %>%
  summarise(
    mean_savings = mean(savings, na.rm = TRUE),
    mean_happiness_score = mean(happiness_score, na.rm = TRUE)
  ) %>%
  mutate(
    savings_category = ntile(mean_savings, 4),
    happiness_category = ntile(mean_happiness_score, 4)
  )

# Merge savings and happiness_score categories
participation_rate_data <- participation_rate_data %>%
  left_join(country_stats, by = "country")

# Replace NAs using calculated medians
participation_rate_data <- participation_rate_data %>%
  group_by(savings_category, happiness_category, year) %>%
  mutate(
    median_participation_rate = median(participation_rate, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  mutate(
    participation_rate = ifelse(is.na(participation_rate), median_participation_rate, participation_rate)
  ) %>%
  select(-median_participation_rate)

# Step 4: Handle cases where one variable is missing
savings_only_medians <- participation_rate_data %>%
  group_by(savings_category, year) %>%
  summarise(median_savings_only = median(participation_rate, na.rm = TRUE))

happiness_only_medians <- participation_rate_data %>%
  group_by(happiness_category, year) %>%
  summarise(median_happiness_only = median(participation_rate, na.rm = TRUE))

participation_rate_data <- participation_rate_data %>%
  left_join(savings_only_medians, by = c("savings_category", "year")) %>%
  left_join(happiness_only_medians, by = c("happiness_category", "year")) %>%
  mutate(
    participation_rate = ifelse(
      is.na(participation_rate),
      ifelse(!is.na(median_savings_only), median_savings_only, median_happiness_only),
      participation_rate
    )
  ) %>%
  select(-median_savings_only, -median_happiness_only)

# Step 5: Update participation_rate directly in finally_happy
finally_happy <- finally_happy %>%
  left_join(participation_rate_data %>% select(year, country, participation_rate), 
            by = c("year", "country")) %>%
  mutate(participation_rate = coalesce(participation_rate.y, participation_rate.x)) %>%
  select(-participation_rate.x, -participation_rate.y)




#####################################################
#### 3. working_poverty_rate
#####################################################

# Correlation of working_poverty_rate is high with ln_gpd_per_cap and perc_corr

# Step 1: Create a dataset with year, country, working_poverty_rate, ln_gpd_per_cap, and perc_corr
working_poverty_rate_data <- finally_happy %>%
  select(year, country, working_poverty_rate, ln_gpd_per_cap, perc_corr)

# Step 2: Assign Categories for ln_gpd_per_cap and perc_corr Independently
country_stats <- working_poverty_rate_data %>%
  group_by(country) %>%
  summarise(
    mean_ln_gpd_per_cap = mean(ln_gpd_per_cap, na.rm = TRUE),
    mean_perc_corr = mean(perc_corr, na.rm = TRUE)
  ) %>%
  mutate(
    ln_gpd_per_cap_category = ntile(mean_ln_gpd_per_cap, 4),
    perc_corr_category = ntile(mean_perc_corr, 4)
  )

# Merge ln_gpd_per_cap and perc_corr categories
working_poverty_rate_data <- working_poverty_rate_data %>%
  left_join(country_stats, by = "country")

# Replace NAs using calculated medians
working_poverty_rate_data <- working_poverty_rate_data %>%
  group_by(ln_gpd_per_cap_category, perc_corr_category, year) %>%
  mutate(
    mean_working_poverty_rate = mean(working_poverty_rate, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  mutate(
    working_poverty_rate = ifelse(is.na(working_poverty_rate), mean_working_poverty_rate, working_poverty_rate)
  ) %>%
  select(-mean_working_poverty_rate)

# Step 4: Handle cases where one variable is missing
ln_gpd_per_cap_only_medians <- working_poverty_rate_data %>%
  group_by(ln_gpd_per_cap_category, year) %>%
  summarise(median_ln_gpd_per_cap_only = median(working_poverty_rate, na.rm = TRUE))

perc_corr_only_medians <- working_poverty_rate_data %>%
  group_by(perc_corr_category, year) %>%
  summarise(median_perc_corr_only = median(working_poverty_rate, na.rm = TRUE))

working_poverty_rate_data <- working_poverty_rate_data %>%
  left_join(ln_gpd_per_cap_only_medians, by = c("ln_gpd_per_cap_category", "year")) %>%
  left_join(perc_corr_only_medians, by = c("perc_corr_category", "year")) %>%
  mutate(
    working_poverty_rate = ifelse(
      is.na(working_poverty_rate),
      ifelse(!is.na(median_ln_gpd_per_cap_only), median_ln_gpd_per_cap_only, median_perc_corr_only),
      working_poverty_rate
    )
  ) %>%
  select(-median_ln_gpd_per_cap_only, -median_perc_corr_only)

# Step 5: Update working_poverty_rate directly in finally_happy
finally_happy <- finally_happy %>%
  left_join(working_poverty_rate_data %>% select(year, country, working_poverty_rate), 
            by = c("year", "country")) %>%
  mutate(working_poverty_rate = coalesce(working_poverty_rate.y, working_poverty_rate.x)) %>%
  select(-working_poverty_rate.x, -working_poverty_rate.y)


sum(is.na(finally_happy$working_poverty_rate)) #check 


############################################################
### #4. Accumulated wealth
############################################################

# Step 1: Create a dataset with year, country, accum_wealth, ln_gpd_per_cap, and working_poverty_rate
accum_wealth_data <- finally_happy %>%
  select(year, country, accum_wealth, ln_gpd_per_cap, working_poverty_rate)

# Step 2: Assign GDP and Poverty Categories Independently
country_stats <- accum_wealth_data %>%
  group_by(country) %>%
  summarise(
    mean_gdp_per_cap = mean(ln_gpd_per_cap, na.rm = TRUE),
    mean_poverty_rate = mean(working_poverty_rate, na.rm = TRUE)
  ) %>%
  mutate(
    gdp_category = ntile(mean_gdp_per_cap, 4),
    poverty_category = ntile(mean_poverty_rate, 4)
  )

# Merge GDP and Poverty categories
accum_wealth_data <- accum_wealth_data %>%
  left_join(country_stats, by = "country")

# Replace NAs using calculated medians
accum_wealth_data <- accum_wealth_data %>%
  group_by(gdp_category, poverty_category, year) %>%
  mutate(
    median_accum_wealth = median(accum_wealth, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  mutate(
    accum_wealth = ifelse(is.na(accum_wealth), median_accum_wealth, accum_wealth)
  ) %>%
  select(-median_accum_wealth)

# Step 4: Handle cases where one variable is missing
gdp_only_medians <- accum_wealth_data %>%
  group_by(gdp_category, year) %>%
  summarise(median_gdp_only = median(accum_wealth, na.rm = TRUE))

poverty_only_medians <- accum_wealth_data %>%
  group_by(poverty_category, year) %>%
  summarise(median_poverty_only = median(accum_wealth, na.rm = TRUE))

accum_wealth_data <- accum_wealth_data %>%
  left_join(gdp_only_medians, by = c("gdp_category", "year")) %>%
  left_join(poverty_only_medians, by = c("poverty_category", "year")) %>%
  mutate(
    accum_wealth = ifelse(
      is.na(accum_wealth),
      ifelse(!is.na(median_gdp_only), median_gdp_only, median_poverty_only),
      accum_wealth
    )
  ) %>%
  select(-median_gdp_only, -median_poverty_only)

# Update accum_wealth directly in finally_happy
finally_happy <- finally_happy %>%
  left_join(accum_wealth_data %>% select(year, country, accum_wealth), 
            by = c("year", "country")) %>%
  mutate(accum_wealth = coalesce(accum_wealth.y, accum_wealth.x)) %>%
  select(-accum_wealth.x, -accum_wealth.y)

sum(is.na(finally_happy$accum_wealth)) #check 


############################################################
### 5. Wealth Inequality
############################################################

# Repeat similar logic for wealth_inequ_top10
wealth_inequ_data <- finally_happy %>%
  select(year, country, wealth_inequ_top10, ln_gpd_per_cap, happiness_score)

country_stats <- wealth_inequ_data %>%
  group_by(country) %>%
  summarise(
    mean_gdp_per_cap = mean(ln_gpd_per_cap, na.rm = TRUE),
    mean_happiness_score = mean(happiness_score, na.rm = TRUE)
  ) %>%
  mutate(
    gdp_category = ntile(mean_gdp_per_cap, 4),
    happiness_category = ntile(mean_happiness_score, 4)
  )

wealth_inequ_data <- wealth_inequ_data %>%
  left_join(country_stats, by = "country")

wealth_inequ_data <- wealth_inequ_data %>%
  group_by(gdp_category, happiness_category, year) %>%
  mutate(
    median_wealth_inequ = median(wealth_inequ_top10, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  mutate(
    wealth_inequ_top10 = ifelse(is.na(wealth_inequ_top10), median_wealth_inequ, wealth_inequ_top10)
  ) %>%
  select(-median_wealth_inequ)

# Handle cases where one variable is missing
gdp_only_medians <- wealth_inequ_data %>%
  group_by(gdp_category, year) %>%
  summarise(median_gdp_only = median(wealth_inequ_top10, na.rm = TRUE))

happiness_only_medians <- wealth_inequ_data %>%
  group_by(happiness_category, year) %>%
  summarise(median_happiness_only = median(wealth_inequ_top10, na.rm = TRUE))

wealth_inequ_data <- wealth_inequ_data %>%
  left_join(gdp_only_medians, by = c("gdp_category", "year")) %>%
  left_join(happiness_only_medians, by = c("happiness_category", "year")) %>%
  mutate(
    wealth_inequ_top10 = ifelse(
      is.na(wealth_inequ_top10),
      ifelse(!is.na(median_gdp_only), median_gdp_only, median_happiness_only),
      wealth_inequ_top10
    )
  ) %>%
  select(-median_gdp_only, -median_happiness_only)

finally_happy <- finally_happy %>%
  left_join(wealth_inequ_data %>% select(year, country, wealth_inequ_top10), 
            by = c("year", "country")) %>%
  mutate(wealth_inequ_top10 = coalesce(wealth_inequ_top10.y, wealth_inequ_top10.x)) %>%
  select(-wealth_inequ_top10.x, -wealth_inequ_top10.y)

sum(is.na(finally_happy$wealth_inequ_top10)) #check 


############################################################
### 6. Happiness Score
############################################################

# Repeat similar logic for happiness_score
happiness_data <- finally_happy %>%
  select(year, country, happiness_score, ln_gpd_per_cap, healthy_life_exp)

country_stats <- happiness_data %>%
  group_by(country) %>%
  summarise(
    mean_gdp_per_cap = mean(ln_gpd_per_cap, na.rm = TRUE),
    mean_healthy_life_exp = mean(healthy_life_exp, na.rm = TRUE)
  ) %>%
  mutate(
    gdp_category = ntile(mean_gdp_per_cap, 4),
    health_category = ntile(mean_healthy_life_exp, 4)
  )

happiness_data <- happiness_data %>%
  left_join(country_stats, by = "country")

happiness_data <- happiness_data %>%
  group_by(gdp_category, health_category, year) %>%
  mutate(
    median_happiness_score = median(happiness_score, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  mutate(
    happiness_score = ifelse(is.na(happiness_score), median_happiness_score, happiness_score)
  ) %>%
  select(-median_happiness_score)

# Handle cases where one variable is missing
gdp_only_medians <- happiness_data %>%
  group_by(gdp_category, year) %>%
  summarise(median_gdp_only = median(happiness_score, na.rm = TRUE))

health_only_medians <- happiness_data %>%
  group_by(health_category, year) %>%
  summarise(median_health_only = median(happiness_score, na.rm = TRUE))

happiness_data <- happiness_data %>%
  left_join(gdp_only_medians, by = c("gdp_category", "year")) %>%
  left_join(health_only_medians, by = c("health_category", "year")) %>%
  mutate(
    happiness_score = ifelse(
      is.na(happiness_score),
      ifelse(!is.na(median_gdp_only), median_gdp_only, median_health_only),
      happiness_score
    )
  ) %>%
  select(-median_gdp_only, -median_health_only)

finally_happy <- finally_happy %>%
  left_join(happiness_data %>% select(year, country, happiness_score), 
            by = c("year", "country")) %>%
  mutate(happiness_score = coalesce(happiness_score.y, happiness_score.x)) %>%
  select(-happiness_score.x, -happiness_score.y)


sum(is.na(finally_happy$happiness_score)) #check 


############################################
## 7. - Inflation_rate
############################################


# Step 1: Create a dataset with year, country, and inflation_rate
inflation_data <- finally_happy %>%
  dplyr::select(year, country, inflation_rate, ln_gpd_per_cap, working_poverty_rate)

# Step 2: Assign GDP and Poverty Categories Independently
country_stats <- inflation_data %>%
  group_by(country) %>%
  summarise(
    mean_gdp_per_cap = mean(ln_gpd_per_cap, na.rm = TRUE),
    mean_poverty_rate = mean(working_poverty_rate, na.rm = TRUE)
  ) %>%
  mutate(
    gdp_category = ntile(mean_gdp_per_cap, 4),          # Quartiles for GDP
    poverty_category = ntile(mean_poverty_rate, 4)     # Quartiles for Poverty
  )

# Merge GDP and Poverty categories
inflation_data <- inflation_data %>%
  left_join(country_stats, by = "country")

# Step 3: Replace NAs using calculated medians
inflation_data <- inflation_data %>%
  group_by(gdp_category, poverty_category, year) %>%
  mutate(
    median_inflation_rate = median(inflation_rate, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  mutate(
    inflation_rate = ifelse(is.na(inflation_rate), median_inflation_rate, inflation_rate)
  ) %>%
  dplyr::select(-median_inflation_rate)

# Step 4: Handle cases where one variable is missing
gdp_only_medians <- inflation_data %>%
  group_by(gdp_category, year) %>%
  summarise(median_gdp_only = median(inflation_rate, na.rm = TRUE))

poverty_only_medians <- inflation_data %>%
  group_by(poverty_category, year) %>%
  summarise(median_poverty_only = median(inflation_rate, na.rm = TRUE))

inflation_data <- inflation_data %>%
  left_join(gdp_only_medians, by = c("gdp_category", "year")) %>%
  left_join(poverty_only_medians, by = c("poverty_category", "year")) %>%
  mutate(
    inflation_rate = ifelse(
      is.na(inflation_rate),
      ifelse(!is.na(median_gdp_only), median_gdp_only, median_poverty_only),
      inflation_rate
    )
  ) %>%
  dplyr::select(-median_gdp_only, -median_poverty_only)

# Step 5: Update inflation_rate directly in finally_happy
finally_happy <- finally_happy %>%
  left_join(inflation_data %>% dplyr::select(year, country, inflation_rate), 
            by = c("year", "country")) %>%
  mutate(inflation_rate = coalesce(inflation_rate.y, inflation_rate.x)) %>%
  dplyr::select(-inflation_rate.x, -inflation_rate.y)

sum(is.na(finally_happy$inflation_rate)) #check


#####################################################
#### 8. savings
#####################################################

# Correlation of savings is high with generosity and participation_rate

# Step 1: Create a dataset with year, country, savings, generosity, and participation_rate
savings_data <- finally_happy %>%
  select(year, country, savings, generosity, participation_rate)

# Step 2: Assign Categories for generosity and participation_rate Independently
country_stats <- savings_data %>%
  group_by(country) %>%
  summarise(
    mean_generosity = mean(generosity, na.rm = TRUE),
    mean_participation_rate = mean(participation_rate, na.rm = TRUE)
  ) %>%
  mutate(
    generosity_category = ntile(mean_generosity, 4),
    participation_rate_category = ntile(mean_participation_rate, 4)
  )

# Merge generosity and participation_rate categories
savings_data <- savings_data %>%
  left_join(country_stats, by = "country")

# Replace NAs using calculated medians
savings_data <- savings_data %>%
  group_by(generosity_category, participation_rate_category, year) %>%
  mutate(
    median_savings = median(savings, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  mutate(
    savings = ifelse(is.na(savings), median_savings, savings)
  ) %>%
  select(-median_savings)

# Step 4: Handle cases where one variable is missing
generosity_only_medians <- savings_data %>%
  group_by(generosity_category, year) %>%
  summarise(median_generosity_only = median(savings, na.rm = TRUE))

participation_rate_only_medians <- savings_data %>%
  group_by(participation_rate_category, year) %>%
  summarise(median_participation_rate_only = median(savings, na.rm = TRUE))

savings_data <- savings_data %>%
  left_join(generosity_only_medians, by = c("generosity_category", "year")) %>%
  left_join(participation_rate_only_medians, by = c("participation_rate_category", "year")) %>%
  mutate(
    savings = ifelse(
      is.na(savings),
      ifelse(!is.na(median_generosity_only), median_generosity_only, median_participation_rate_only),
      savings
    )
  ) %>%
  select(-median_generosity_only, -median_participation_rate_only)

# Step 5: Update savings directly in finally_happy
finally_happy <- finally_happy %>%
  left_join(savings_data %>% select(year, country, savings), 
            by = c("year", "country")) %>%
  mutate(savings = coalesce(savings.y, savings.x)) %>%
  select(-savings.x, -savings.y)


sum(is.na(finally_happy$savings)) #check

colnames(finally_happy)

#####################################################
#### 9. healthy_life_exp
#####################################################

# Correlation of healthy_life_exp is high with ln_gdp_per_cap and working_poverty_rate

# Step 1: Create a dataset with year, country, healthy_life_exp, ln_gdp_per_cap, and working_poverty_rate
healthy_life_exp_data <- finally_happy %>%
  select(year, country, healthy_life_exp, ln_gpd_per_cap, working_poverty_rate)

# Step 2: Assign Categories for ln_gdp_per_cap and working_poverty_rate Independently
country_stats <- healthy_life_exp_data %>%
  group_by(country) %>%
  summarise(
    mean_ln_gdp_per_cap = mean(ln_gpd_per_cap, na.rm = TRUE),
    mean_working_poverty_rate = mean(working_poverty_rate, na.rm = TRUE)
  ) %>%
  mutate(
    ln_gdp_per_cap_category = ntile(mean_ln_gdp_per_cap, 4),
    working_poverty_rate_category = ntile(mean_working_poverty_rate, 4)
  )

# Merge ln_gdp_per_cap and working_poverty_rate categories
healthy_life_exp_data <- healthy_life_exp_data %>%
  left_join(country_stats, by = "country")

# Replace NAs using calculated medians
healthy_life_exp_data <- healthy_life_exp_data %>%
  group_by(ln_gdp_per_cap_category, working_poverty_rate_category, year) %>%
  mutate(
    median_healthy_life_exp = median(healthy_life_exp, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  mutate(
    healthy_life_exp = ifelse(is.na(healthy_life_exp), median_healthy_life_exp, healthy_life_exp)
  ) %>%
  select(-median_healthy_life_exp)

# Step 4: Handle cases where one variable is missing
ln_gdp_per_cap_only_medians <- healthy_life_exp_data %>%
  group_by(ln_gdp_per_cap_category, year) %>%
  summarise(median_ln_gdp_per_cap_only = median(healthy_life_exp, na.rm = TRUE))

working_poverty_rate_only_medians <- healthy_life_exp_data %>%
  group_by(working_poverty_rate_category, year) %>%
  summarise(median_working_poverty_rate_only = median(healthy_life_exp, na.rm = TRUE))

healthy_life_exp_data <- healthy_life_exp_data %>%
  left_join(ln_gdp_per_cap_only_medians, by = c("ln_gdp_per_cap_category", "year")) %>%
  left_join(working_poverty_rate_only_medians, by = c("working_poverty_rate_category", "year")) %>%
  mutate(
    healthy_life_exp = ifelse(
      is.na(healthy_life_exp),
      ifelse(!is.na(median_ln_gdp_per_cap_only), median_ln_gdp_per_cap_only, median_working_poverty_rate_only),
      healthy_life_exp
    )
  ) %>%
  select(-median_ln_gdp_per_cap_only, -median_working_poverty_rate_only)

# Step 5: Update healthy_life_exp directly in finally_happy
finally_happy <- finally_happy %>%
  left_join(healthy_life_exp_data %>% select(year, country, healthy_life_exp), 
            by = c("year", "country")) %>%
  mutate(healthy_life_exp = coalesce(healthy_life_exp.y, healthy_life_exp.x)) %>%
  select(-healthy_life_exp.x, -healthy_life_exp.y)


sum(is.na(finally_happy$healthy_life_exp)) # Check


#####################################################
#### 10. generosity
#####################################################

# Correlation of generosity is high with unemployment_rate and savings

# Step 1: Create a dataset with year, country, generosity, unemployment_rate, and savings
generosity_data <- finally_happy %>%
  select(year, country, generosity, unemployment_rate, savings)

# Step 2: Assign Categories for unemployment_rate and savings Independently
country_stats <- generosity_data %>%
  group_by(country) %>%
  summarise(
    mean_unemployment_rate = mean(unemployment_rate, na.rm = TRUE),
    mean_savings = mean(savings, na.rm = TRUE)
  ) %>%
  mutate(
    unemployment_rate_category = ntile(mean_unemployment_rate, 4),
    savings_category = ntile(mean_savings, 4)
  )

# Merge unemployment_rate and savings categories
generosity_data <- generosity_data %>%
  left_join(country_stats, by = "country")

# Replace NAs using calculated medians
generosity_data <- generosity_data %>%
  group_by(unemployment_rate_category, savings_category, year) %>%
  mutate(
    median_generosity = median(generosity, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  mutate(
    generosity = ifelse(is.na(generosity), median_generosity, generosity)
  ) %>%
  select(-median_generosity)

# Step 4: Handle cases where one variable is missing
unemployment_rate_only_medians <- generosity_data %>%
  group_by(unemployment_rate_category, year) %>%
  summarise(median_unemployment_rate_only = median(generosity, na.rm = TRUE))

savings_only_medians <- generosity_data %>%
  group_by(savings_category, year) %>%
  summarise(median_savings_only = median(generosity, na.rm = TRUE))

# Replace all NA values with overall_median_generosity in the savings_only_medians dataframe (2005)
overall_median_generosity <- median(finally_happy$generosity, na.rm=TRUE)
savings_only_medians <- savings_only_medians %>%
  mutate(across(everything(), ~ replace(., is.na(.), overall_median_generosity)))


generosity_data <- generosity_data %>%
  left_join(unemployment_rate_only_medians, by = c("unemployment_rate_category", "year")) %>%
  left_join(savings_only_medians, by = c("savings_category", "year")) %>%
  mutate(
    generosity = ifelse(
      is.na(generosity),
      ifelse(!is.na(median_unemployment_rate_only), median_unemployment_rate_only, median_savings_only),
      generosity
    )
  ) %>%
  select(-median_unemployment_rate_only, -median_savings_only)

# Step 5: Update generosity directly in finally_happy
finally_happy <- finally_happy %>%
  left_join(generosity_data %>% select(year, country, generosity), 
            by = c("year", "country")) %>%
  mutate(generosity = coalesce(generosity.y, generosity.x)) %>%
  select(-generosity.x, -generosity.y)


sum(is.na(finally_happy$generosity)) # 89 remained - 2005 has no observation at all hence median in all groups is NA - replacced by overall median


#####################################################
#### 11. perc_corr
#####################################################

# Correlation of perc_corr is high with unemployment_rate and accum_wealth

# Step 1: Create a dataset with year, country, perc_corr, unemployment_rate, and accum_wealth
perc_corr_data <- finally_happy %>%
  select(year, country, perc_corr, unemployment_rate, accum_wealth)

# Step 2: Assign Categories for unemployment_rate and accum_wealth Independently
country_stats <- perc_corr_data %>%
  group_by(country) %>%
  summarise(
    mean_unemployment_rate = mean(unemployment_rate, na.rm = TRUE),
    mean_accum_wealth = mean(accum_wealth, na.rm = TRUE)
  ) %>%
  mutate(
    unemployment_rate_category = ntile(mean_unemployment_rate, 4),
    accum_wealth_category = ntile(mean_accum_wealth, 4)
  )

# Merge unemployment_rate and accum_wealth categories
perc_corr_data <- perc_corr_data %>%
  left_join(country_stats, by = "country")

# Replace NAs using calculated medians
perc_corr_data <- perc_corr_data %>%
  group_by(unemployment_rate_category, accum_wealth_category, year) %>%
  mutate(
    median_perc_corr = median(perc_corr, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  mutate(
    perc_corr = ifelse(is.na(perc_corr), median_perc_corr, perc_corr)
  ) %>%
  select(-median_perc_corr)

# Step 4: Handle cases where one variable is missing
unemployment_rate_only_medians <- perc_corr_data %>%
  group_by(unemployment_rate_category, year) %>%
  summarise(median_unemployment_rate_only = median(perc_corr, na.rm = TRUE))

accum_wealth_only_medians <- perc_corr_data %>%
  group_by(accum_wealth_category, year) %>%
  summarise(median_accum_wealth_only = median(perc_corr, na.rm = TRUE))

perc_corr_data <- perc_corr_data %>%
  left_join(unemployment_rate_only_medians, by = c("unemployment_rate_category", "year")) %>%
  left_join(accum_wealth_only_medians, by = c("accum_wealth_category", "year")) %>%
  mutate(
    perc_corr = ifelse(
      is.na(perc_corr),
      ifelse(!is.na(median_unemployment_rate_only), median_unemployment_rate_only, median_accum_wealth_only),
      perc_corr
    )
  ) %>%
  select(-median_unemployment_rate_only, -median_accum_wealth_only)

# Step 5: Update perc_corr directly in finally_happy
finally_happy <- finally_happy %>%
  left_join(perc_corr_data %>% select(year, country, perc_corr), 
            by = c("year", "country")) %>%
  mutate(perc_corr = coalesce(perc_corr.y, perc_corr.x)) %>%
  select(-perc_corr.x, -perc_corr.y)


sum(is.na(finally_happy$perc_corr)) # Check


#####################################################
#### 12. ln_gpd_per_cap
#####################################################

# Correlation of ln_gpd_per_cap is high with working_poverty_rate and healthy_life_exp

# Step 1: Convert `ln_gpd_per_cap` back to its original scale
original_gdp_data <- finally_happy %>%
  mutate(gdp_per_cap = exp(ln_gpd_per_cap))  # Convert to original scale

# Step 2: Create a dataset for imputation
gdp_data <- original_gdp_data %>%
  dplyr::select(year, country, gdp_per_cap, working_poverty_rate, healthy_life_exp)

# Step 3: Assign Healthy Life Expectancy and Poverty Categories Independently
country_stats <- gdp_data %>%
  group_by(country) %>%
  summarise(
    mean_healthy_life_exp = mean(healthy_life_exp, na.rm = TRUE),
    mean_poverty_rate = mean(working_poverty_rate, na.rm = TRUE)
  ) %>%
  mutate(
    healthy_life_exp_category = ntile(mean_healthy_life_exp, 4),  # Quartiles for Healthy Life Exp
    poverty_category = ntile(mean_poverty_rate, 4)               # Quartiles for Poverty
  )

# Merge Healthy Life Expectancy and Poverty categories
gdp_data <- gdp_data %>%
  left_join(country_stats, by = "country")

# Step 4: Replace NAs using calculated medians
gdp_data <- gdp_data %>%
  group_by(healthy_life_exp_category, poverty_category, year) %>%
  mutate(
    median_gdp_per_cap = median(gdp_per_cap, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  mutate(
    gdp_per_cap = ifelse(is.na(gdp_per_cap), median_gdp_per_cap, gdp_per_cap)
  ) %>%
  dplyr::select(-median_gdp_per_cap)

# Step 5: Handle cases where one variable is missing
healthy_life_exp_only_medians <- gdp_data %>%
  group_by(healthy_life_exp_category, year) %>%
  summarise(median_healthy_life_exp_only = median(gdp_per_cap, na.rm = TRUE))

poverty_only_medians <- gdp_data %>%
  group_by(poverty_category, year) %>%
  summarise(median_poverty_only = median(gdp_per_cap, na.rm = TRUE))

gdp_data <- gdp_data %>%
  left_join(healthy_life_exp_only_medians, by = c("healthy_life_exp_category", "year")) %>%
  left_join(poverty_only_medians, by = c("poverty_category", "year")) %>%
  mutate(
    gdp_per_cap = ifelse(
      is.na(gdp_per_cap),
      ifelse(!is.na(median_healthy_life_exp_only), median_healthy_life_exp_only, median_poverty_only),
      gdp_per_cap
    )
  ) %>%
  dplyr::select(-median_healthy_life_exp_only, -median_poverty_only)

# Step 6: Convert back to log scale
gdp_data <- gdp_data %>%
  mutate(ln_gpd_per_cap = log(gdp_per_cap)) %>%  # Convert back to log scale
  dplyr::select(-gdp_per_cap)  # Remove the original scale column

# Step 7: Update `ln_gpd_per_cap` in the original dataset
finally_happy <- finally_happy %>%
  left_join(gdp_data %>% dplyr::select(year, country, ln_gpd_per_cap), 
            by = c("year", "country")) %>%
  mutate(ln_gpd_per_cap = coalesce(ln_gpd_per_cap.y, ln_gpd_per_cap.x)) %>%
  dplyr::select(-ln_gpd_per_cap.x, -ln_gpd_per_cap.y)

sum(is.na(finally_happy$ln_gpd_per_cap)) #check 

sum(is.na(finally_happy)) #check

############################################
## 13. Growth rate
############################################


finally_happy_abs <- finally_happy

# List of columns to exclude
exclude_cols <- c("EU", "OECD", "Africa", "Asia", "Europe", 
                  "North_America", "South_America", "Oceania", "unknown", "year")

# Convert absolute values to growth rates
finally_happy_growth <- finally_happy_abs %>%
  arrange(country, year) %>%  # Ensure data is sorted by country and year
  group_by(country) %>%  # Group by country
  mutate(across(
    where(is.numeric) & !any_of(exclude_cols),  # Exclude specific columns
    ~ ifelse(lag(.) == 0 | is.na(lag(.)), NA, (.-lag(.)) / lag(.) * 100),  # Handle growth rate formula
    .names = "growth_{col}"  # Prefix for new columns
  )) %>%
  ungroup()

# View the resulting dataset (optional)
print(head(finally_happy_growth))

View(finally_happy_growth) 

sum(is.na(finally_happy_abs))
sum(is.na(finally_happy_growth))

############################################
## 14. save data frames 
############################################
file_path <- "~/Library/Mobile Documents/com~apple~CloudDocs/Uni/Master BSE/Data Science /Happiness/finally_happy_abs.csv"
write.csv(finally_happy_abs, file = file_path, row.names = FALSE)

file_path <- "~/Library/Mobile Documents/com~apple~CloudDocs/Uni/Master BSE/Data Science /Happiness/finally_happy_growth.csv"
write.csv(finally_happy_growth, file = file_path, row.names = FALSE)

