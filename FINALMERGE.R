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


############################################
## 1. Create empty data frame with all countries from happiness data set (whp) and the years 2005-2023
############################################

#note: we exclude Congo ("Congo (Brazzaville)", "Congo (Kinshasa)") and Somaliland Region ("Somaliland region") because of ambiguity 
#and renamed some countries for clarification

#The whp does not include all countries, thus, e.g. Bahamas and North Korea are not included 

# Updated list of countries
countries <- c(
  "Afghanistan", "Albania", "Algeria", "Angola", "Argentina", "Armenia", "Australia", "Austria",
  "Azerbaijan", "Bahrain", "Bangladesh", "Belarus", "Belgium", "Belize", "Benin", "Bhutan",
  "Bolivia", "Bosnia and Herzegovina", "Botswana", "Brazil", "Bulgaria", "Burkina Faso", "Burundi",
  "Cambodia", "Cameroon", "Canada", "Central African Republic", "Chad", "Chile", "China", "Colombia",
  "Comoros", "Costa Rica", "Croatia", "Cuba", "Cyprus",
  "Czechia", "Denmark", "Djibouti", "Dominican Republic", "Ecuador", "Egypt", "El Salvador", "Estonia",
  "Eswatini", "Ethiopia", "Finland", "France", "Gabon", "Gambia", "Georgia", "Germany", "Ghana",
  "Greece", "Guatemala", "Guinea", "Guyana", "Haiti", "Honduras", "Hong Kong S.A.R. of China", "Hungary",
  "Iceland", "India", "Indonesia", "Iran", "Iraq", "Ireland", "Israel", "Italy", "Ivory Coast",
  "Jamaica", "Japan", "Jordan", "Kazakhstan", "Kenya", "Kosovo", "Kuwait", "Kyrgyzstan", "Laos",
  "Latvia", "Lebanon", "Lesotho", "Liberia", "Libya", "Lithuania", "Luxembourg", "Madagascar",
  "Malawi", "Malaysia", "Maldives", "Mali", "Malta", "Mauritania", "Mauritius", "Mexico", "Moldova",
  "Mongolia", "Montenegro", "Morocco", "Mozambique", "Myanmar", "Namibia", "Nepal", "Netherlands",
  "New Zealand", "Nicaragua", "Niger", "Nigeria", "North Macedonia", "Norway", "Oman", "Pakistan",
  "Panama", "Paraguay", "Peru", "Philippines", "Poland", "Portugal", "Qatar", "Romania", "Russia",
  "Rwanda", "Saudi Arabia", "Senegal", "Serbia", "Sierra Leone", "Singapore", "Slovakia", "Slovenia",
  "Somalia", "South Africa", "South Korea", "South Sudan", "Spain", "Sri Lanka",
  "State of Palestine", "Sudan", "Suriname", "Sweden", "Switzerland", "Syria", "Taiwan Province of China",
  "Tajikistan", "Tanzania", "Thailand", "Togo", "Trinidad and Tobago", "Tunisia", "Turkmenistan",
  "Turkey", "Uganda", "Ukraine", "United Arab Emirates", "United Kingdom", "United States", "Uruguay",
  "Uzbekistan", "Venezuela", "Vietnam", "Yemen", "Zambia", "Zimbabwe"
)

# Years from 2005 to 2023
years <- 2005:2023

# Create DataFrame with each country paired with each year
final_happy_1 <- expand.grid(country = countries, year = years) %>% arrange(country)

# View the DataFrame
head(final_happy_1)

############################################
## 2. merge with happiness data 
############################################

#load data set and filter for variables of interest 

whp_full <- read.csv("~/Library/Mobile Documents/com~apple~CloudDocs/Uni/Master BSE/Data Science /Happiness/Predictor for inequality data sets/World-happiness-report-updated_2024.csv")

whp <- whp_full %>%
  select(
    Country.name,
    year,
    Life.Ladder,
    Log.GDP.per.capita,
    Healthy.life.expectancy.at.birth,
    Generosity,
    Perceptions.of.corruption
  )

whp <- rename(whp, country=Country.name,
              happiness_score=Life.Ladder,
              ln_gpd_per_cap=Log.GDP.per.capita,
              healthy_life_exp=Healthy.life.expectancy.at.birth,
              generosity=Generosity,
              perc_corr=Perceptions.of.corruption)

#Rename turkey
whp <- whp %>% mutate(country = ifelse(country == "T\xfcrkiye", "Turkey", country))

#left join with final happy data frame
final_happy_2 <-  left_join(final_happy_1, whp, by = c("country", "year"))


############################################
## 3. merge with wealth inequality 
############################################


# wealth inequality dataset (top 10%) from the world inequlality database for years 2005 untill 2022

wealth_inequality <- read_excel("~/Library/Mobile Documents/com~apple~CloudDocs/Uni/Master BSE/Data Science /Happiness/Predictor for inequality data sets/wealth_inequality_top10.xlsx", col_names = FALSE)
colnames(wealth_inequality) <- c("country", "variable_code", "description", "year", "wealth_inequ_top10")

# Filter to keep only the `z_QT` data for South Africa
wealth_inequality <- wealth_inequality %>%
  filter(!(country == "South Africa" & str_detect(variable_code, "z_ZA")))
# Comment: We choose to use the `z_QT` data for South Africa as it may provide standardized estimates suitable for cross-country comparisons.

wi <- wealth_inequality %>%
  select(
    country,
    year,
    wealth_inequ_top10
  )

#match country names to final happy data: 
wi_clean <- wi %>%
  mutate(country = case_when(
    country == "The Bahamas" ~ "Bahamas",
    country == "Brunei Darussalam" ~ "Brunei",
    country == "Cape Verde" ~ "Cabo Verde",
    country == "Republic of the Congo" ~ "Congo (Brazzaville)",
    country == "Democratic Republic of the Congo" ~ "Congo (Kinshasa)",
    country == "Cote d’Ivoire" ~ "Ivory Coast",
    country == "Czech Republic" ~ "Czechia",
    country == "Guinea-Bissau" ~ "Guinea-Bissau",
    country == "Hong Kong S.A.R." ~ "Hong Kong S.A.R. of China",
    country == "South Korea" ~ "Korea, Republic of",
    country == "Lao PDR" ~ "Laos",
    country == "Korea" ~ "South Korea",                        
    country == "Macao" ~ "Macau",
    country == "Palestine" ~ "State of Palestine",
    country == "Hong Kong" ~ "Hong Kong S.A.R. of China",
    country == "Papua New Guinea" ~ "Papua New Guinea",
    country == "Russian Federation" ~ "Russia",
    country == "Sao Tome and Principe" ~ "São Tomé and Príncipe",
    country == "Syrian Arab Republic" ~ "Syria",
    country == "Swaziland" ~ "Eswatini",
    country == "Taiwan" ~ "Taiwan Province of China",
    country == "Timor-Leste" ~ "Timor Leste",
    country == "USA" ~ "United States",
    country == "Viet Nam" ~ "Vietnam",
    TRUE ~ country # Keep all other country names as they are
  ))


#merge: 
final_happy_3 <-  left_join(final_happy_2, wi_clean, by = c("country", "year"))


############################################
## 4. merge with unemployment rate, participation rate,
#### social protection plolicy rate, working poverty rate from ILO 
############################################

#load data 
unempl <- read.csv("~/Library/Mobile Documents/com~apple~CloudDocs/Uni/Master BSE/Data Science /Happiness/Predictor for inequality data sets/unemployment_rate_2005-2023_ILO.csv")
partic <- read.csv("~/Library/Mobile Documents/com~apple~CloudDocs/Uni/Master BSE/Data Science /Happiness/Predictor for inequality data sets/participation_rate_2005-2023_ILO.csv")
socialprotect <- read.csv("~/Library/Mobile Documents/com~apple~CloudDocs/Uni/Master BSE/Data Science /Happiness/Predictor for inequality data sets/SDG_0131_pop_covered_social_protect_2005-2023_ILO.csv")
workingpov <- read.csv("~/Library/Mobile Documents/com~apple~CloudDocs/Uni/Master BSE/Data Science /Happiness/Predictor for inequality data sets/SDG_0111_working_poverty_rate_2005-2023_ILO.csv")

#select columns and rename columns ti fit final happiness
unempl <- select(unempl, ref_area.label, time, obs_value)
colnames(unempl) <- c("country", "year", "unemploymemt_rate")

partic <- select(partic, ref_area.label, time, obs_value)
colnames(partic) <- c("country", "year", "participation_rate")

socialprotect <- select(socialprotect, ref_area.label, time, obs_value)
colnames(socialprotect) <- c("country", "year", "pop_covered")

workingpov <- select(workingpov, ref_area.label, time, obs_value)
colnames(workingpov) <- c("country", "year", "working_poverty_rate")


# Adjust country names in all data sets to match those final happiness 
unempl <- unempl %>%
  mutate(country = case_when(
    country == "Bolivia (Plurinational State of)" ~ "Bolivia",
    country == "Côte d'Ivoire" ~ "Ivory Coast",
    country == "Hong Kong, China" ~ "Hong Kong S.A.R. of China",
    country == "Iran (Islamic Republic of)" ~ "Iran",
    country == "Republic of Korea" ~ "South Korea",
    country == "Lao People's Democratic Republic" ~ "Laos",
    country == "Macao, China" ~ "Macau",
    country == "Russian Federation" ~ "Russia",
    country == "Syrian Arab Republic" ~ "Syria",
    country == "Venezuela (Bolivarian Republic of)" ~ "Venezuela",
    country == "United States of America" ~ "United States",
    country == "Occupied Palestinian Territory" ~ "State of Palestine",
    country == "Korea (the Democratic People's Republic of)" ~ "North Korea",
    country == "Türkiye" ~ "Turkey",
    country == "United Kingdom of Great Britain and Northern Ireland" ~ "United Kingdom", 
    country == "Viet Nam" ~ "Vietnam",
    country == "Taiwan, China" ~ "Taiwan Province of China",
    country == "Tanzania, United Republic of" ~ "Tanzania",
    country == "Republic of Moldova" ~ "Moldova", 
    TRUE ~ country
  ))

partic <- partic %>%
  mutate(country = case_when(
    country == "Bolivia (Plurinational State of)" ~ "Bolivia",
    country == "Côte d'Ivoire" ~ "Ivory Coast",
    country == "Hong Kong, China" ~ "Hong Kong S.A.R. of China",
    country == "Iran (Islamic Republic of)" ~ "Iran",
    country == "Republic of Korea" ~ "South Korea",
    country == "Lao People's Democratic Republic" ~ "Laos",
    country == "Macao, China" ~ "Macau",
    country == "Russian Federation" ~ "Russia",
    country == "Syrian Arab Republic" ~ "Syria",
    country == "Venezuela (Bolivarian Republic of)" ~ "Venezuela",
    country == "United States of America" ~ "United States",
    country == "Occupied Palestinian Territory" ~ "State of Palestine",
    country == "Korea (the Democratic People's Republic of)" ~ "North Korea",
    country == "Türkiye" ~ "Turkey",
    country == "United Kingdom of Great Britain and Northern Ireland" ~ "United Kingdom", 
    country == "Viet Nam" ~ "Vietnam",
    country == "Taiwan, China" ~ "Taiwan Province of China",
    country == "Tanzania, United Republic of" ~ "Tanzania",
    country == "Republic of Moldova" ~ "Moldova",
    TRUE ~ country
  ))

socialprotect <- socialprotect %>%
  mutate(country = case_when(
    country == "Bolivia (Plurinational State of)" ~ "Bolivia",
    country == "Côte d'Ivoire" ~ "Ivory Coast",
    country == "Hong Kong, China" ~ "Hong Kong S.A.R. of China",
    country == "Iran (Islamic Republic of)" ~ "Iran",
    country == "Republic of Korea" ~ "South Korea",
    country == "Lao People's Democratic Republic" ~ "Laos",
    country == "Macao, China" ~ "Macau",
    country == "Russian Federation" ~ "Russia",
    country == "Syrian Arab Republic" ~ "Syria",
    country == "Venezuela (Bolivarian Republic of)" ~ "Venezuela",
    country == "United States of America" ~ "United States",
    country == "Occupied Palestinian Territory" ~ "State of Palestine",
    country == "Korea (the Democratic People's Republic of)" ~ "North Korea",
    country == "Türkiye" ~ "Turkey",
    country == "United Kingdom of Great Britain and Northern Ireland" ~ "United Kingdom", 
    country == "Viet Nam" ~ "Vietnam",
    country == "Taiwan, China" ~ "Taiwan Province of China",
    country == "Tanzania, United Republic of" ~ "Tanzania",
    country == "Republic of Moldova" ~ "Moldova",
    TRUE ~ country
  ))

workingpov <- workingpov %>%
  mutate(country = case_when(
    country == "Bolivia (Plurinational State of)" ~ "Bolivia",
    country == "Côte d'Ivoire" ~ "Ivory Coast",
    country == "Hong Kong, China" ~ "Hong Kong S.A.R. of China",
    country == "Iran (Islamic Republic of)" ~ "Iran",
    country == "Republic of Korea" ~ "South Korea",
    country == "Lao People's Democratic Republic" ~ "Laos",
    country == "Macao, China" ~ "Macau",
    country == "Russian Federation" ~ "Russia",
    country == "Syrian Arab Republic" ~ "Syria",
    country == "Venezuela (Bolivarian Republic of)" ~ "Venezuela",
    country == "United States of America" ~ "United States",
    country == "Occupied Palestinian Territory" ~ "State of Palestine",
    country == "Korea (the Democratic People's Republic of)" ~ "North Korea",
    country == "Türkiye" ~ "Turkey",
    country == "United Kingdom of Great Britain and Northern Ireland" ~ "United Kingdom", 
    country == "Viet Nam" ~ "Vietnam",
    country == "Taiwan, China" ~ "Taiwan Province of China",
    country == "Tanzania, United Republic of" ~ "Tanzania",
    country == "Republic of Moldova" ~ "Moldova",
    TRUE ~ country
  ))

#merge: 
final_happy_4 <-  left_join(final_happy_3, unempl, by = c("country", "year"))
final_happy_4 <-  left_join(final_happy_4, partic, by = c("country", "year"))
final_happy_4 <-  left_join(final_happy_4, socialprotect, by = c("country", "year"))
final_happy_4 <-  left_join(final_happy_4, workingpov, by = c("country", "year"))



############################################
## 5. merge average years of schooling  
############################################
#load data 
schooling <- read.csv("~/Library/Mobile Documents/com~apple~CloudDocs/Uni/Master BSE/Data Science /Happiness/Predictor for inequality data sets/mean_years_of_schooling_2005-2023_UNESCO.csv")

##################################### merge schooling
# select the important stuff and rename
schooling_filtered <- schooling %>%
  filter(str_detect(Indicator, "both sexes")) %>%  # Keep only rows with 'both sexes' in the Indicator
  select(Country, Time, Value) %>%
  rename(country=Country, year=Time, mean_year_edu = Value)

##### check country mismatches
school_countries <- unique(schooling_filtered$Country)
master_countries <- unique(final_happy_4$country_name)
# Find countries in wi_clean that are not in whp
mismatched_countries <- setdiff(school_countries, master_countries)
# Display the mismatched countries
mismatched_countries
mismatched_countries2 <- setdiff(master_countries, school_countries)
mismatched_countries2

###### rename mismatches
schooling_filteredi <- schooling_filtered %>%
  mutate(country = case_when(
    country == "Venezuela (Bolivarian Republic of)" ~ "Venezuela",
    country == "Syrian Arab Republic" ~ "Syria",
    country == "United Republic of Tanzania" ~ "Tanzania",
    country == "Palestine" ~ "State of Palestine",
    country == "United Kingdom of Great Britain and Northern Ireland" ~ "United Kingdom",
    country == "Democratic Republic of the Congo" ~ "Congo (Kinshasa)",
    country == "Russian Federation" ~ "Russia",
    country == "China, Macao Special Administrative Region" ~ "Hong Kong S.A.R. of China",
    country == "Republic of Korea" ~ "South Korea",
    country == "United States of America" ~ "United States",
    country == "Côte d'Ivoire" ~ "Ivory Coast",
    country == "Bolivia (Plurinational State of)" ~ "Bolivia",
    country == "Türkiye" ~ "Turkey",
    country == "Republic of Moldova" ~ "Moldova",
    country == "China, Hong Kong Special Administrative Region" ~ "Hong Kong S.A.R. of China",
    country == "Iran (Islamic Republic of)" ~ "Iran",
    country == "Viet Nam" ~ "Vietnam",
    country == "Lao People's Democratic Republic" ~ "Laos",
    country == "Sao Tome and Principe" ~ "São Tomé and Príncipe",
    TRUE ~ country # Leave other names unchanged
  ))


# merging final happiness with schooling
final_happy_5 <- left_join(final_happy_4, schooling_filteredi, by = c("country", "year"))

############################################
## 6. merge inflation rate 
############################################

#load data 
inflation <- read_excel("~/Library/Mobile Documents/com~apple~CloudDocs/Uni/Master BSE/Data Science /Happiness/Predictor for inequality data sets/Inflation rate (% change) - IMF.xls")

# Step 0: Remove the first row of all NAs
inflation <- inflation %>%
  filter(rowSums(is.na(.)) != ncol(.))

# Step 1: Pivot from wide to long format
inflation_long <- inflation %>%
  pivot_longer(
    cols = -`Inflation rate, average consumer prices (Annual percent change)`,  # All columns except the country column
    names_to = "year",    # New column for year
    values_to = "inflation_rate"  # New column for values (inflation rate)
  )

# Step 2: Rename the country column and format year as integer if necessary
inflation_long <- inflation_long %>%
  rename(
    country = `Inflation rate, average consumer prices (Annual percent change)`
  ) %>%
  mutate(
    year = as.integer(year),                  # Convert year to integer
    inflation_rate = as.numeric(gsub("no data", NA, inflation_rate)) # Replace "no data" with NA and convert to numeric
  ) %>%
# Step 3: Filter for years 2005 to 2023
  filter(year >= 2005 & year <= 2023)

# rename countries
inflation_longi <- inflation_long %>%
  mutate(country = case_when(
    country == "China, People's Republic of" ~ "China",
    country == "Czech Republic" ~ "Czechia",
    country == "Gambia, The" ~ "Gambia",
    country == "Hong Kong SAR" ~ "Hong Kong S.A.R. of China",
    country == "Côte d'Ivoire" ~ "Ivory Coast",
    country == "Kyrgyz Republic" ~ "Kyrgyzstan",
    country == "Lao P.D.R." ~ "Laos",
    country == "Russian Federation" ~ "Russia",
    country == "Slovak Republic" ~ "Slovakia",
    country == "Türkiye, Republic of" ~ "Turkey",
    country == "Korea, Republic of" ~ "South Korea",
    country == "South Sudan, Republic of" ~ "South Sudan",
    country == "West Bank and Gaza" ~ "State of Palestine",               
    TRUE ~ country # Keep all other country names as they are
  ))

#merge: 
final_happy_6 <- left_join(final_happy_5, inflation_longi, by = c("country", "year"))

############################################
## 7. merge accumulated wealth & 10% share income inequality  
############################################
#load data 
accum_and_income <- read_excel("~/Library/Mobile Documents/com~apple~CloudDocs/Uni/Master BSE/Data Science /Happiness/Predictor for inequality data sets/WID_accumwealth_top10income.xlsx", col_names = FALSE)

colnames(accum_and_income) <- c("country", "variable_code", "description", "year", "value")

# Filter to keep only the `QT` data for South Africa
accum_and_income_1 <- accum_and_income %>%  filter(!(country == "South Africa" & str_detect(variable_code, "z_ZA|i_ZA")))

# Reshape the data to have one row per country-year, keeping both pall and p90p100 values
accum_and_income_2 <- accum_and_income_1 %>%
  select(country, year, description, value) %>%  # Select only relevant columns for merging
  pivot_wider(names_from = description, values_from = value, values_fill = NA)  # Pivot on description

#rename countries to match final happiness 
accum_and_income_2 <- accum_and_income_2 %>%
  mutate(country = case_when(
    country == "The Bahamas" ~ "Bahamas",
    country == "Brunei Darussalam" ~ "Brunei",
    country == "Cape Verde" ~ "Cabo Verde",
    country == "Republic of the Congo" ~ "Congo (Brazzaville)",
    country == "Democratic Republic of the Congo" ~ "Congo (Kinshasa)",            
    country == "Cote d’Ivoire" ~ "Ivory Coast",
    country == "Czech Republic" ~ "Czechia",
    country == "Guinea-Bissau" ~ "Guinea-Bissau",
    country == "Hong Kong S.A.R." ~ "Hong Kong S.A.R. of China",
    country == "South Korea" ~ "Korea, Republic of",
    country == "Lao PDR" ~ "Laos",
    country == "Korea" ~ "South Korea",                       
    country == "Macao" ~ "Macau",
    country == "Palestine" ~ "State of Palestine",
    country == "Hong Kong" ~ "Hong Kong S.A.R. of China",
    country == "Papua New Guinea" ~ "Papua New Guinea",
    country == "Russian Federation" ~ "Russia",
    country == "Sao Tome and Principe" ~ "São Tomé and Príncipe",
    country == "Syrian Arab Republic" ~ "Syria",
    country == "Swaziland" ~ "Eswatini",
    country == "Taiwan" ~ "Taiwan Province of China",
    country == "Timor-Leste" ~ "Timor Leste",
    country == "USA" ~ "United States",
    country == "Viet Nam" ~ "Vietnam",
    TRUE ~ country # Keep all other country names as they are
  ))


#  merge:
final_happy_7 <- left_join(final_happy_6, accum_and_income_2, by = c("country", "year"))

final_happy_7 <- final_happy_7 %>%
  rename(
    accum_wealth = pall,
    income_inequ_top10 = p90p100
  )

# Reorder columns to make income_inequ_top10 the 3rd column
final_happy_7 <- final_happy_7 %>%
  select(country, year, income_inequ_top10, everything())


############################################
## 8. Gini, interest rate, savings rate IRACHE 
############################################

# Load the data
savings <- read_excel("~/Library/Mobile Documents/com~apple~CloudDocs/Uni/Master BSE/Data Science /Happiness/Predictor for inequality data sets/Gross savings_world_bank.xlsx")
interest <- read_excel("~/Library/Mobile Documents/com~apple~CloudDocs/Uni/Master BSE/Data Science /Happiness/Predictor for inequality data sets/Real interest rate_percentage.xlsx")
gini_index <- read_excel("~/Library/Mobile Documents/com~apple~CloudDocs/Uni/Master BSE/Data Science /Happiness/Predictor for inequality data sets/Gini index - World Bank.xls" , skip=2)

##########################
# Clean interest
##########################

reshaped_interest <- interest %>%
  pivot_longer(cols = c(`2005 [YR2005]`, `2006 [YR2006]`, `2007 [YR2007]`, `2008 [YR2008]`, `2009 [YR2009]`,
                        `2010 [YR2010]`, `2011 [YR2011]`, `2012 [YR2012]`, `2013 [YR2013]`, `2014 [YR2014]`,
                        `2015 [YR2015]`, `2016 [YR2016]`, `2017 [YR2017]`, `2018 [YR2018]`, `2019 [YR2019]`,
                        `2020 [YR2020]`, `2021 [YR2021]`, `2022 [YR2022]`, `2023 [YR2023]`),
               names_to = "Year",
               values_to = "Interest Rate") %>%
  mutate(Year = str_extract(Year, "[0-9]{4}"))


# Delete unnecesary variables and rename
reshaped_interest <- reshaped_interest %>%
  select(-`Series Name`) %>%
  select(-`Series Code`) %>%
  select(-`Country Code`)%>%
  rename(country = `Country Name`) %>%
  rename(interest_rate = `Interest Rate`) %>%
  rename(year = `Year`)

# Replace .. as NA
reshaped_interest <- reshaped_interest %>%
  mutate_all(~ na_if(., ".."))

reshaped_interest$year <- as.integer(reshaped_interest$year)

##########################
# Clean savings
##########################
reshaped_savings <- savings %>%
  pivot_longer(cols = c(`2005 [YR2005]`, `2006 [YR2006]`, `2007 [YR2007]`, `2008 [YR2008]`, `2009 [YR2009]`,
                        `2010 [YR2010]`, `2011 [YR2011]`, `2012 [YR2012]`, `2013 [YR2013]`, `2014 [YR2014]`,
                        `2015 [YR2015]`, `2016 [YR2016]`, `2017 [YR2017]`, `2018 [YR2018]`, `2019 [YR2019]`,
                        `2020 [YR2020]`, `2021 [YR2021]`, `2022 [YR2022]`, `2023 [YR2023]`),
               names_to = "Year",
               values_to = "Savings") %>%
  mutate(Year = str_extract(Year, "[0-9]{4}"))

# Delete unnecesary variables
reshaped_savings <- reshaped_savings %>%
  select(-`Series Name`) %>%
  select(-`Series Code`) %>%
  select(-`Country Code`)%>%
  rename(country = `Country Name`) %>%
  rename(savings = `Savings`) %>%
  rename(year = `Year`)

# Replace .. as NA
reshaped_savings <- reshaped_savings %>%
  mutate_all(~ na_if(., ".."))

reshaped_savings$year <- as.integer(reshaped_savings$year)

##########################
# Clean GINI index
##########################
gini_reshaped <- gini_index %>%
  pivot_longer(
    cols = `1960`:`2023`,     # Specify the range of years
    names_to = "Year",
    values_to = "Gini Index"
  )

# Delete unnecesary variables
gini_reshaped <- gini_reshaped %>%
  select(-`Indicator Name`) %>%
  select(-`Country Code`) %>%
  select(-`Indicator Code`) %>%
  rename(country = `Country Name`) %>%
  rename(gini_index = `Gini Index`) %>%
  rename(year = `Year`)

gini_reshaped$year <- as.integer(gini_reshaped$year)

##########################
# Remove duplicates (checked that it is only observations with NAs) & correct country names 
##########################
reshaped_savings <- reshaped_savings %>%
  distinct(country, year, .keep_all = TRUE)

reshaped_interest <- reshaped_interest %>%
  distinct(country, year, .keep_all = TRUE)


#rename countries to match final happiness 
reshaped_savings <- reshaped_savings %>%
  mutate(country = case_when(
    country == "Bahamas, The" ~ "Bahamas",
    country == "Congo, Dem. Rep." ~ "Congo (Kinshasa)",
    country == "Congo, Rep." ~ "Congo (Brazzaville)",
    country == "Cote d'Ivoire" ~ "Ivory Coast",
    country == "Egypt, Arab Rep." ~ "Egypt",
    country == "Gambia, The" ~ "Gambia",
    country == "Hong Kong SAR, China" ~ "Hong Kong S.A.R. of China",
    country == "Iran, Islamic Rep." ~ "Iran",
    country == "Korea, Dem. People's Rep." ~ "North Korea",
    country == "Korea, Rep." ~ "South Korea",
    country == "Kyrgyz Republic" ~ "Kyrgyzstan",
    country == "Lao PDR" ~ "Laos",
    country == "Macao SAR, China" ~ "Macau",
    country == "Micronesia, Fed. Sts." ~ "Micronesia",
    country == "Russian Federation" ~ "Russia",
    country == "Slovak Republic" ~ "Slovakia",
    country == "Syrian Arab Republic" ~ "Syria",
    country == "Turkiye" ~ "Turkey",
    country == "Venezuela, RB" ~ "Venezuela",
    country == "West Bank and Gaza" ~ "State of Palestine",
    country == "Yemen, Rep." ~ "Yemen",
    TRUE ~ country # Keep all other country names as they are
  ))

reshaped_interest <- reshaped_interest %>%
  mutate(country = case_when(
    country == "Bahamas, The" ~ "Bahamas",
    country == "Congo, Dem. Rep." ~ "Congo (Kinshasa)",
    country == "Congo, Rep." ~ "Congo (Brazzaville)",
    country == "Cote d'Ivoire" ~ "Ivory Coast",
    country == "Egypt, Arab Rep." ~ "Egypt",
    country == "Gambia, The" ~ "Gambia",
    country == "Hong Kong SAR, China" ~ "Hong Kong S.A.R. of China",
    country == "Iran, Islamic Rep." ~ "Iran",
    country == "Korea, Dem. People's Rep." ~ "North Korea",
    country == "Korea, Rep." ~ "South Korea",
    country == "Kyrgyz Republic" ~ "Kyrgyzstan",
    country == "Lao PDR" ~ "Laos",
    country == "Macao SAR, China" ~ "Macau",
    country == "Micronesia, Fed. Sts." ~ "Micronesia",
    country == "Russian Federation" ~ "Russia",
    country == "Slovak Republic" ~ "Slovakia",
    country == "Syrian Arab Republic" ~ "Syria",
    country == "Turkiye" ~ "Turkey",
    country == "Venezuela, RB" ~ "Venezuela",
    country == "Vietnam" ~ "Vietnam",
    country == "West Bank and Gaza" ~ "State of Palestine",
    country == "Yemen, Rep." ~ "Yemen",
    TRUE ~ country # Keep all other country names as they are
  ))

gini_reshaped <- gini_reshaped %>%
  mutate(country = case_when(
    country == "Afganistán" ~ "Afghanistan",
    country == "El mundo árabe" ~ "Arab World",
    country == "Emiratos Árabes Unidos" ~ "United Arab Emirates",
    country == "Samoa Americana" ~ "American Samoa",
    country == "Antigua y Barbuda" ~ "Antigua and Barbuda",
    country == "Azerbaiyán" ~ "Azerbaijan",
    country == "Bélgica" ~ "Belgium",
    country == "Bahrein" ~ "Bahrain",
    country == "Bosnia y Herzegovina" ~ "Bosnia and Herzegovina",
    country == "Belarús" ~ "Belarus",
    country == "Belice" ~ "Belize",
    country == "Bermudas" ~ "Bermuda",
    country == "Brasil" ~ "Brazil",
    country == "Bhután" ~ "Bhutan",
    country == "República Centroafricana" ~ "Central African Republic",
    country == "Canadá" ~ "Canada",
    country == "Islas del Canal" ~ "Channel Islands",
    country == "Camerún" ~ "Cameroon",
    country == "Congo, República Democrática del" ~ "Congo (Kinshasa)",
    country == "Congo, República del" ~ "Congo (Brazzaville)",
    country == "Cabo Verde" ~ "Cape Verde",
    country == "Islas Caimán" ~ "Cayman Islands",
    country == "República Checa" ~ "Czechia",
    country == "Alemania" ~ "Germany",
    country == "Dominica" ~ "Dominica",
    country == "Dinamarca" ~ "Denmark",
    country == "Argelia" ~ "Algeria",
    country == "Ecuador" ~ "Ecuador",
    country == "Egipto, República Árabe de" ~ "Egypt",
    country == "España" ~ "Spain",
    country == "Estonia" ~ "Estonia",
    country == "Etiopía" ~ "Ethiopia",
    country == "Finlandia" ~ "Finland",
    country == "Francia" ~ "France",
    country == "Islas Feroe" ~ "Faroe Islands",
    country == "Gabón" ~ "Gabon",
    country == "Reino Unido" ~ "United Kingdom",
    country == "Georgia" ~ "Georgia",
    country == "Ghana" ~ "Ghana",
    country == "Grecia" ~ "Greece",
    country == "Granada" ~ "Grenada",
    country == "Guatemala" ~ "Guatemala",
    country == "Guinea" ~ "Guinea",
    country == "Guinea-Bissau" ~ "Guinea-Bissau",
    country == "Guinea Ecuatorial" ~ "Equatorial Guinea",
    country == "Hong Kong, Región Administrativa Especial" ~ "Hong Kong S.A.R. of China",
    country == "Honduras" ~ "Honduras",
    country == "Hungría" ~ "Hungary",
    country == "India" ~ "India",
    country == "Indonesia" ~ "Indonesia",
    country == "Irán, República Islámica del" ~ "Iran",
    country == "Irlanda" ~ "Ireland",
    country == "Islandia" ~ "Iceland",
    country == "Italia" ~ "Italy",
    country == "Jamaica" ~ "Jamaica",
    country == "Jordania" ~ "Jordan",
    country == "Japón" ~ "Japan",
    country == "Kazajstán" ~ "Kazakhstan",
    country == "Kenya" ~ "Kenya",
    country == "Kirguistán" ~ "Kyrgyzstan",
    country == "Camboya" ~ "Cambodia",
    country == "Kuwait" ~ "Kuwait",
    country == "Líbano" ~ "Lebanon",
    country == "Liberia" ~ "Liberia",
    country == "Libia" ~ "Libya",
    country == "Santa Lucía" ~ "Saint Lucia",
    country == "Sri Lanka" ~ "Sri Lanka",
    country == "Lesotho" ~ "Lesotho",
    country == "Lituania" ~ "Lithuania",
    country == "Luxemburgo" ~ "Luxembourg",
    country == "Letonia" ~ "Latvia",
    country == "Mónaco" ~ "Monaco",
    country == "Marruecos" ~ "Morocco",
    country == "República de Moldova" ~ "Moldova",
    country == "Madagascar" ~ "Madagascar",
    country == "Maldivas" ~ "Maldives",
    country == "México" ~ "Mexico",
    country == "Islas Marshall" ~ "Marshall Islands",
    country == "Malí" ~ "Mali",
    country == "Malta" ~ "Malta",
    country == "Myanmar" ~ "Myanmar",
    country == "Mongolia" ~ "Mongolia",
    country == "Mozambique" ~ "Mozambique",
    country == "Mauritania" ~ "Mauritania",
    country == "Mauricio" ~ "Mauritius",
    country == "Malawi" ~ "Malawi",
    country == "Malasia" ~ "Malaysia",
    country == "Namibia" ~ "Namibia",
    country == "Níger" ~ "Niger",
    country == "Nigeria" ~ "Nigeria",
    country == "Noruega" ~ "Norway",
    country == "Nepal" ~ "Nepal",
    country == "Nueva Zelandia" ~ "New Zealand",
    country == "Omán" ~ "Oman",
    country == "Pakistán" ~ "Pakistan",
    country == "Panamá" ~ "Panama",
    country == "Perú" ~ "Peru",
    country == "Filipinas" ~ "Philippines",
    country == "Polonia" ~ "Poland",
    country == "Portugal" ~ "Portugal",
    country == "Paraguay" ~ "Paraguay",
    country == "Qatar" ~ "Qatar",
    country == "Rumania" ~ "Romania",
    country == "Federación de Rusia" ~ "Russia",
    country == "Rwanda" ~ "Rwanda",
    country == "Arabia Saudita" ~ "Saudi Arabia",
    country == "Sudán" ~ "Sudan",
    country == "Senegal" ~ "Senegal",
    country == "Singapur" ~ "Singapore",
    country == "Eslovenia" ~ "Slovenia",
    country == "Suecia" ~ "Sweden",
    country == "Eswatini" ~ "Eswatini",
    country == "República Árabe Siria" ~ "Syria",
    country == "Tailandia" ~ "Thailand",
    country == "Tayikistán" ~ "Tajikistan",
    country == "Togo" ~ "Togo",
    country == "Turkmenistán" ~ "Turkmenistan",
    country == "Trinidad y Tobago" ~ "Trinidad and Tobago",
    country == "Túnez" ~ "Tunisia",
    country == "Turquía" ~ "Turkey",
    country == "Tanzanía" ~ "Tanzania",
    country == "Uganda" ~ "Uganda",
    country == "Ucrania" ~ "Ukraine",
    country == "Uruguay" ~ "Uruguay",
    country == "Estados Unidos" ~ "United States",
    country == "Uzbekistán" ~ "Uzbekistan",
    country == "San Vicente y las Granadinas" ~ "Saint Vincent and the Grenadines",
    country == "Venezuela" ~ "Venezuela",
    country == "Viet Nam" ~ "Vietnam",
    country == "Yemen, Rep. del" ~ "Yemen",
    country == "Sudáfrica" ~ "South Africa",
    TRUE ~ country # Keep all other country names as they are
  ))


#merge: 
final_happy_8 <- left_join(final_happy_7, reshaped_savings, by = c("country", "year"))
final_happy_8 <- left_join(final_happy_8, reshaped_interest, by = c("country", "year"))
final_happy_8 <- left_join(final_happy_8, gini_reshaped, by = c("country", "year"))

############################################
## 9. NA overview
############################################

finally_happy <- final_happy_8

# Calculate the count and percentage of NA values for each column
na_summary <- sapply(finally_happy, function(x) sum(is.na(x)))
na_percentage <- sapply(finally_happy, function(x) mean(is.na(x)) * 100)

# Create a data frame with both count and percentage of NA values
na_summary_df <- data.frame(
  NA_Count = na_summary,
  NA_Percentage = na_percentage
)
na_summary_df

#alternative: drop all values where happiness score is NA: 
# Drop rows with NA in the "happiness_score" column
finally_happy_drop <- finally_happy %>% filter(!is.na(happiness_score))

# Calculate the count and percentage of NA values for each column
na_summary <- sapply(finally_happy_drop, function(x) sum(is.na(x)))
na_percentage <- sapply(finally_happy_drop, function(x) mean(is.na(x)) * 100)

# Create a data frame with both count and percentage of NA values
na_summary_df_drop <- data.frame(
  NA_Count = na_summary,
  NA_Percentage = na_percentage
)
na_summary_df_drop


############################################
## 10. add dummies (EU area, Europe, continents, OECD ,....) 
############################################


#we used this wikipedia article for our classification: https://simple.wikipedia.org/wiki/List_of_countries_by_continents

#In the following, we assign contries to subgroups to create dummy variables: 
EU_countries <- c("Austria", "Belgium", "Bulgaria", "Croatia", "Cyprus", "Czechia", "Denmark", 
                  "Estonia", "Finland", "France", "Germany", "Greece", "Hungary", "Ireland", 
                  "Italy", "Latvia", "Lithuania", "Luxembourg", "Malta", "Netherlands", 
                  "Poland", "Portugal", "Romania", "Slovakia", "Slovenia", "Spain", "Sweden")

OECD_countries <- c("Australia", "Austria", "Belgium", "Canada", "Chile", "Colombia", "Czechia", 
                    "Denmark", "Estonia", "Finland", "France", "Germany", "Greece", "Hungary", 
                    "Iceland", "Ireland", "Israel", "Italy", "Japan", "South Korea", "Latvia", 
                    "Lithuania", "Luxembourg", "Mexico", "Netherlands", "New Zealand", 
                    "Norway", "Poland", "Portugal", "Slovakia", "Slovenia", "Spain", "Sweden", 
                    "Switzerland", "Turkey", "United Kingdom", "United States")

Africa <- c("Algeria", "Angola", "Benin", "Botswana", "Burkina Faso", "Burundi", "Cameroon", 
            "Central African Republic", "Chad", "Comoros",  
            "Djibouti", "Egypt", "Equatorial Guinea", "Eritrea", "Eswatini", "Ethiopia", "Gabon", "Gambia", 
            "Ghana", "Guinea", "Guinea-Bissau", "Ivory Coast", "Kenya", "Lesotho", "Liberia", "Libya", 
            "Madagascar", "Malawi", "Mali", "Mauritania", "Mauritius", "Morocco", "Mozambique", "Namibia", 
            "Niger", "Nigeria", "Rwanda", "Senegal", "Seychelles", "Sierra Leone", "Somalia", "South Africa", 
            "South Sudan", "Sudan", "Tanzania", "Togo", "Tunisia", "Uganda", "Zambia", "Zimbabwe")

Asia <- c("Afghanistan", "Armenia", "Azerbaijan", "Bahrain", "Bangladesh", "Bhutan", "Brunei", 
          "Cambodia", "China", "Cyprus", "Georgia", "Hong Kong S.A.R. of China", "India", "Indonesia", "Iran", "Iraq", 
          "Israel", "Japan", "Jordan", "Kazakhstan", "Kuwait", "Kyrgyzstan", "Laos", "Lebanon", 
          "Malaysia", "Maldives", "Mongolia", "Myanmar", "Nepal", "North Korea", "Oman", 
          "Pakistan", "State of Palestine", "Philippines", "Qatar", "Saudi Arabia", "Singapore", 
          "South Korea", "Sri Lanka", "Syria", "Taiwan Province of China", "Tajikistan", "Thailand", 
          "Timor-Leste", "Turkey", "Turkmenistan", "United Arab Emirates", "Uzbekistan", "Vietnam", "Yemen")

Europe <- c("Albania", "Andorra", "Austria", "Belarus", "Belgium", "Bosnia and Herzegovina", 
            "Bulgaria", "Croatia", "Cyprus", "Czechia", "Denmark", "Estonia", "Finland", 
            "France", "Germany", "Greece", "Hungary", "Iceland", "Ireland", "Italy", "Kosovo", 
            "Latvia", "Lithuania", "Luxembourg", "Malta", "Moldova", "Montenegro", "Netherlands", 
            "North Macedonia", "Norway", "Poland", "Portugal", "Romania", "Russia", 
            "Serbia", "Slovakia", "Slovenia", "Spain", "Sweden", "Switzerland", "Ukraine", 
            "United Kingdom")

North_America <- c("Belize", "Canada", "Costa Rica", "Cuba", "Dominican Republic", "El Salvador", 
                   "Guatemala", "Haiti", "Honduras", "Jamaica", "Mexico", "Nicaragua", "Panama", 
                   "Trinidad and Tobago", "United States")

South_America <- c("Argentina", "Bolivia", "Brazil", "Chile", "Colombia", "Ecuador", "Guyana", 
                   "Paraguay", "Peru", "Suriname", "Uruguay", "Venezuela")

Oceania <- c("Australia", "Fiji", "Kiribati", "Marshall Islands", "Micronesia", "Nauru", 
             "New Zealand", "Palau", "Papua New Guinea", "Samoa", "Solomon Islands", 
             "Tonga", "Tuvalu", "Vanuatu")

# Add dummy columns to the dataset
finally_happy$EU <- as.integer(finally_happy$country %in% EU_countries)
finally_happy$OECD <- as.integer(finally_happy$country %in% OECD_countries)
finally_happy$Africa <- as.integer(finally_happy$country %in% Africa)
finally_happy$Asia <- as.integer(finally_happy$country %in% Asia)
finally_happy$Europe <- as.integer(finally_happy$country %in% Europe)
finally_happy$North_America <- as.integer(finally_happy$country %in% North_America)
finally_happy$South_America <- as.integer(finally_happy$country %in% South_America)
finally_happy$Oceania <- as.integer(finally_happy$country %in% Oceania)
                   

#Sanity check: if all countries have at least one dummy then we get 0
no_dummy_countries <- finally_happy[rowSums(finally_happy[, c("EU", "OECD", "Africa", "Asia", 
                                                              "Europe", "North_America", 
                                                              "South_America", "Oceania")]) == 0, ]

no_dummy_countries


############################################
## Final save
############################################

# Rename specified country names in the 'country' column
finally_happy$country[finally_happy$country == "State of Palestine"] <- "Palestine"
finally_happy$country[finally_happy$country == "Hong Kong S.A.R. of China"] <- "Hong Kong"
finally_happy$country[finally_happy$country == "Taiwan Province of China"] <- "Taiwan"


write.csv(finally_happy, "finally_happy.csv", row.names = FALSE)
