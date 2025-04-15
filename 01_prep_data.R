# TITLE: 01_prep_data.R

#_____________________________________________________________________________________________________________
# SET UP
#_____________________________________________________________________________________________________________
# Clear environment
rm(list = ls())

# Load packages
library(tidyverse)
library(countrycode)

# Path to data directory
datadir <- "/home/shares/food-systems/social_justice_projects/food_trade_pressures/output_data"
covar_datadir <- "/home/shares/food-systems/social_justice_projects/food_trade_ej/data"

# Flow threshold
# Very small flows are likely unrealistic and a result of the underlying model. Therefore, we remove them
flow_threshold <- 0

#_____________________________________________________________________________________________________________
# LOAD RAW DATA
#_____________________________________________________________________________________________________________
# Load trade data with pressures 
stressor_trade <- read.csv("/home/shares/food-systems/social_justice_projects/food_trade_pressures/output_data/04c_adjust_trade_matrix_pressures_for_feed/trade_matrix_w_pressures_accounting_for_feed.csv")
# Load formatted gravity covariates
gravity_covars <- read.csv(file.path(covar_datadir, "gravity_covars_03_27_24.csv")) 

#_____________________________________________________________________________________________________________
# CUSTOM FUNCTIONS
#_____________________________________________________________________________________________________________
standardize_countries <- function(data, col_iso3, col_country_name) {
  
  cleaned_data <- data %>%
    mutate(clean_iso3c = "",
           clean_country_name = "") %>%
    mutate(clean_iso3c = case_when(
      # United States' territories----------------------------------------------
      .data[[col_iso3]] == "ASM" ~ "USA", # American Samoa
      .data[[col_iso3]] == "GUM" ~ "USA", # Guam
      .data[[col_iso3]] == "MNP" ~ "USA", # New Marianas Islands
      .data[[col_iso3]] == "PRI" ~ "USA", # Puerto Rico
      .data[[col_iso3]] == "VIR" ~ "USA", # Virgin Islands
      # Great Britain's territories---------------------------------------------
      .data[[col_iso3]] == "AIA" ~ "GBR", # Anguilla
      .data[[col_iso3]] == "BMU" ~ "GBR", # Bermuda
      .data[[col_iso3]] == "IOT" ~ "GBR", # British Indian Ocean Territory
      .data[[col_iso3]] == "VGB" ~ "GBR", # British Virgin Islands
      .data[[col_iso3]] == "CYM" ~ "GBR", # Cayman Islands
      .data[[col_iso3]] == "GIB" ~ "GBR", # Gibraltar
      .data[[col_iso3]] == "PCN" ~ "GBR", # Pitcairn Islands
      .data[[col_iso3]] == "SHN" ~ "GBR", # St Helena, ascencion and Tristan da cunha
      .data[[col_iso3]] == "TCA" ~ "GBR", # Turks and Caicos
      .data[[col_iso3]] == "FLK" ~ "GBR", # Falkland Islands
      .data[[col_iso3]] == "IMN" ~ "GBR", # Isle of Man
      .data[[col_country_name]] == "Channel Islands" ~ "GBR", # Channel Islands does not have an ISO3 code
      # France's territories----------------------------------------------------
      .data[[col_iso3]] == "PYF" ~ "FRA", # French Polynesia
      .data[[col_iso3]] == "MYT" ~ "FRA", # Mayotte
      .data[[col_iso3]] == "NCL" ~ "FRA", # New Caledonia
      .data[[col_iso3]] == "SPM" ~ "FRA", # St. Pierre & Miquelon
      .data[[col_iso3]] == "WLF" ~ "FRA", # Wallis and Futuna
      .data[[col_iso3]] == "GUF" ~ "FRA", # French Guiana
      .data[[col_iso3]] == "GLP" ~ "FRA", # Guadeloupe
      .data[[col_iso3]] == "MTQ" ~ "FRA", # Martinique
      .data[[col_iso3]] == "MCO" ~ "FRA", # Monaco
      .data[[col_iso3]] == "REU" ~ "FRA", # Reunion
      .data[[col_iso3]] == "MAF" ~ "FRA", # Saint Martin
      .data[[col_iso3]] == "BLM" ~ "FRA", # Saint Barthelemy
      .data[[col_iso3]] == "ATF" ~ "FRA", # French Southern and Antartic Lands
      # China's territories-----------------------------------------------------
      .data[[col_iso3]] == "HKG" ~ "CHN", # Hong Kong -> China
      .data[[col_iso3]] == "MAC" ~ "CHN", # Macao -> China
      # Netherlands' territories------------------------------------------------
      .data[[col_iso3]] == "ABW" ~ "NLD", # Aruba
      .data[[col_iso3]] == "ANT" ~ "NLD", # Netherlands Antilles
      .data[[col_iso3]] == "BES" ~ "NLD", # Bonaire, Sint Eustatius and Saba
      .data[[col_iso3]] == "SXM" ~ "NLD", # Sint Maarten
      .data[[col_iso3]] == "CUW" ~ "NLD", # Curacao
      # New Zealand's territories-----------------------------------------------
      .data[[col_iso3]] == "COK" ~ "NZL", # Cook Islands
      .data[[col_iso3]] == "NIU" ~ "NZL", # Niue
      .data[[col_iso3]] == "TKL" ~ "NZL", # Tokelau
      # Australia's territories-------------------------------------------------
      .data[[col_iso3]] == "NFK" ~ "AUS", # Norfolk Island
      .data[[col_iso3]] == "CXR" ~ "AUS", # Christmas Island
      .data[[col_iso3]] == "CCK" ~ "AUS", # Cocos (keeling) Islands
      # Denmark's territories---------------------------------------------------
      .data[[col_iso3]] == "GRL" ~ "DNK", # Greenland
      .data[[col_iso3]] == "FRO" ~ "DNK", # Faroe Islands
      # Tanzania's territories--------------------------------------------------
      .data[[col_iso3]] == "EAZ" ~ "TZA", # Zanzibar
      TRUE ~ .data[[col_iso3]]
    )) %>%
    #-----------------------------------------------------------------------------
  # Rename countries based on standardized iso3 codes
  mutate(clean_iso3c = case_when(
    .data[[col_country_name]] == "Other nei" ~ "NEI",
    TRUE ~ clean_iso3c
  )) %>%
    mutate(clean_country_name = case_when(
      clean_iso3c == "SCG" ~ .data[[col_country_name]],
      clean_iso3c != "NEI" ~ countrycode(clean_iso3c, origin = "iso3c", destination = "country.name"),
      TRUE ~ .data[[col_country_name]]
    )) %>%
    # Filter out cases that do not appear in final production
    filter(
      !(clean_iso3c %in% c("CSK", "SUN", "YUG"))
    )
  
  return(cleaned_data)
}

#_____________________________________________________________________________________________________________
# CLEAN STRESSOR DATA
#_____________________________________________________________________________________________________________
# Items to remove sugar, oil, spices, stimulants (other than oils as part of feeds)
items_exclude <- c("sugar cane", "cocoa, beans", "cloves", "spices nes", "anise, badian, fennel, 
                   coriander", "sugar beet", "cinnamon (cannella)", "ginger", 
                   "nutmeg, mace and cardamoms", "vanilla", "chicory roots", "peppermint",
                   "oilcrop", "palmoil", "coconuts")

stressor_trade <- stressor_trade %>% 
  # Remove non-food items
  mutate(item = tolower(item)) %>%
  filter(!(item %in% items_exclude)) %>%
  # Modify food groups so feeds are included with livestock/aquaculture
  rename("food_group_original" = "food_group") %>%
  mutate(food_group = case_when(
    (str_detect(food_group_original, pattern = "_feed") & 
       str_detect(item, pattern = "culture")) ~ "fisheries and aquaculture",
    (str_detect(food_group_original, pattern = "_feed") & 
       str_detect(item, pattern = "meat|milk|eggs|no_animal")) ~ "livestock",
    TRUE ~ food_group_original
  )) %>% 
  # Rename to align with covariate structure
  rename("iso3_o" = "producer_iso3c", "iso3_d" = "consumer_iso3c") %>% 
  # Add country names 
  mutate(country_o = countrycode(iso3_o, origin = "iso3c", destination = "country.name"), 
         country_d = countrycode(iso3_d, origin = "iso3c", destination = "country.name")) 


# perform standardization on origins
stressor_trade <- standardize_countries(data = stressor_trade,
                                        col_iso3 = "iso3_o",
                                        col_country_name = "country_o") %>% 
  #remove iso3_o and artis_country_name
  select(-c(iso3_o, country_o)) %>% 
  # make the artis_iso3 into iso3_o
  rename(iso3_o = clean_iso3c, country_o = clean_country_name)

# now do the same for destination and organize df
stressor_trade <- standardize_countries(data = stressor_trade,
                                        col_iso3 = "iso3_d",
                                        col_country_name = "country_d") %>% 
  #remove iso3_o and artis_country_name
  select(-c(iso3_d, country_d)) %>% 
  # make the artis_iso3 into iso3_o
  rename(iso3_d = clean_iso3c, country_d = clean_country_name)

#_____________________________________________________________________________________________________________
# FORMAT TOTAL STRESSOR DATA 
#_____________________________________________________________________________________________________________
stressor_trade_total <- stressor_trade %>%
  # Calculate total flow by origin-destination pair
  group_by(iso3_o, country_o, iso3_d, country_d) %>%
  summarise(tonnes = sum(tonnes_traded), 
            ghg = sum(ghg_traded), 
            water = sum(water_traded), 
            nutrient = sum(nutrient_traded), 
            disturbance = sum(disturbance_traded)) %>%
  # Remove small flows (NOTE: This effectively undoes the all origin-destination code from above. If kept, remove
  # irrelevant code)
  filter(tonnes > flow_threshold)

# # ppml can handle zeros, so if we believe missing values to be true zeros, we can add them back in as explicit zeros.
# # NOTE: This is currently undone by the filter later. If we keep that, this step can be removed. 
countries <- unique(c(stressor_trade$iso3_o, stressor_trade$iso3_d))

stressor_trade_total <- expand_grid(iso3_o = countries, iso3_d = countries) %>%
  # Join stressor data to all combinations of origin-destination
  left_join(stressor_trade_total, by = c("iso3_o", "iso3_d")) %>%
  # Add zeros in for NAs (i.e., take missing values to be true zeros)
  mutate_at(c("tonnes", "ghg", "water", "nutrient", "disturbance"),
            ~replace_na(.,0)) %>%
  # Add country names 
  mutate(country_o = countrycode(iso3_o, origin = "iso3c", destination = "country.name"), 
         country_d = countrycode(iso3_d, origin = "iso3c", destination = "country.name")) 


#_____________________________________________________________________________________________________________
# FORMAT STRESSOR DATA BY FOOD TYPE
#_____________________________________________________________________________________________________________
# Data summarized by food group
stressor_trade_by_food <- stressor_trade %>%
  # Calculate total flow by origin-destination pair, by food group
  group_by(iso3_o, country_o, iso3_d, country_d, food_group) %>%
  summarise(tonnes = sum(tonnes_traded), 
            ghg = sum(ghg_traded), 
            water = sum(water_traded), 
            nutrient = sum(nutrient_traded), 
            disturbance = sum(disturbance_traded)) %>%
  # Remove small flows (NOTE: This effectively undoes the all origin-destination code from above. If kept, remove
  # irrelevant code)
  filter(tonnes > flow_threshold)

stressor_trade_by_food <- expand_grid(iso3_o = countries, iso3_d = countries) %>%
  # Join stressor data to all combinations of origin-destination
  left_join(stressor_trade_by_food, by = c("iso3_o", "iso3_d")) %>%
  # Add zeros in for NAs (i.e., take missing values to be true zeros)
  mutate_at(c("tonnes", "ghg", "water", "nutrient", "disturbance"),
            ~replace_na(.,0)) %>%
  # Add country names 
  mutate(country_o = countrycode(iso3_o, origin = "iso3c", destination = "country.name"), 
         country_d = countrycode(iso3_d, origin = "iso3c", destination = "country.name")) 

#_____________________________________________________________________________________________________________
# FORMAT TOTAL STRESSOR DATA WITH COVARIATES
#_____________________________________________________________________________________________________________
stressor_trade_total_w_covariates <- stressor_trade_total %>% 
  # Join on the covariates
  left_join(gravity_covars, by = c("iso3_o", "iso3_d")) %>%
  # FIXIT: Origin-destination combos with missing covariate data create NAs
  filter(!is.na(gdp_d)) %>%
  ungroup() 

#_____________________________________________________________________________________________________________
# FORMAT STRESSOR DATA BY FOOD TYPE WITH COVARIATES
#_____________________________________________________________________________________________________________
stressor_trade_by_food_w_covariates <- stressor_trade_by_food %>% 
  # Join with covariates
  left_join(gravity_covars, by = c("iso3_o", "iso3_d")) %>%
  # FIXIT: Origin-destination combos with missing covariate data create NAs
  filter(!is.na(gdp_d)) %>%
  ungroup() 

#_____________________________________________________________________________________________________________
# WRITE OUT CLEANED DATA FILES
#_____________________________________________________________________________________________________________
# Write files without covariates joined
write.csv(stressor_trade_total, "data/stressor_trade_total.csv", row.names = FALSE)
write.csv(stressor_trade_by_food, "data/stressor_trade_by_food.csv", row.names = FALSE)

# Write files with covariates joined
write.csv(stressor_trade_total_w_covariates, "data/stressor_trade_total_w_covariates.csv", row.names = FALSE)
write.csv(stressor_trade_by_food_w_covariates, "data/stressor_trade_by_food_w_covariates.csv", row.names = FALSE)
