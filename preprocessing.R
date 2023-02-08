##### UPDATE DATA INPUTS #####

library(readr)
library(dplyr)
library(highcharter)

# Get trade data by partner

temp <- tempfile()
download.file("https://www.fao.org/fishery/static/Data/FI_Trade_Partners_2022.1.0.zip", temp)
data <- read_csv(unz(temp, "TRADE_PARTNERS_VALUE.csv"))
countries <- read_csv(unz(temp, "CL_FI_COUNTRY_GROUPS.csv"), na = "") %>% # adding na = "" so that Namibia's ISO2 code isn't interpreted as a missing value
  select(UN_Code, ISO2_Code, Name_En, Continent_Group_En, GeoRegion_Group_En)
commodities <- read_csv(unz(temp, "CL_FI_COMMODITY_ISSCFC.csv")) %>%
  select(Code, ISSCAAP, Name_En)
unlink(temp)

# Join tables, restructure and export data

trade_partner_raw <- data %>%
  left_join(countries, by = c("COUNTRY_REPORTER.UN_CODE" = "UN_Code"), keep = FALSE) %>%
  rename(reporting_country = Name_En, reporting_iso2 = ISO2_Code, reporting_continent = Continent_Group_En, reporting_region = GeoRegion_Group_En) %>%
  left_join(countries, by = c("COUNTRY_PARTNER.UN_CODE" = "UN_Code"), keep = FALSE) %>%
  rename(partner_country = Name_En, partner_iso2 = ISO2_Code, partner_continent = Continent_Group_En, partner_region = GeoRegion_Group_En) %>%
  left_join(commodities, by = c("COMMODITY.FAO_CODE" = "Code"), keep = FALSE) %>%
  rename(commodity_code = COMMODITY.FAO_CODE, commodity_isscaap_group = ISSCAAP, commodity_name = Name_En) %>%
  rename(trade_flow	= TRADE_FLOW.ALPHA_CODE, unit = MEASURE,	year = PERIOD,	value = VALUE) %>%
  mutate(trade_flow = case_when(trade_flow == "I" ~ "Imports", trade_flow == "E" ~ "Exports", trade_flow == "R" ~ "Exports")) %>%
  mutate(unit = "USD", value = value * 1000, year = as.integer(year)) %>%
  select(reporting_country,	reporting_iso2,	reporting_continent,	reporting_region,	partner_country,	partner_iso2,	partner_continent,	partner_region, commodity_code,	commodity_isscaap_group, commodity_name,	trade_flow,	unit,	year,	value) %>%
  group_by_at(vars(-value)) %>%
  summarise(value = sum(value)) %>%
  ungroup()

# Deal with missing values where partner_country is "Other nei"

trade_partner_raw$partner_iso2[trade_partner_raw$partner_country == "Other nei"] <- "XX"
trade_partner_raw$partner_continent[is.na(trade_partner_raw$partner_continent)] <- "Others"
trade_partner_raw$partner_region[is.na(trade_partner_raw$partner_region)] <- "Others"

# Join geographical coordinates

trade_partner_raw$partner_iso2[trade_partner_raw$partner_country == "Czechoslovakia"] <- "CZ" # Fix lack of ISO2 for Czechoslovakia
trade_partner_raw$reporting_iso2[trade_partner_raw$reporting_country == "Czechoslovakia"] <- "CZ" # Fix lack of ISO2 for Czechoslovakia
trade_partner_raw$partner_iso2[trade_partner_raw$partner_country == "Sudan (former)"] <- "SD" # Fix lack of ISO2 for Sudan (former)
trade_partner_raw$reporting_iso2[trade_partner_raw$reporting_country == "Sudan (former)"] <- "SD" # Fix lack of ISO2 for Sudan (former)
trade_partner_raw$partner_iso2[trade_partner_raw$partner_country == "Zanzibar"] <- "ZZ" # Fix lack of ISO2 for Zanzibar (unofficial)
trade_partner_raw$reporting_iso2[trade_partner_raw$reporting_country == "Zanzibar"] <- "ZZ" # Fix lack of ISO2 for Zanzibar (unofficial)
trade_partner_raw$partner_iso2[trade_partner_raw$partner_country == "Channel Islands"] <- "CS" # Fix lack of ISO2 for Channel Islands (unofficial)
trade_partner_raw$reporting_iso2[trade_partner_raw$reporting_country == "Channel Islands"] <- "CS" # Fix lack of ISO2 for Channel Islands (unofficial)
trade_partner_raw$partner_iso2[trade_partner_raw$partner_country == "Sint Maarten"] <- "SX" # Fix lack of ISO2 for Sint Maarten (unofficial)
trade_partner_raw$reporting_iso2[trade_partner_raw$reporting_country == "Sint Maarten"] <- "SX" # Fix lack of ISO2 for Sint Maarten (unofficial)
trade_partner_raw$partner_iso2[trade_partner_raw$partner_country == "Saint-Martin (French)"] <- "SF" # Fix lack of ISO2 for Saint-Martin (French) (unofficial)
trade_partner_raw$reporting_iso2[trade_partner_raw$reporting_country == "Saint-Martin (French)"] <- "SF" # Fix lack of ISO2 for Saint-Martin (French) (unofficial)
trade_partner_raw$partner_iso2[trade_partner_raw$partner_country == "Saint Barthélemy"] <- "SF" # Fix lack of ISO2 for Saint Barthélemy (unofficial)
trade_partner_raw$reporting_iso2[trade_partner_raw$reporting_country == "Saint Barthélemy"] <- "SW" # Fix lack of ISO2 for Saint Barthélemy (unofficial)

cou_coordinates <- read_csv("https://raw.githubusercontent.com/google/dspl/master/samples/google/canonical/countries.csv", na = "") %>%
  rename(ISO2 = country) %>%
  select(ISO2, latitude, longitude) %>%
  add_row(ISO2 = "XX", latitude = -50, longitude = 0) %>% # Add Other nei's coordinates
  add_row(ISO2 = "SU", latitude = 61.524010, longitude = 105.318756) %>% # Add USSR using Russia's coordinates
  add_row(ISO2 = "BQ", latitude = 12.201890, longitude = -68.262383) %>% # Add Bonaire's coordinates
  add_row(ISO2 = "CW", latitude = 12.169570, longitude = -68.990021) %>% # Add Curaçao's coordinates
  add_row(ISO2 = "CS", latitude = 44.016521, longitude = 21.005859) %>% # Add Serbia and Montenegro using Serbia's coordinates
  add_row(ISO2 = "SS", latitude = 4.859363, longitude = 31.571251) %>% # Add South Sudan's coordinates
  add_row(ISO2 = "YU", latitude = 44.016521, longitude = 21.005859) %>% # Add Yugoslavia using Serbia's coordinates
  add_row(ISO2 = "ZZ", latitude = -6.165917, longitude = 39.202641) %>% # Add Zanzibar using its capital's coordinates
  add_row(ISO2 = "CS", latitude = 49.354417, longitude = -2.372106) %>% # Add Channel Islands using an arbitrary point in the English Channel
  add_row(ISO2 = "SX", latitude = 18.0237, longitude = -63.0458) %>% # Add Sint Maarten using its capital's coordinates
  add_row(ISO2 = "SF", latitude = 18.0731, longitude = -63.0822) %>% # Add Saint-Martin (French) using its capital's coordinates
  add_row(ISO2 = "SW", latitude = 17.897908, longitude = -62.850556) # Add Saint Barthélemy using its capital's coordinates

saveRDS(cou_coordinates, "cou_coordinates.RDS")

trade_partner_raw <- trade_partner_raw %>%
  left_join(y = cou_coordinates, by = c("partner_iso2" = "ISO2")) %>%
  rename(lat = latitude, lon = longitude)

saveRDS(trade_partner_raw, "trade_partner_raw.RDS")

# Aggregate data at ISSCAAP group level

isscaap_classif <- read_csv("https://raw.githubusercontent.com/openfigis/RefData/gh-pages/species/CL_FI_SPECIES_ISSCAAP_GROUP.csv") %>%
  rename(commodity_isscaap_group = ISSCAAP_Code, name_isscaap_group = Name_En) %>%
  select(commodity_isscaap_group, name_isscaap_group) %>%
  mutate(conc_isscaap_group = paste(commodity_isscaap_group, "-", name_isscaap_group))

trade_partner_ISSCAAP <- trade_partner_raw %>%
  group_by_at(vars(-c("commodity_code", "value", "commodity_name"))) %>%
  summarise(value = sum(value)) %>%
  ungroup() %>%
  left_join(isscaap_classif)

saveRDS(trade_partner_ISSCAAP, "trade_partner_ISSCAAP.RDS")

# Aggregate data at country level

trade_partner_agg <- trade_partner_ISSCAAP %>%
  group_by_at(vars(-c("commodity_isscaap_group", "name_isscaap_group", "conc_isscaap_group", "value"))) %>%
  summarise(value = sum(value)) %>%
  ungroup()

saveRDS(trade_partner_agg, "trade_partner_agg.RDS")

# Download map for HC viz

map <- download_map_data(url = "custom/world-continents.js", showinfo = FALSE, quiet = FALSE)

saveRDS(map, "map.RDS")
