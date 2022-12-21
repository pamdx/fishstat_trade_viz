##### UPDATE DATA INPUTS #####

library(readr)
library(dplyr)
library(highcharter)

# Get trade data by partner

temp <- tempfile()
download.file("https://www.fao.org/fishery/static/Data/FI_Trade_Partners_2022.1.0.zip", temp)
data <- read_csv(unz(temp, "TRADE_PARTNERS_VALUE.csv"))
countries <- read_csv(unz(temp, "CL_FI_COUNTRY_GROUPS.csv")) %>%
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

cou_coordinates <- read_csv("https://raw.githubusercontent.com/google/dspl/master/samples/google/canonical/countries.csv", na = "") %>%
  rename(ISO2 = country) %>%
  select(ISO2, latitude, longitude)

trade_partner_raw <- trade_partner_raw %>%
  left_join(y = cou_coordinates, by = c("partner_iso2" = "ISO2")) %>%
  rename(lat = latitude, lon = longitude)

trade_partner_raw$lat[trade_partner_raw$partner_country == "Other nei"] <- -50
trade_partner_raw$lon[trade_partner_raw$partner_country == "Other nei"] <- 0

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
  group_by_at(vars(-c("commodity_isscaap_group", "value"))) %>%
  summarise(value = sum(value)) %>%
  ungroup()

saveRDS(trade_partner_agg, "trade_partner_agg.RDS")

# Download map for HC viz

map <- download_map_data(url = "custom/world-continents.js", showinfo = FALSE, quiet = FALSE)

saveRDS(map, "map.RDS")
