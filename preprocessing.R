##### UPDATE DATA INPUTS #####

library(readr)
library(dplyr)
library(highcharter)

# Get trade data by partner

temp <- tempfile()
download.file("https://www.fao.org/fishery/static/Data/FI_Trade_Partners_2024.1.0.zip", temp)
data_value <- read_csv(unz(temp, "TRADE_PARTNERS_VALUE.csv"))
data_quantity <- read_csv(unz(temp, "TRADE_PARTNERS_QUANTITY.csv"))
countries <- read_csv(unz(temp, "CL_FI_COUNTRY_GROUPS.csv"), na = "") %>% # adding na = "" so that Namibia's ISO2 code isn't interpreted as a missing value
  select(UN_Code, Name_En, Continent_Group_En, GeoRegion_Group_En)
commodities <- read_csv(unz(temp, "CL_FI_COMMODITY_ISSCFC.csv")) %>%
  select(Code, ISSCAAP, Name_En)
unlink(temp)

# Join tables, restructure and export data

data <- rbind(data_value, data_quantity)

trade_partner_raw <- data %>%
  rename(reporting_un_code = COUNTRY_REPORTER.UN_CODE,
         partner_un_code = COUNTRY_PARTNER.UN_CODE) %>%
  left_join(countries, by = c("reporting_un_code" = "UN_Code"), keep = FALSE) %>%
  rename(reporting_country = Name_En, reporting_continent = Continent_Group_En, reporting_region = GeoRegion_Group_En) %>%
  left_join(countries, by = c("partner_un_code" = "UN_Code"), keep = FALSE) %>%
  rename(partner_country = Name_En, partner_continent = Continent_Group_En, partner_region = GeoRegion_Group_En) %>%
  left_join(commodities, by = c("COMMODITY.FAO_CODE" = "Code"), keep = FALSE) %>%
  rename(commodity_code = COMMODITY.FAO_CODE, commodity_isscaap_group = ISSCAAP, commodity_name = Name_En) %>%
  rename(trade_flow	= TRADE_FLOW.ALPHA_CODE, unit = MEASURE,	year = PERIOD,	value = VALUE) %>%
  mutate(trade_flow = case_when(trade_flow == "I" ~ "Imports", trade_flow == "E" ~ "Exports", trade_flow == "R" ~ "Exports")) %>%
  mutate(unit = case_when(unit == "V_USD_1000" ~ "USD", unit == "Q_tpw" ~ "Tonnes - net product weight"),
         value = case_when(unit == "USD" ~ value * 1000, unit == "Tonnes - net product weight" ~ value),
         year = as.integer(year)) %>%
  select(reporting_un_code, reporting_country,	reporting_continent,	reporting_region,	partner_un_code, partner_country,	partner_continent,	partner_region, commodity_code,	commodity_isscaap_group, commodity_name,	trade_flow,	unit,	year,	value) %>%
  group_by_at(vars(-value)) %>%
  summarise(value = sum(value)) %>%
  ungroup()

# Deal with missing values where partner_country is "Other nei"

# trade_partner_raw$partner_iso2[trade_partner_raw$partner_country == "Other nei"] <- "XX"
trade_partner_raw$partner_continent[is.na(trade_partner_raw$partner_continent)] <- "Others"
trade_partner_raw$partner_region[is.na(trade_partner_raw$partner_region)] <- "Others"

# Add "China, " in front of the name for Taiwan

trade_partner_raw$reporting_country[trade_partner_raw$reporting_un_code == "158"] <- "China, Taiwan Province of China"
trade_partner_raw$partner_country[trade_partner_raw$partner_un_code == "158"] <- "China, Taiwan Province of China"

# Join geographical coordinates

cou_coordinates <- read_csv("https://raw.githubusercontent.com/pamdx/country_coordinates/refs/heads/main/country_coordinates.csv") %>%
  select(un_code, lat, lon)

saveRDS(cou_coordinates, "cou_coordinates.RDS")

trade_partner_raw <- trade_partner_raw %>%
  left_join(y = cou_coordinates, by = c("partner_un_code" = "un_code"))

if (any(is.na(trade_partner_raw$lat)) | any(is.na(trade_partner_raw$lon))) {
  
  warning(paste("The following partner countries are missing coordinates:", 
                trade_partner_raw %>%
                  filter(is.na(lat)) %>%
                  pull(partner_country) %>%
                  unique()
  )
  )
  
}

saveRDS(trade_partner_raw, "trade_partner_raw.RDS")

# Aggregate data at ISSCAAP group level

isscaap_classif <- read_csv("https://raw.githubusercontent.com/openfigis/RefData/gh-pages/species/CL_FI_SPECIES_ISSCAAP_GROUP.csv") %>%
  rename(commodity_isscaap_group = ISSCAAP_Code, name_isscaap_group = Name_En) %>%
  select(commodity_isscaap_group, name_isscaap_group) %>%
  mutate(conc_isscaap_group = paste(commodity_isscaap_group, "-", name_isscaap_group)) %>%
  mutate(commodity_isscaap_division = as.numeric(substr(commodity_isscaap_group, 1, 1))) %>%
  left_join(read_csv("https://raw.githubusercontent.com/openfigis/RefData/gh-pages/species/CL_FI_SPECIES_ISSCAAP_DIVISION.csv"), by = c("commodity_isscaap_division" = "ISSCAAP_Code")) %>%
  select(-c(Identifier, Name_Es, Name_Fr, Name_Ar, Name_Cn, Name_Ru)) %>%
  rename(name_isscaap_division = Name_En) %>%
  mutate(conc_isscaap_division = paste(commodity_isscaap_division, "-", name_isscaap_division)) %>%
  mutate(name_yearbook_selection = case_when(
    commodity_isscaap_group %in% c(1:59, 71:72, 74:77) ~ "Aquatic animals", 
    commodity_isscaap_group %in% c(61:64, 73, 81:83) ~ "Other aq. animals & products", 
    commodity_isscaap_group > 90 ~ "Algae"))

trade_partner_ISSCAAPgroup <- trade_partner_raw %>%
  group_by_at(vars(-c("commodity_code", "value", "commodity_name"))) %>%
  summarise(value = sum(value)) %>%
  ungroup() %>%
  left_join(isscaap_classif) %>%
  select(-c("commodity_isscaap_division", "name_isscaap_division", "conc_isscaap_division", "name_yearbook_selection"))

saveRDS(trade_partner_ISSCAAPgroup, "trade_partner_ISSCAAPgroup.RDS")

trade_partner_ISSCAAPdivision <- trade_partner_raw %>%
  left_join(isscaap_classif) %>%
  select(-c("commodity_isscaap_group", "name_isscaap_group", "conc_isscaap_group", "name_yearbook_selection")) %>%
  group_by_at(vars(-c("commodity_code", "value", "commodity_name"))) %>%
  summarise(value = sum(value)) %>%
  ungroup()

saveRDS(trade_partner_ISSCAAPdivision, "trade_partner_ISSCAAPdivision.RDS")

trade_partner_yearbookgroup <- trade_partner_raw %>%
  left_join(isscaap_classif) %>%
  select(-c("commodity_isscaap_group", "name_isscaap_group", "conc_isscaap_group",
            "commodity_isscaap_division", "name_isscaap_division", "conc_isscaap_division")) %>%
  group_by_at(vars(-c("commodity_code", "value", "commodity_name"))) %>%
  summarise(value = sum(value)) %>%
  ungroup()

saveRDS(trade_partner_yearbookgroup, "trade_partner_yearbookgroup.RDS")

# Aggregate data at country level

trade_partner_agg <- trade_partner_raw %>%
  group_by_at(vars(-c("commodity_isscaap_group", "commodity_code", "commodity_name", "value"))) %>%
  summarise(value = sum(value)) %>%
  ungroup()

saveRDS(trade_partner_agg, "trade_partner_agg.RDS")

# Download map for HC viz

# map <- download_map_data(url = "custom/world-continents.js", showinfo = FALSE, quiet = FALSE)
# 
# saveRDS(map, "map.RDS")
