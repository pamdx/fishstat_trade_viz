# Changing HC default theme
newtheme <- hc_theme_merge(
  getOption("highcharter.theme"),  
  hc_theme(colors = c('#377eb8','#4daf4a','#e41a1c','#984ea3','#ff7f00','#ffff33','#a65628','#f781bf'))
)

options(highcharter.theme = newtheme)

# Load data

cou_coordinates <- read_csv("https://raw.githubusercontent.com/google/dspl/master/samples/google/canonical/countries.csv", na = "") %>%
  rename(ISO2 = country) %>%
  select(ISO2, latitude, longitude) %>%
  add_row(ISO2 = "SU", latitude = 61.524010, longitude = 105.318756) %>% # Add USSR using Russia's coordinates
  add_row(ISO2 = "BQ", latitude = 12.201890, longitude = -68.262383) %>% # Add Bonaire's coordinates
  add_row(ISO2 = "CW", latitude = 12.169570, longitude = -68.990021) %>% # Add Curaçao's coordinates
  add_row(ISO2 = "CS", latitude = 44.016521, longitude = 21.005859) %>% # Add Serbia and Montenegro using Serbia's coordinates
  add_row(ISO2 = "SS", latitude = 4.859363, longitude = 31.571251) %>% # Add South Sudan's coordinates
  add_row(ISO2 = "YU", latitude = 44.016521, longitude = 21.005859) # Add Yugoslavia using Serbia's coordinates

edata <- readRDS("trade_partner_agg.RDS")

map <- readRDS("map.RDS")
