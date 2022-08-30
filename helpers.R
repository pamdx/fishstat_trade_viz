# Changing HC default theme
newtheme <- hc_theme_merge(
  getOption("highcharter.theme"),  
  hc_theme(colors = c('#377eb8','#4daf4a','#e41a1c','#984ea3','#ff7f00','#ffff33','#a65628','#f781bf'))
)

options(highcharter.theme = newtheme)

# Load data

edata <- readRDS("trade_partner_agg.RDS")

map <- readRDS("map.RDS")
