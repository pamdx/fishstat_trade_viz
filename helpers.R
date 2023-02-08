# Changing HC default theme

newtheme <- hc_theme_merge(
  getOption("highcharter.theme"),  
  hc_theme(colors = c('#377eb8','#4daf4a','#e41a1c','#984ea3','#ff7f00','#ffff33','#a65628','#f781bf'))
)

options(highcharter.theme = newtheme)

# Load data

cou_coordinates <- readRDS("cou_coordinates.RDS")

edata <- readRDS("trade_partner_ISSCAAP.RDS")

map <- readRDS("map.RDS")

# More user-friendly y axes on plots

addUnits <- function(n) {
  labels <- ifelse(n < 1000, round(n, 1),  # less than thousands
                   ifelse(n < 1e6, paste0(round(n/1e3, 1), ' thousands'),  # in thousands
                          ifelse(n < 1e9, paste0(round(n/1e6, 1), ' millions'),  # in millions
                                 ifelse(n < 1e12, paste0(round(n/1e9, 1), ' billions'), # in billions
                                        ifelse(n < 1e15, paste0(round(n/1e12, 1), ' trillions'), # in trillions
                                               'too big!'
                                        )))))
  return(labels)
}