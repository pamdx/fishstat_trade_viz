library(shiny)
library(highcharter)
library(dplyr)
library(tidyr)
library(tibble)
library(readr)
library(DT)
library(shinyfullscreen)
library(shinycssloaders)

# library(rsconnect)
# deployApp()

source("helpers.R")

ui <- function(request){
        navbarPage(title = a(href = "https://www.fao.org/fishery/en/fishstat", target = "_blank", style="text-decoration:none;color:inherit", div(img(src = "fao-logo-blue-3lines-en.svg", id = "logo", height = "35px", style = "border-right: 1px solid grey; padding: 0 0.5rem; position: relative; margin:-15px 0px; display:right-align; "), "FishStat Trade Data")),
         tabPanel("Data Explorer",
          sidebarLayout(
            sidebarPanel(
              selectInput('country','Reporting country', choices = c("Please select...", sort(unique(data_agg$reporting_country)))),
              selectInput('flow','Trade flow', choices = c("Please select...",sort(unique(data_agg$trade_flow)))),
              selectInput('year','Year', choices = sort(unique(data_agg$year), decreasing = TRUE)),
              selectInput('species_choice', 'Filter by species group', choices = c('Disabled', 'Yearbook/SOFIA Selection', 'ISSCAAP Division', 'ISSCAAP Group'), selected = "Disabled"),
              conditionalPanel(
                condition = "input.species_choice == 'Yearbook/SOFIA Selection'",
                uiOutput('yearbook_selection'),
                helpText("")
              ),
              conditionalPanel(
                condition = "input.species_choice == 'ISSCAAP Division'",
                uiOutput('isscaap_division'),
                helpText("Click ", a(href = "https://www.fao.org/fishery/en/collection/asfis/en", "here", target = "_blank"), " for more information about the ISSCAAP classification.")
              ),
              conditionalPanel(
                condition = "input.species_choice == 'ISSCAAP Group'",
                uiOutput('isscaap_group'),
                helpText("Click ", a(href = "https://www.fao.org/fishery/en/collection/asfis/en", "here", target = "_blank"), " for more information about the ISSCAAP classification.")
              ),
              
              hr(),
              fullscreen_button("full_screen", label = "Fullscreen On/Off", icon = shiny::icon("expand", lib = "font-awesome"), target = NULL),
              br(),
              br(),
              bookmarkButton(label = "Share this view", icon = shiny::icon("share-alt", lib = "font-awesome")),
              width=2
            ),
            mainPanel(
              tabsetPanel(
                tabPanel(
                  "Map", 
                  highchartOutput('map', height = "700px") %>% withSpinner()
                  ),
                tabPanel(
                  "Chart", 
                  highchartOutput("chart", height = "700px") %>% withSpinner()
                  ),
                tabPanel(
                  "Table", 
                  DT::dataTableOutput("data_table", height = "700px") %>% withSpinner()
                  )
              )
            )
          ),
          
          hr(),
          div(
            class = "footer",
            includeHTML("footer.html")
          ),
        ),
       tabPanel("Readme",
                 fluidPage(
                   mainPanel(
                     h1("How to use this tool"),
                     p("This website features interactive visualizations to explore FAO's", a(href="https://www.fao.org/fishery/en/collection/global_commodity_prod?lang=en", "Global Fish Trade by Partner Country", target="_blank"), "dataset."),
                     p("We hope you enjoy this application. Click ", a(href="https://www.fao.org/fishery/en/fishstat", "here", target="_blank"), "if you want to learn more about FAO's Fisheries and Aquaculture statistics."),
                     p("We encourage users to provide their feedback or ask their questions about this tool at", a(href="mailto:Fish-Statistics-Inquiries@fao.org", "Fish-Statistics-Inquiries@fao.org", target="_blank")),
                     h2("The Data Explorer"),
                     p("Under the", em("Data Explorer"), "tab, you can explore in details how the country of your choice trades with partner countries around the world."),
                     h3("Side panel"),
                     p("The", em("side panel"), "located on the left of the user interface allows you to filter the data to visualize on the right side of the interface. Using these filters, one can display the imports/exports flows towards/from a given country for a specific year. The data can also be further filtered by species group (three species classifications are available). Finally, the two buttons on the bottom of the side panels allow the user to display the application in fullscreen and to share the current view with somebody else."), 
                     tags$img(src = "side_panel.png"),
                     h3("Map"),
                     p("The data is first represented on a map under the", em("Map"), "tab. While the green bubble represents the reporting country, the blue bubbles represent the reporting country's trade flows with the rest of the world for the year selected in the side panel. Placing your cursor on individual bubbles will give you more information on a specific partner country's trade with the reporting country. You can zoom in or out of the map using the + and - buttons on the top left of the map. This is particularly useful to better explore data in areas of the world with a high density of countries. Finally, you can export the map as an image by clicking on the three lines on the top right of the map."), 
                     tags$img(src = "map_illustration.png"),
                     h3("Chart"),
                     p("If you want to focus on the main partner countries for the reporting country you selected, you can display them as a bar chart ordered by their shares of trade under the", em("Chart"), "tab. Placing your cursor on individual bars will give you more information on a given partner country's trade with the reporting country. You can export the visual as an image by clicking on the three lines on the top right of the chart."), 
                     tags$img(src = "chart_illustration.png"),
                     h3("Table"),
                     p("Finally, you can display a table listing the partner countries for the reporting country you selected in the", em("Table"), "tab. The data can be exported by clicking on any of the buttons on the top left of the table."),
                     tags$img(src = "table_illustration.png"),
                     h1("Notes"),
                     p("Differences between figures given for total exports and total imports of any one commodity may be due to several factors, e.g. the time lapse between the dispatch of goods from the exporting country and their arrival in the importing country; the use of a different classification of the same product by different countries; or the fact that some countries supply trade data on general trade, while others give data on special trade.")
                   )
                 )
        )
      )
}

server <- function(input, output, session) {
  
  # Initialize conditional species filters
  
  output$yearbook_selection <- renderUI({
    selectInput('yearbook_selection','Yearbook/SOFIA Selection', choices = unique(sort(data_yearbook$name_yearbook_selection)), selected = 'Aquatic animals', multiple = FALSE)
  })
  
  output$isscaap_division <- renderUI({
    selectInput('isscaap_division','ISSCAAP Division', choices = unique(sort(data_division$conc_isscaap_division)), multiple = FALSE)
  })
  
  output$isscaap_group <- renderUI({
    selectInput('isscaap_group','ISSCAAP Group', choices = unique(sort(data_group$conc_isscaap_group)), multiple = FALSE)
  })
  
  # Country overview
  
  data <- reactive(switch(input$species_choice, 
                          'Disabled' = data_agg, 
                          'Yearbook/SOFIA Selection' = data_yearbook, 
                          'ISSCAAP Division' = data_division, 
                          'ISSCAAP Group' = data_group) %>% 
                     filter(year == input$year) %>%
                     filter(reporting_country == input$country) %>%
                     filter(trade_flow == input$flow) %>%
                     {if (input$species_choice == 'Yearbook/SOFIA Selection') filter(., name_yearbook_selection %in% input$yearbook_selection) 
                       else if (input$species_choice == 'ISSCAAP Division') filter(., conc_isscaap_division %in% input$isscaap_division) 
                       else if (input$species_choice == 'ISSCAAP Group') filter(., conc_isscaap_group %in% input$isscaap_group) 
                       else .} %>%
                     rename(z = value) %>%
                     group_by() %>%
                     mutate(total = sum(z)) %>%
                     ungroup() %>%
                     mutate(z_formatted = addUnits(z)) %>%
                     mutate(share = sprintf("%0.1f%%", z/total*100))
                   )
  
  data_reporting <- reactive(switch(input$species_choice, 
                                    'Disabled' = data_agg, 
                                    'Yearbook/SOFIA Selection' = data_yearbook, 
                                    'ISSCAAP Division' = data_division, 
                                    'ISSCAAP Group' = data_group) %>% 
                               filter(year == input$year) %>%
                               filter(reporting_country == input$country) %>%
                               filter(trade_flow == input$flow) %>%
                               rename(z = value) %>%
                               group_by(reporting_country, reporting_iso2, unit, year, trade_flow) %>%
                               summarise(z = sum(z)) %>%
                               inner_join(y = cou_coordinates, by = c("reporting_iso2" = "ISO2")) %>%
                               rename(lat = latitude, lon = longitude) %>%
                               mutate(z = replace(z, is.numeric(z), 1))
                             )
  
  data_total <- reactive(switch(input$species_choice, 
                                'Disabled' = data_agg, 
                                'Yearbook/SOFIA Selection' = data_yearbook, 
                                'ISSCAAP Division' = data_division, 
                                'ISSCAAP Group' = data_group) %>% 
                           filter(year == input$year) %>%
                           filter(reporting_country == input$country) %>%
                           filter(trade_flow == input$flow) %>%
                           {if (input$species_choice == 'Yearbook/SOFIA Selection') filter(., name_yearbook_selection %in% input$yearbook_selection) 
                             else if (input$species_choice == 'ISSCAAP Division') filter(., conc_isscaap_division %in% input$isscaap_division) 
                             else if (input$species_choice == 'ISSCAAP Group') filter(., conc_isscaap_group %in% input$isscaap_group) 
                             else .} %>%
                           group_by(reporting_country) %>%
                           summarise(value = sum(value)) %>%
                           pull() %>%
                           addUnits()
                         )
  
  data_n <- reactive(switch(input$species_choice, 
                            'Disabled' = data_agg, 
                            'Yearbook/SOFIA Selection' = data_yearbook, 
                            'ISSCAAP Division' = data_division, 
                            'ISSCAAP Group' = data_group) %>% 
                       filter(year == input$year) %>%
                       filter(reporting_country == input$country) %>%
                       filter(trade_flow == input$flow) %>%
                       {if (input$species_choice == 'Yearbook/SOFIA Selection') filter(., name_yearbook_selection %in% input$yearbook_selection) 
                         else if (input$species_choice == 'ISSCAAP Division') filter(., conc_isscaap_division %in% input$isscaap_division) 
                         else if (input$species_choice == 'ISSCAAP Group') filter(., conc_isscaap_group %in% input$isscaap_group) 
                         else .} %>%
                       summarise(n = n()) %>%
                       pull()
  )

  output$map <- renderHighchart({
    
    if (input$country == "Please select..." || input$flow == "Please select...") {
      return(NULL)
    } else
    
    highchart(type = "map") %>%
      hc_add_series(mapData = map, showInLegend = F) %>%
      hc_add_series(data = data_reporting(), 
                    type = "mapbubble", 
                    name = if_else(input$flow == "Exports", "Origin of exports", "Destination of imports"), 
                    color = "#4daf4a",
                    minSize = "20",
                    maxSize = "20",
                    tooltip = list(pointFormat = "Country: {point.reporting_country}<br>Year: {point.year}")) %>%
      hc_add_series(data = data(), 
                    type = "mapbubble", 
                    name = if_else(input$flow == "Exports", "Destinations of exports", "Origin of imports"), 
                    color = "#377eb8",
                    tooltip = list(pointFormat = paste('Partner country: {point.partner_country}<br>Year: {point.year}<br>Value (USD): {point.z_formatted}<br>Share: {point.share}'))) %>%
      hc_title(text = 
                 if (input$species_choice == 'Yearbook/SOFIA Selection') {
                   paste0(input$country, ", ", tolower(input$flow),  " of ", tolower(data_yearbook[data_yearbook$name_yearbook_selection == input$yearbook_selection,]$name_yearbook_selection[[1]]), " (", input$year, ")")
                 } else if (input$species_choice == 'ISSCAAP Division') {
                   paste0(input$country, ", ", tolower(input$flow),  " of ", tolower(data_division[data_division$conc_isscaap_division == input$isscaap_division,]$name_isscaap_division[[1]]), " (", input$year, ")")
                 } else if (input$species_choice == 'ISSCAAP Group') {
                   paste0(input$country, ", ", tolower(input$flow),  " of ", tolower(data_group[data_group$conc_isscaap_group == input$isscaap_group,]$name_isscaap_group[[1]]), " (", input$year, ")")
                 } else {
                   paste0(input$country, ", ", tolower(input$flow),  " of fishery and aquaculture products (", input$year, ")")
                 }
               ) %>%
      hc_subtitle(text = paste0('Total ', tolower(input$flow), ": ", "USD ", data_total(), ", number of partners: ", data_n())) %>%
      hc_mapNavigation(enabled = T) %>%
      hc_legend(enabled = TRUE, 
                layout = "horizontal", 
                align = "right",
                verticalAlign = "bottom") %>%
      hc_caption(text = "<center>Note: 'Other nei' refers to unspecified partners.</center>") %>%
      hc_exporting(enabled = TRUE, 
                   buttons = list(
                     contextButton = list(
                       menuItems = hc_export_options
                     )
                   ),
                   chartOptions = list(
                     chart = list(
                       backgroundColor = "#FFFFFF"
                     )
                   ),
                   sourceWidth = 1920,
                   sourceHeight = 1080,
                   filename = 
                     if (input$species_choice == 'Yearbook/SOFIA Selection') {
                       paste0("(Map) ", input$country, ", ", tolower(input$flow),  " of ", tolower(data_yearbook[data_yearbook$name_yearbook_selection == input$yearbook_selection,]$name_yearbook_selection[[1]]), " (", input$year, ")")
                     } else if (input$species_choice == 'ISSCAAP Division') {
                       paste0("(Map) ", input$country, ", ", tolower(input$flow),  " of ", tolower(data_division[data_division$conc_isscaap_division == input$isscaap_division,]$name_isscaap_division[[1]]), " (", input$year, ")")
                     } else if (input$species_choice == 'ISSCAAP Group') {
                       paste0("(Map) ", input$country, ", ", tolower(input$flow),  " of ", tolower(data_group[data_group$conc_isscaap_group == input$isscaap_group,]$name_isscaap_group[[1]]), " (", input$year, ")")
                     } else {
                       paste0("(Map) ", input$country, ", ", tolower(input$flow),  " of fishery and aquaculture products (", input$year, ")")
                     }
      )
  })
  
  data_chart <- reactive(
    switch(input$species_choice, 
           'Disabled' = data_agg, 
           'Yearbook/SOFIA Selection' = data_yearbook, 
           'ISSCAAP Division' = data_division, 
           'ISSCAAP Group' = data_group) %>%
      filter(year == input$year) %>%
      filter(reporting_country == input$country) %>%
      filter(trade_flow == input$flow) %>%
      {if (input$species_choice == 'Yearbook/SOFIA Selection') filter(., name_yearbook_selection %in% input$yearbook_selection) 
        else if (input$species_choice == 'ISSCAAP Division') filter(., conc_isscaap_division %in% input$isscaap_division) 
        else if (input$species_choice == 'ISSCAAP Group') filter(., conc_isscaap_group %in% input$isscaap_group) 
        else .} %>%
      arrange(desc(value)) %>%
      group_by() %>%
      mutate(total = sum(value)) %>%
      ungroup() %>%
      mutate(share = value/total*100) %>%
      mutate(partner_country = ifelse(share < 1, "Others", partner_country)) %>%
      mutate(bottom = ifelse(partner_country == "Others", 1, 0)) %>%
      group_by(partner_country, year, bottom) %>%
      summarize(value = sum(value), share = sum(share)) %>%
      ungroup() %>%
      arrange(bottom, desc(share)) %>%
      mutate(value_formatted = addUnits(value), share_pretty = sprintf("%0.1f%%", share))
  )
    
  output$chart <- renderHighchart({
    
    if (input$country == "Please select..." || input$flow == "Please select...") {
      return(NULL)
    } else
    
    hchart(data_chart(), 
           type = "column", 
           hcaes(x = partner_country, y = share), 
           name = paste("Share of", tolower(input$flow)),
           tooltip = list(pointFormat = "Partner country: {point.partner_country}<br>Year: {point.year}<br>Value (USD): {point.value_formatted}<br>Share: {point.share_pretty}")
    ) %>%
      hc_xAxis(title = list(text = NULL)) %>%
      hc_yAxis(title = list(text = paste("Share of", tolower(input$flow))),
               labels = list(format = "{value}%"),
               ceiling = 100) %>%
      hc_title(text = 
                 if (input$species_choice == 'Yearbook/SOFIA Selection') {
                   paste0(input$country, ", ", tolower(input$flow),  " of ", tolower(data_yearbook[data_yearbook$name_yearbook_selection == input$yearbook_selection,]$name_yearbook_selection[[1]]), " (", input$year, ")")
                 } else if (input$species_choice == 'ISSCAAP Division') {
                   paste0(input$country, ", ", tolower(input$flow),  " of ", tolower(data_division[data_division$conc_isscaap_division == input$isscaap_division,]$name_isscaap_division[[1]]), " (", input$year, ")")
                 } else if (input$species_choice == 'ISSCAAP Group') {
                   paste0(input$country, ", ", tolower(input$flow),  " of ", tolower(data_group[data_group$conc_isscaap_group == input$isscaap_group,]$name_isscaap_group[[1]]), " (", input$year, ")")
                 } else {
                   paste0(input$country, ", ", tolower(input$flow),  " of fishery and aquaculture products (", input$year, ")")
                 }
      ) %>%
      hc_subtitle(text = paste0('Total ', tolower(input$flow), ": ", "USD ", data_total(), ", number of partners: ", data_n())) %>%
      hc_caption(text = paste("Note: the 'Others' category groups all partner countries with a share of", tolower(input$flow), "lower than 1%, while 'Other nei' refers to unspecified partners.")) %>%
      hc_exporting(enabled = TRUE, 
                   buttons = list(
                     contextButton = list(
                       menuItems = hc_export_options
                     )
                   ),
                   chartOptions = list(
                     chart = list(
                       backgroundColor = "#FFFFFF"
                     )
                   ),
                   # sourceWidth = 1920,
                   # sourceHeight = 1080,
                   filename = 
                     if (input$species_choice == 'Yearbook/SOFIA Selection') {
                       paste0("(Chart) ", input$country, ", ", tolower(input$flow),  " of ", tolower(data_yearbook[data_yearbook$name_yearbook_selection == input$yearbook_selection,]$name_yearbook_selection[[1]]), " (", input$year, ")")
                     } else if (input$species_choice == 'ISSCAAP Division') {
                       paste0("(Chart) ", input$country, ", ", tolower(input$flow),  " of ", tolower(data_division[data_division$conc_isscaap_division == input$isscaap_division,]$name_isscaap_division[[1]]), " (", input$year, ")")
                     } else if (input$species_choice == 'ISSCAAP Group') {
                       paste0("(Chart) ", input$country, ", ", tolower(input$flow),  " of ", tolower(data_group[data_group$conc_isscaap_group == input$isscaap_group,]$name_isscaap_group[[1]]), " (", input$year, ")")
                     } else {
                       paste0("(Chart) ", input$country, ", ", tolower(input$flow),  " of fishery and aquaculture products (", input$year, ")")
                     }
      )
  })
  
  data_table <- reactive(
    switch(input$species_choice, 
           'Disabled' = data_agg, 
           'Yearbook/SOFIA Selection' = data_yearbook, 
           'ISSCAAP Division' = data_division, 
           'ISSCAAP Group' = data_group) %>% 
      filter(year == input$year) %>%
      filter(reporting_country == input$country) %>%
      filter(trade_flow == input$flow) %>%
      {if (input$species_choice == 'Yearbook/SOFIA Selection') filter(., name_yearbook_selection %in% input$yearbook_selection) 
        else if (input$species_choice == 'ISSCAAP Division') filter(., conc_isscaap_division %in% input$isscaap_division) 
        else if (input$species_choice == 'ISSCAAP Group') filter(., conc_isscaap_group %in% input$isscaap_group) 
        else .} %>%
      arrange(desc(value)) %>%
      group_by() %>%
      mutate(total = sum(value)) %>%
      ungroup() %>%
      mutate(share = value/total*100) %>%
      select(partner_country, year, trade_flow, value, unit, share)
    )
  
  output$data_table <- DT::renderDataTable(server = FALSE, { # server = FALSE used to make sure the entire dataset is downloaded when using the buttons
    
    if (input$country == "Please select..." || input$flow == "Please select...") {
      return(NULL)
    } else
    
    datatable(data_table(),
              extensions = 'Buttons',
              options = list(
                paging = TRUE,
                searching = TRUE,
                fixedColumns = TRUE,
                autoWidth = FALSE,
                ordering = TRUE,
                dom = 'Bfrtip',
                buttons = list('copy', list(
                  extend = 'collection',
                  buttons = list(
                    list(extend = 'csv', 
                         filename =
                           if (input$species_choice == 'Yearbook/SOFIA Selection') {
                             paste0("(Table) ", input$country, ", ", tolower(input$flow),  " of ", tolower(data_yearbook[data_yearbook$name_yearbook_selection == input$yearbook_selection,]$name_yearbook_selection[[1]]), " (", input$year, ")")
                           } else if (input$species_choice == 'ISSCAAP Division') {
                             paste0("(Table) ", input$country, ", ", tolower(input$flow),  " of ", tolower(data_division[data_division$conc_isscaap_division == input$isscaap_division,]$name_isscaap_division[[1]]), " (", input$year, ")")
                           } else if (input$species_choice == 'ISSCAAP Group') {
                             paste0("(Table) ", input$country, ", ", tolower(input$flow),  " of ", tolower(data_group[data_group$conc_isscaap_group == input$isscaap_group,]$name_isscaap_group[[1]]), " (", input$year, ")")
                           } else {
                             paste0("(Table) ", input$country, ", ", tolower(input$flow),  " of fishery and aquaculture products (", input$year, ")")
                           }
                           ),
                    list(extend = 'excel', 
                         filename =
                           if (input$species_choice == 'Yearbook/SOFIA Selection') {
                             paste0("(Table) ", input$country, ", ", tolower(input$flow),  " of ", tolower(data_yearbook[data_yearbook$name_yearbook_selection == input$yearbook_selection,]$name_yearbook_selection[[1]]), " (", input$year, ")")
                           } else if (input$species_choice == 'ISSCAAP Division') {
                             paste0("(Table) ", input$country, ", ", tolower(input$flow),  " of ", tolower(data_division[data_division$conc_isscaap_division == input$isscaap_division,]$name_isscaap_division[[1]]), " (", input$year, ")")
                           } else if (input$species_choice == 'ISSCAAP Group') {
                             paste0("(Table) ", input$country, ", ", tolower(input$flow),  " of ", tolower(data_group[data_group$conc_isscaap_group == input$isscaap_group,]$name_isscaap_group[[1]]), " (", input$year, ")")
                           } else {
                             paste0("(Table) ", input$country, ", ", tolower(input$flow),  " of fishery and aquaculture products (", input$year, ")")
                           }
                           ),
                    list(extend = 'pdf', 
                         filename =
                           if (input$species_choice == 'Yearbook/SOFIA Selection') {
                             paste0("(Table) ", input$country, ", ", tolower(input$flow),  " of ", tolower(data_yearbook[data_yearbook$name_yearbook_selection == input$yearbook_selection,]$name_yearbook_selection[[1]]), " (", input$year, ")")
                           } else if (input$species_choice == 'ISSCAAP Division') {
                             paste0("(Table) ", input$country, ", ", tolower(input$flow),  " of ", tolower(data_division[data_division$conc_isscaap_division == input$isscaap_division,]$name_isscaap_division[[1]]), " (", input$year, ")")
                           } else if (input$species_choice == 'ISSCAAP Group') {
                             paste0("(Table) ", input$country, ", ", tolower(input$flow),  " of ", tolower(data_group[data_group$conc_isscaap_group == input$isscaap_group,]$name_isscaap_group[[1]]), " (", input$year, ")")
                           } else {
                             paste0("(Table) ", input$country, ", ", tolower(input$flow),  " of fishery and aquaculture products (", input$year, ")")
                           }
                           )),
                  text = 'Download'
                )),
                pageLength = 15, 
                lengthMenu = c(10,50,100)
              ),
              class = "display",
              caption = 
                if (input$species_choice == 'Yearbook/SOFIA Selection') {
                  paste0(input$country, ", ", tolower(input$flow),  " of ", tolower(data_yearbook[data_yearbook$name_yearbook_selection == input$yearbook_selection,]$name_yearbook_selection[[1]]), " (", input$year, ")")
                } else if (input$species_choice == 'ISSCAAP Division') {
                  paste0(input$country, ", ", tolower(input$flow),  " of ", tolower(data_division[data_division$conc_isscaap_division == input$isscaap_division,]$name_isscaap_division[[1]]), " (", input$year, ")")
                } else if (input$species_choice == 'ISSCAAP Group') {
                  paste0(input$country, ", ", tolower(input$flow),  " of ", tolower(data_group[data_group$conc_isscaap_group == input$isscaap_group,]$name_isscaap_group[[1]]), " (", input$year, ")")
                } else {
                  paste0(input$country, ", ", tolower(input$flow),  " of fishery and aquaculture products (", input$year, ")")
                },
              colnames = c("Partner country", "Year", "Flow type", "Value", "Unit", "Share (%)")) %>%
      formatRound(c("share"), 1) %>%
      formatCurrency("value", currency = "", interval = 3, mark = " ", digits = 0)
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server, enableBookmarking = "url")