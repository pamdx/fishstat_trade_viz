library(shiny)
library(highcharter)
library(dplyr)
library(tidyr)
library(tibble)
library(readr)
library(DT)
library(shinyfullscreen)
library(shinycssloaders)

source("helpers.R")

ui <- function(request){
        fluidPage(
          conditionalPanel(
            condition = "output.show != 'map' && output.show != 'chart' && output.show != 'table'", style = "display: none;",
            navbarPage(title = a(href = "https://www.fao.org/fishery/en/collection/global_commodity_prod?lang=en", target = "_blank", style="text-decoration:none;color:inherit", div(img(src = "fao-logo-three-lines.svg", id = "logo", height = "35px", style = "border-right: 1px solid grey; padding: 0 0.5rem; position: relative; margin:-15px 0px; display:right-align; "), "FishStat Global Aquatic Trade")),
                       tabPanel("Data Explorer",
                                sidebarLayout(
                                  sidebarPanel(
                                    selectInput('country','Country or area', choices = c("Please select...", sort(unique(data_agg$reporting_country)))),
                                    selectInput('flow','Trade flow', choices = c("Please select...",sort(unique(data_agg$trade_flow)))),
                                    selectInput('year','Year', choices = sort(unique(data_agg$year), decreasing = TRUE)),
                                    selectInput('unit', 'Unit', choices = sort(unique(data_agg$unit), decreasing = TRUE)),
                                    selectInput('classification_choice', 'Filter by species group', choices = c('Disabled', 'Yearbook/SOFIA Selection', 'ISSCAAP Division', 'ISSCAAP Group'), selected = "Disabled"),
                                    conditionalPanel(
                                      condition = "input.classification_choice == 'Yearbook/SOFIA Selection'",
                                      uiOutput('yearbook_selection'),
                                      helpText("")
                                    ),
                                    conditionalPanel(
                                      condition = "input.classification_choice == 'ISSCAAP Division'",
                                      uiOutput('isscaap_division'),
                                      helpText("Click ", a(href = "https://www.fao.org/fishery/en/collection/asfis/en", "here", target = "_blank"), " for more information about the ISSCAAP classification.")
                                    ),
                                    conditionalPanel(
                                      condition = "input.classification_choice == 'ISSCAAP Group'",
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
                                        highchartOutput('countrymap', height = "850px") %>% withSpinner()
                                      ),
                                      tabPanel(
                                        "Chart", 
                                        highchartOutput("chart", height = "850px") %>% withSpinner()
                                      ),
                                      tabPanel(
                                        "Table", 
                                        DT::dataTableOutput("data_table", height = "850px") %>% withSpinner()
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
                       tabPanel("Instructions",
                                fluidPage(
                                  mainPanel(
                                    h1("How to use this tool"),
                                    p("This website features interactive visualizations to explore FAO's", a(href="https://www.fao.org/fishery/en/collection/global_commodity_prod?lang=en", "Global aquatic trade - By partner country", target="_blank"), "dataset."),
                                    p("We hope you enjoy this application. Click ", a(href="https://www.fao.org/fishery/en/fishstat", "here", target="_blank"), "if you want to learn more about FAO's Fisheries and Aquaculture statistics."),
                                    p("We encourage users to provide their feedback or ask their questions about this tool at", a(href="mailto:Fish-Statistics-Inquiries@fao.org", "Fish-Statistics-Inquiries@fao.org", target="_blank", .noWS = c('after')), "."),
                                    h2("The Data Explorer"),
                                    p("Under the", em("Data Explorer"), "tab, you can explore in detail how the country or territory of your choice trades with partners around the world."),
                                    h3("Side panel"),
                                    p("The", em("side panel"), "located on the left of the user interface allows you to filter the data to visualize on the right side of the interface. Using these filters, one can display the imports/exports flows towards/from a given country or territory for a specific year. The data can also be further filtered by species group (three species classifications are available). Finally, the two buttons on the bottom of the side panels allow the user to display the application in full screen and to share the current view with somebody else."), 
                                    tags$img(src = "side_panel.png"),
                                    h3("Map"),
                                    p("The data is first represented on a map under the", em("Map"), "tab. While the green bubble represents the reporting country/territory, the blue bubbles represent the reporting country/territory's trade flows with the rest of the world for the year selected in the side panel. Placing your cursor on individual bubbles will give you more information on a specific partner's trade with the reporting country/territory. You can zoom in or out of the map using the + and - buttons on the top left of the map. This is particularly useful to better explore data in areas of the world with a high density of countries. Finally, you can export the map as an image by clicking on the three lines on the top right of the map."), 
                                    tags$img(src = "map_illustration.png"),
                                    h3("Chart"),
                                    p("If you want to focus on the main partners for the reporting country/territory you selected, you can display them as a bar chart ordered by their shares of trade under the", em("Chart"), "tab. Placing your cursor on individual bars will give you more information on a given partner's trade with the reporting country/territory. You can export the visual as an image by clicking on the three lines on the top right of the chart."), 
                                    tags$img(src = "chart_illustration.png"),
                                    h3("Table"),
                                    p("Finally, you can display a table listing the partners for the reporting country/territory you selected in the", em("Table"), "tab. The data can be exported by clicking on any of the buttons on the top left of the table."),
                                    tags$img(src = "table_illustration.png"),
                                    h1("Notes"),
                                    p("Differences between figures given for total exports and total imports of any one commodity may be due to several factors, e.g. the time lapse between the dispatch of goods from the exporting country/territory and their arrival in the importing country/territory; the use of a different classification of the same product by different countries/territories; or the fact that some countries/territories supply trade data on general trade, while others give data on special trade."),
                                    h1("Map disclaimer"),
                                    p("The boundaries and names shown and the designations used on this map do not imply the expression of any opinion whatsoever on the part of FAO concerning the legal status of any country, territory, city or area or of its authorities, or concerning the delimitation of its frontiers and boundaries."),
                                    h1("License"),
                                    tags$img(src = "cc_by.png"),
                                    p(""),
                                    p("This work is made available under the Creative Commons Attribution-4.0 International licence (CC BY 4.0 ", a(href = "https://creativecommons.org/licenses/by/4.0/legalcode.en", "https://creativecommons.org/licenses/by/4.0/legalcode.en", target="_blank", .noWS = c('after')), "). By using this database, you agree to be bound by the terms of this license and the ", a(href = "https://www.fao.org/contact-us/terms/db-terms-of-use/en", "FAO Statistical Database Terms of Use", target="_blank", .noWS = c('after')), ".")
                                  )
                                )
                       )
            )
            
          ),
          conditionalPanel(condition = "output.show == 'map'", 
                           highchartOutput("countrymap_solo", height = "700px") %>% withSpinner()
          ),
          conditionalPanel(condition = "output.show == 'chart'", 
                           highchartOutput("chart_solo", height = "700px") %>% withSpinner()
          ),
          conditionalPanel(condition = "output.show == 'table'", 
                           DT::dataTableOutput("data_table_solo", height = "700px") %>% withSpinner()
          ),
          tags$head(
            tags$link(rel = "stylesheet", type = "text/css", href = "stylesheet.css")
          )
        )
}

server <- function(input, output, session) {
  
  # Parse query string to identify which elements should be shown (either everything or just one of the interactive visuals)
  
  observe({
    query <- parseQueryString(session$clientData$url_search)
    if (!is.null(query[['show']])) {
      output$show <- renderText({
        query[['show']]
      })
      outputOptions(output, "show", suspendWhenHidden = FALSE)
    }
  })
  
  # Initialize conditional species filters
  
  output$yearbook_selection <- renderUI({
    selectInput('yearbook_selection','Yearbook/SOFIA Selection', 
                choices = data_yearbook %>% filter(reporting_country == input$country, trade_flow == input$flow, year == input$year, unit == input$unit) %>% pull(name_yearbook_selection) %>% unique() %>% sort(), 
                selected = 'Aquatic animals', 
                multiple = FALSE)
  })
  
  output$isscaap_division <- renderUI({
    selectInput('isscaap_division','ISSCAAP Division', 
                choices = data_division %>% filter(reporting_country == input$country, trade_flow == input$flow, year == input$year, unit == input$unit) %>% pull(conc_isscaap_division) %>% unique() %>% sort(), 
                multiple = FALSE)
  })
  
  output$isscaap_group <- renderUI({
    selectInput('isscaap_group','ISSCAAP Group',
                choices = data_group %>% filter(reporting_country == input$country, trade_flow == input$flow, year == input$year, unit == input$unit) %>% pull(conc_isscaap_group) %>% unique() %>% sort(), 
                multiple = FALSE)
  })
  
  # Country overview
  
  data_partners <- reactive(switch(input$classification_choice, 
                          'Disabled' = data_agg, 
                          'Yearbook/SOFIA Selection' = data_yearbook, 
                          'ISSCAAP Division' = data_division, 
                          'ISSCAAP Group' = data_group) %>% 
                     filter(year == input$year) %>%
                     filter(reporting_country == input$country) %>%
                     filter(trade_flow == input$flow) %>%
                     filter(unit == input$unit) %>%
                     filter(partner_country != "Other NEI") %>% # doesn't make sense to map the location of the "Other NEI" country
                     {if (input$classification_choice == 'Yearbook/SOFIA Selection') filter(., name_yearbook_selection %in% input$yearbook_selection) 
                       else if (input$classification_choice == 'ISSCAAP Division') filter(., conc_isscaap_division %in% input$isscaap_division) 
                       else if (input$classification_choice == 'ISSCAAP Group') filter(., conc_isscaap_group %in% input$isscaap_group) 
                       else .} %>%
                     # filter(value > 0) %>% # better not to show bubbles when trade = 0
                     rename(z = value) %>%
                     group_by() %>%
                     mutate(total = sum(z)) %>%
                     ungroup() %>%
                     mutate(z_formatted = addUnits(z)) %>%
                     mutate(share = sprintf("%0.1f%%", z/total*100))
                   )
  
  data_reporting <- reactive(switch(input$classification_choice, 
                                    'Disabled' = data_agg, 
                                    'Yearbook/SOFIA Selection' = data_yearbook, 
                                    'ISSCAAP Division' = data_division, 
                                    'ISSCAAP Group' = data_group) %>% 
                               filter(year == input$year) %>%
                               filter(reporting_country == input$country) %>%
                               filter(trade_flow == input$flow) %>%
                               filter(unit == input$unit) %>%
                               rename(z = value) %>%
                               group_by(reporting_country, reporting_un_code, unit, year, trade_flow) %>%
                               summarise(z = sum(z)) %>%
                               inner_join(y = cou_coordinates, by = c("reporting_un_code" = "un_code")) %>%
                               mutate(z = replace(z, is.numeric(z), 1))
                             )
  
  data_total <- reactive(switch(input$classification_choice, 
                                'Disabled' = data_agg, 
                                'Yearbook/SOFIA Selection' = data_yearbook, 
                                'ISSCAAP Division' = data_division, 
                                'ISSCAAP Group' = data_group) %>% 
                           filter(year == input$year) %>%
                           filter(reporting_country == input$country) %>%
                           filter(trade_flow == input$flow) %>%
                           filter(unit == input$unit) %>%
                           {if (input$classification_choice == 'Yearbook/SOFIA Selection') filter(., name_yearbook_selection %in% input$yearbook_selection) 
                             else if (input$classification_choice == 'ISSCAAP Division') filter(., conc_isscaap_division %in% input$isscaap_division) 
                             else if (input$classification_choice == 'ISSCAAP Group') filter(., conc_isscaap_group %in% input$isscaap_group) 
                             else .} %>%
                           group_by(reporting_country) %>%
                           summarise(value = sum(value)) %>%
                           pull() %>%
                           addUnits()
                         )
  
  data_n <- reactive(switch(input$classification_choice, 
                            'Disabled' = data_agg, 
                            'Yearbook/SOFIA Selection' = data_yearbook, 
                            'ISSCAAP Division' = data_division, 
                            'ISSCAAP Group' = data_group) %>% 
                       filter(year == input$year) %>%
                       filter(reporting_country == input$country) %>%
                       filter(trade_flow == input$flow) %>%
                       filter(unit == input$unit) %>%
                       {if (input$classification_choice == 'Yearbook/SOFIA Selection') filter(., name_yearbook_selection %in% input$yearbook_selection) 
                         else if (input$classification_choice == 'ISSCAAP Division') filter(., conc_isscaap_division %in% input$isscaap_division) 
                         else if (input$classification_choice == 'ISSCAAP Group') filter(., conc_isscaap_group %in% input$isscaap_group) 
                         else .} %>%
                       summarise(n = n()) %>%
                       pull()
  )
  
  title <- reactive(
    if (input$classification_choice == 'Yearbook/SOFIA Selection') {
      paste0(input$country, ", ", tolower(input$flow),  " of ", tolower(data_yearbook[data_yearbook$name_yearbook_selection == input$yearbook_selection,]$name_yearbook_selection[[1]]), ", ", input$year)
    } else if (input$classification_choice == 'ISSCAAP Division') {
      paste0(input$country, ", ", tolower(input$flow),  " of ", tolower(data_division[data_division$conc_isscaap_division == input$isscaap_division,]$name_isscaap_division[[1]]), ", ", input$year)
    } else if (input$classification_choice == 'ISSCAAP Group') {
      paste0(input$country, ", ", tolower(input$flow),  " of ", tolower(data_group[data_group$conc_isscaap_group == input$isscaap_group,]$name_isscaap_group[[1]]), ", ", input$year)
    } else {
      paste0(input$country, ", ", tolower(input$flow),  " of fishery and aquaculture products, ", input$year)
    }
  )
  
  subtitle <- reactive(
    paste0('Total ', tolower(input$flow), " (", input$unit, "): ", data_total(), ", number of partners: ", data_n())
  )
  
  source = paste0("Source: FAO ", format(Sys.Date(), "%Y"), ". Global Aquatic Trade Statistics. In: Fisheries and Aquaculture. Rome. [Cited ", format(Sys.time(), "%A, %B %d %Y"), "]. https://www.fao.org/fishery/en/collection/global_commodity_prod")

  countrymap <- renderHighchart({
    
    validate(need(input$country != "Please select..." & input$flow != "Please select...", "
           
        ← To start, please select a country and a trade flow on the side panel."))
    
    highchart(type = "map") %>%
      hc_add_series(mapData = map, showInLegend = F) %>%
      hc_add_series(data = data_partners(), 
                    type = "mapbubble",
                    name = if_else(input$flow == "Exports", "Destinations of exports", "Origin of imports"), 
                    color = "#377eb8",
                    tooltip = list(pointFormat = paste0('Partner country or area: {point.partner_country}<br>Year: {point.year}<br>Value (',  input$unit,'): {point.z_formatted}<br>Share: {point.share}'))) %>%
      hc_add_series(data = data_reporting(), 
                    type = "mapbubble", 
                    name = if_else(input$flow == "Exports", "Origin of exports", "Destination of imports"), 
                    color = "#4daf4a",
                    minSize = "20",
                    maxSize = "20",
                    tooltip = list(pointFormat = "Country or area: {point.reporting_country}<br>Year: {point.year}")) %>%
      hc_title(text = title()) %>%
      hc_subtitle(text = subtitle()) %>%
      hc_mapNavigation(enabled = T) %>%
      hc_legend(enabled = TRUE, 
                # bubbleLegend = list(enabled = TRUE),
                layout = "horizontal", 
                align = "right",
                verticalAlign = "bottom"
                ) %>%
      hc_caption(text = paste("Note: 'Other nei' refers to unspecified partners.<br>", source)) %>%
      hc_exporting(enabled = TRUE, 
                   buttons = list(
                     contextButton = list(
                       menuItems = hc_export_options
                     )
                   ),
                   chartOptions = list(
                     chart = list(backgroundColor = "#FFFFFF"),
                     legend = list(bubbleLegend = list(enabled = TRUE))
                   ),
                   sourceWidth = 1920,
                   sourceHeight = 1080,
                   filename = paste0("(Map) ", title())
                   )
  })
  
  output$countrymap <- countrymap
  output$countrymap_solo <- countrymap # can't refer to the same output twice in the UI
  
  data_chart <- reactive(
    switch(input$classification_choice, 
           'Disabled' = data_agg, 
           'Yearbook/SOFIA Selection' = data_yearbook, 
           'ISSCAAP Division' = data_division, 
           'ISSCAAP Group' = data_group) %>%
      filter(year == input$year) %>%
      filter(reporting_country == input$country) %>%
      filter(trade_flow == input$flow) %>%
      filter(unit == input$unit) %>%
      {if (input$classification_choice == 'Yearbook/SOFIA Selection') filter(., name_yearbook_selection %in% input$yearbook_selection) 
        else if (input$classification_choice == 'ISSCAAP Division') filter(., conc_isscaap_division %in% input$isscaap_division) 
        else if (input$classification_choice == 'ISSCAAP Group') filter(., conc_isscaap_group %in% input$isscaap_group) 
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
    
  chart <- renderHighchart({
    
    validate(need(input$country != "Please select..." & input$flow != "Please select...", "
           
        ← To start, please select a country and a trade flow on the side panel."))
    
    hchart(data_chart(), 
           type = "column", 
           hcaes(x = partner_country, y = share), 
           name = paste("Share of", tolower(input$flow)),
           tooltip = list(pointFormat = paste0("Partner country or area: {point.partner_country}<br>Year: {point.year}<br>Value (",  input$unit, "): {point.value_formatted}<br>Share: {point.share_pretty}"))
    ) %>%
      hc_xAxis(title = list(text = NULL)) %>%
      hc_yAxis(title = list(text = paste("Share of", tolower(input$flow))),
               labels = list(format = "{value}%"),
               ceiling = 100) %>%
      hc_title(text = title()) %>%
      hc_subtitle(text = subtitle()) %>%
      hc_caption(text = paste("Note: the 'Others' category groups all partner countries with a share of", tolower(input$flow), "lower than 1%, while 'Other nei' refers to unspecified partners.<br>", source)) %>%
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
                   filename = paste0("(Chart) ", title())
      )
  })
  
  output$chart <- chart
  output$chart_solo <- chart # can't refer to the same output twice in the UI
  
  data_table <- reactive(
    switch(input$classification_choice, 
           'Disabled' = data_agg, 
           'Yearbook/SOFIA Selection' = data_yearbook, 
           'ISSCAAP Division' = data_division, 
           'ISSCAAP Group' = data_group) %>% 
      filter(year == input$year) %>%
      filter(reporting_country == input$country) %>%
      filter(trade_flow == input$flow) %>%
      filter(unit == input$unit) %>%
      {if (input$classification_choice == 'Yearbook/SOFIA Selection') filter(., name_yearbook_selection %in% input$yearbook_selection) 
        else if (input$classification_choice == 'ISSCAAP Division') filter(., conc_isscaap_division %in% input$isscaap_division) 
        else if (input$classification_choice == 'ISSCAAP Group') filter(., conc_isscaap_group %in% input$isscaap_group) 
        else .} %>%
      arrange(desc(value)) %>%
      group_by() %>%
      mutate(total = sum(value)) %>%
      ungroup() %>%
      mutate(share = value/total*100) %>%
      select(partner_country, year, trade_flow, value, unit, share) %>%
      add_row(partner_country = source) # add citation in the last row
    )
  
  interactive_table <- DT::renderDataTable(server = FALSE, { # server = FALSE used to make sure the entire dataset is downloaded when using the buttons
    
    validate(need(input$country != "Please select..." & input$flow != "Please select...", "
           
        ← To start, please select a country and a trade flow on the side panel."))
    
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
                         filename = paste0("(Table) ", title())),
                    list(extend = 'excel', 
                         filename = paste0("(Table) ", title())),
                    list(extend = 'pdf', 
                         filename = paste0("(Table) ", title()))),
                  text = 'Download'
                )),
                pageLength = 15, 
                lengthMenu = c(10,50,100)
              ),
              class = "display",
              caption = title(),
              colnames = c("Partner country or area", "Year", "Flow type", "Value", "Unit", "Share (%)")) %>%
      formatRound(c("share"), 1) %>%
      formatCurrency("value", currency = "", interval = 3, mark = " ", digits = 2)
  })
  
  output$data_table <- interactive_table
  output$data_table_solo <- interactive_table # can't refer to the same output twice in the UI
  
}

# Run the application 
shinyApp(ui = ui, server = server, enableBookmarking = "url")