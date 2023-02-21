library(shiny)
library(highcharter)
library(dplyr)
library(tidyr)
library(tibble)
library(readr)
library(DT)

# library(rsconnect)
# deployApp()

source("helpers.R")

ui <- function(request){navbarPage("FishStat Trade Data",
        tabPanel("Home",
         fluidPage(
            mainPanel(
              h1("Welcome"),
              p("This website features interactive visualizations to explore FAO's", a(href="https://www.fao.org/fishery/en/collection/global_commodity_prod?lang=en", "Global Fish Trade by Partner Country", target="_blank"), "dataset."),
              p("In the", strong("Global Overview"), "section, you will find a visualization of the intercontinental trade of fish products. Go ahead and use the", em("Country highlight"), "filter to see how your country trades with the rest of the world. The", em("Heatmap"), "tab displays this information as a heatmap."),
              p("In the", strong("Country Overview"), "section, you can explore in details how the country of your choice trades with its partner countries around the world. The data is represented on a map, but you can also see more details about the data by clicking on the", em("Table"), "tab. Finally, see a chart of the selected country's main trading partners in the", em("Chart"), "tab."),
              p("We hope you enjoy this website. Click ", a(href="https://www.fao.org/fishery/en/statistics", "here", target="_blank"), "if you want to learn more about FAO's Fisheries and Aquaculture statistics."),
              h2("Notes"),
              p("Differences between figures given for total exports and total imports of any one commodity may be due to several factors, e.g. the time lapse between the dispatch of goods from the exporting country and their arrival in the importing country; the use of a different classification of the same product by different countries; or the fact that some countries supply trade data on general trade, while others give data on special trade."),
              p("Created by", a(href='https://pamdx.github.io/personal_site/index.html', 'Pierre Maudoux', target='_blank'), "(FAO). See the", a(href='https://github.com/pamdx/fishstat_trade_viz', 'source code', target='_blank'), "on GitHub.")
            )
         )
       ),
        tabPanel("Global Overview",
          sidebarLayout(
            sidebarPanel(
              selectInput('flow_global','Trade flow', choices = unique(edata$trade_flow)), 
              selectInput('year_global','Year', choices = unique(edata$year)),
              selectInput('country_highlight','Country highlight (optional)', choices = c("(None)", unique(edata$reporting_country)), selected = "China"),
              radioButtons('radio_global', 'Filter by species group', choices=c('Yes', 'No'), selected = "No", inline = TRUE),
              conditionalPanel(
                condition = "input.radio_global == 'Yes'",
                uiOutput('isscaap_group_global'),
                helpText("Click ", a(href="https://www.fao.org/fishery/static/ASFIS/ISSCAAP.pdf", "here", target="_blank"), " for more information about the ISSCAAP classification.")
              ),
              width=2
            ),
            mainPanel(
              tabsetPanel(
                tabPanel("Intercontinental trade", highchartOutput('dependencywheel', height = "800px")),
                tabPanel("Heatmap", highchartOutput('heatmap', height = "700px"))
              )
            )
          )
        ),
        tabPanel("Country Overview",
          sidebarLayout(
            sidebarPanel(
              selectInput('country','Country', choices = unique(edata$reporting_country), selected = sample(unique(edata$reporting_country), 1)),
              selectInput('flow_country','Trade flow', choices = unique(edata$trade_flow)), 
              selectInput('year_country','Year', choices = unique(edata$year)),
              radioButtons('radio_country', 'Filter by species group', choices=c('Yes', 'No'), selected = "No", inline = TRUE),
              conditionalPanel(
                condition = "input.radio_country == 'Yes'",
                uiOutput('isscaap_group_country'),
                helpText("Click ", a(href="https://www.fao.org/fishery/static/ASFIS/ISSCAAP.pdf", "here", target="_blank"), " for more information about the ISSCAAP classification.")
              ),
              width=2
            ),
            mainPanel(
              tabsetPanel(
                tabPanel("Map", highchartOutput('eqmap', height = "700px")),
                tabPanel("Chart", highchartOutput("chart", height = "700px")),
                tabPanel("Table", DT::dataTableOutput("data_table", height = "700px"))
              )
            )
          )
        )
      )
}

server <- function(input, output, session) {
  
  # Initialize conditional species filters
  
  output$isscaap_group_global<-renderUI({
    selectInput('isscaap_group_global','ISSCAAP Group', choices = unique(sort(edata$conc_isscaap_group)), selected = sample(unique(edata$conc_isscaap_group), 1), multiple = FALSE)
  })
  
  output$isscaap_group_country<-renderUI({
    selectInput('isscaap_group_country','ISSCAAP Group', choices = unique(sort(edata$conc_isscaap_group)), selected = sample(unique(edata$conc_isscaap_group), 1), multiple = FALSE)
  })
  
  # Global overview
  
  data_global <- reactive(edata %>%
                            filter(year == input$year_global) %>%
                            filter(trade_flow == input$flow_global) %>%
                            {if (input$radio_global == 'Yes') filter(., conc_isscaap_group %in% input$isscaap_group_global) else .} %>%
                            mutate(reporting_continent = ifelse(reporting_country %in% input$country_highlight, reporting_country, reporting_continent)) %>%
                            mutate(partner_continent = ifelse(partner_country %in% input$country_highlight, partner_country, partner_continent)) %>%
                            group_by(reporting_continent, partner_continent, unit, year, trade_flow) %>%
                            summarise(weight = sum(value)) %>%
                            ungroup() %>%
                            mutate(weight_formatted = addUnits(weight)) %>%
                            rename(from = reporting_continent, to = partner_continent) %>%
                            select(from, to , weight, weight_formatted)
  )
  
  output$dependencywheel <- renderHighchart(
    highchart() %>%
      hc_chart(type = 'dependencywheel') %>%
      hc_add_series(
        data = data_global(), 
        name = paste(input$flow_global, "flows (USD)"),
        dataLabels = JS("{
            color: '#333',
            textPath: {
                enabled: true,
                attributes: {
                    dy: 5
                }
            },
            distance: 7
        }"),
        tooltip = list(nodeFormat = paste('{point.name}: <b>{point.sum:,.0f}</b><br/>'),
                       pointFormat = paste('{point.fromNode.name}', if_else(input$flow_global == "Imports", "←", "→"),'{point.toNode.name}: <b>{point.weight_formatted}</b><br/>'))) %>%
      hc_caption(text = "Note: trade flows that start and end in the same continent (e.g. Europe → Europe) show the size of that continent's internal market (total trade among its countries). The 'Others' category refers to unspecified partners.") %>%
      hc_exporting(enabled = TRUE, 
                   buttons = list(
                     contextButton = list(
                       menuItems = hc_export_options
                     )
                   )
      )
  )
  
  output$heatmap <- renderHighchart({
    hchart(data_global(),
           "heatmap", 
           hcaes(x = to, y = from, value = weight),
           name = paste(input$flow_global, "flows (USD)"),
           tooltip = list(pointFormat = paste('{point.from}', if_else(input$flow_global == "Imports", "←", "→"),'{point.to}: <b>{point.weight:,.0f}</b><br/>'))
    ) %>%
      hc_xAxis(title = list(text = "Partner continent"), opposite = TRUE) %>%
      hc_yAxis(title = list(text = "Reporting continent"), reversed = TRUE) %>%
      hc_caption(text = "Note: trade flows that start and end in the same continent (e.g. Europe → Europe) show the size of that continent's internal market (total trade among its countries). The 'Others' category refers to unspecified partners.") %>%
      hc_exporting(enabled = TRUE, 
                   buttons = list(
                     contextButton = list(
                       menuItems = hc_export_options
                     )
                   )
      )
  })
  
  # Country overview
  
  data <- reactive(edata %>% 
                     filter(year == input$year_country) %>%
                     filter(reporting_country == input$country) %>%
                     filter(trade_flow == input$flow_country) %>%
                     {if (input$radio_country == 'Yes') filter(., conc_isscaap_group %in% input$isscaap_group_country) else .} %>%
                     rename(z = value) %>%
                     group_by_at(vars(-commodity_isscaap_group, -name_isscaap_group, -conc_isscaap_group, -z)) %>%
                     summarize(z = sum(z)) %>%
                     ungroup() %>%
                     group_by() %>%
                     mutate(total = sum(z)) %>%
                     ungroup() %>%
                     mutate(z_formatted = addUnits(z)) %>%
                     mutate(share = sprintf("%0.1f%%", z/total*100))
                   )
  
  data_reporting <- reactive(edata %>% 
                               filter(year == input$year_country) %>%
                               filter(reporting_country == input$country) %>%
                               filter(trade_flow == input$flow_country) %>%
                               rename(z = value) %>%
                               group_by(reporting_country, reporting_iso2, unit, year, trade_flow) %>%
                               summarise(z = sum(z)) %>%
                               inner_join(y = cou_coordinates, by = c("reporting_iso2" = "ISO2")) %>%
                               rename(lat = latitude, lon = longitude) %>%
                               mutate(z = replace(z, is.numeric(z), 1))
                             )
  
  data_total <- reactive(edata %>%
                           filter(year == input$year_country) %>%
                           filter(reporting_country == input$country) %>%
                           filter(trade_flow == input$flow_country) %>%
                           {if (input$radio_country == 'Yes') filter(., conc_isscaap_group %in% input$isscaap_group_country) else .} %>%
                           group_by(reporting_country) %>%
                           summarise(value = sum(value)) %>%
                           pull() %>%
                           addUnits()
                         )
  
  data_n <- reactive(edata %>%
                       filter(year == input$year_country) %>%
                       filter(reporting_country == input$country) %>%
                       filter(trade_flow == input$flow_country) %>%
                       {if (input$radio_country == 'Yes') filter(., conc_isscaap_group %in% input$isscaap_group_country) else .} %>%
                       group_by_at(vars(-commodity_isscaap_group, -name_isscaap_group, -conc_isscaap_group, -value)) %>%
                       summarize(value = sum(value)) %>%
                       ungroup() %>%
                       summarise(n = n()) %>%
                       pull()
  )

  output$eqmap <- renderHighchart(
    highchart(type = "map") %>%
      hc_add_series(mapData = map, showInLegend = F) %>%
      hc_add_series(data = data_reporting(), 
                    type = "mapbubble", 
                    name = if_else(input$flow_country == "Exports", "Origin of exports", "Destination of imports"), 
                    color = "#4daf4a",
                    minSize = "20",
                    maxSize = "20",
                    tooltip = list(pointFormat = "Country: {point.reporting_country}<br>Year: {point.year}")) %>%
      hc_add_series(data = data(), 
                    type = "mapbubble", 
                    name = if_else(input$flow_country == "Exports", "Destinations of exports", "Origin of imports"), 
                    color = "#377eb8",
                    tooltip = list(pointFormat = paste('Country: {point.partner_country}<br>Year: {point.year}<br>Value (USD): {point.z_formatted}<br>Share: {point.share}'))) %>%
      hc_title(text = paste0(input$country, ", ", tolower(input$flow_country),  " of ", ifelse(input$radio_country == 'No', "fishery and aquaculture products", tolower(edata[edata$conc_isscaap_group == input$isscaap_group_country,]$name_isscaap_group[[1]])), " (", input$year_country, ")")) %>%
      hc_subtitle(text = paste0('Total ', tolower(input$flow_country), ": ", "USD ", data_total(), ", number of partners: ", data_n())) %>%
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
                   )
      )
  )
  
  data_chart <- reactive(
    edata %>%
      filter(year == input$year_country) %>%
      filter(reporting_country == input$country) %>%
      filter(trade_flow == input$flow_country) %>%
      {if (input$radio_country == 'Yes') filter(., conc_isscaap_group %in% input$isscaap_group_country) else .} %>%
      group_by_at(vars(-commodity_isscaap_group, -name_isscaap_group, -conc_isscaap_group, -value)) %>%
      summarize(value = sum(value)) %>%
      ungroup() %>%
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
    hchart(data_chart(), 
           type = "column", 
           hcaes(x = partner_country, y = share), 
           name = paste("Share of", tolower(input$flow_country)),
           tooltip = list(pointFormat = "Country: {point.partner_country}<br>Year: {point.year}<br>Production (tonnes): {point.value_formatted}<br>Share: {point.share_pretty}")
    ) %>%
      hc_xAxis(title = list(text = "Partner country")) %>%
      hc_yAxis(title = list(text = paste("Share of", tolower(input$flow_country))),
               labels = list(format = "{value}%")) %>%
      hc_title(text = paste0(input$country, ", ", tolower(input$flow_country),  " of ", ifelse(input$radio_country == 'No', "fishery and aquaculture products", tolower(edata[edata$conc_isscaap_group == input$isscaap_group_country,]$name_isscaap_group[[1]])), " (", input$year_country, ")")) %>%
      hc_subtitle(text = paste0('Total ', tolower(input$flow_country), ": ", "USD ", data_total(), ", number of partners: ", data_n())) %>%
      hc_caption(text = "Note: the 'Others' category groups all partner countries with a share of trade lower than 1%, while 'Other nei' refers to unspecified partners.") %>%
      hc_exporting(enabled = TRUE, 
                   buttons = list(
                     contextButton = list(
                       menuItems = hc_export_options
                     )
                   )
      )
  })
  
  data_table <- reactive(
    edata %>%
      filter(year == input$year_country) %>%
      filter(reporting_country == input$country) %>%
      filter(trade_flow == input$flow_country) %>%
      {if (input$radio_country == 'Yes') filter(., conc_isscaap_group %in% input$isscaap_group_country) else .} %>%
      group_by_at(vars(-commodity_isscaap_group, -name_isscaap_group, -conc_isscaap_group, -value)) %>%
      summarize(value = sum(value)) %>%
      ungroup() %>%
      arrange(desc(value)) %>%
      group_by() %>%
      mutate(total = sum(value)) %>%
      ungroup() %>%
      mutate(share = value/total*100) %>%
      select(partner_country, year, trade_flow, value, unit, share)
    )
  
  output$data_table <- DT::renderDataTable(server = FALSE, { # server = FALSE used to make sure the entire dataset is downloaded when using the buttons
    datatable(data_table(),
              extensions = 'Buttons',
              options = list(
                paging = TRUE,
                searching = TRUE,
                fixedColumns = TRUE,
                autoWidth = FALSE,
                ordering = TRUE,
                dom = 'Bfrtip',
                buttons = c('copy', 'csv', 'excel', 'pdf'),
                pageLength = 10, 
                lengthMenu = c(10,50,100)
              ),
              class = "display",
              caption = paste0(input$country, ", ", tolower(input$flow_country),  " of ", ifelse(input$radio_country == 'No', "fishery and aquaculture products", tolower(edata[edata$conc_isscaap_group == input$isscaap_group_country,]$name_isscaap_group[[1]])), " (", input$year_country, ")"), 
              colnames = c("Partner country", "Year", "Flow type", "Value", "Unit", "Share (%)")) %>%
      formatRound(c("share"), 1) %>%
      formatCurrency("value", currency = "", interval = 3, mark = " ", digits = 0)
  })

  # Automatically bookmark every time an input changes
  observe({
    reactiveValuesToList(input)
    session$doBookmark()
  })
  # Update the query string
  onBookmarked(updateQueryString)
  
}

# Run the application 
shinyApp(ui = ui, server = server, enableBookmarking = "url")