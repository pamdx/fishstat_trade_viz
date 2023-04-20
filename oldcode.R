
       #  tabPanel("Home",
       #   fluidPage(
       #      mainPanel(
       #        h1("Welcome"),
       #        p("This website features interactive visualizations to explore FAO's", a(href="https://www.fao.org/fishery/en/collection/global_commodity_prod?lang=en", "Global Fish Trade by Partner Country", target="_blank"), "dataset."),
       #        p("In the", strong("Global Overview"), "section, you will find a visualization of the intercontinental trade of fish products. Go ahead and use the", em("Country highlight"), "filter to see how your country trades with the rest of the world. The", em("Heatmap"), "tab displays this information as a heatmap."),
       #        p("In the", strong("Country Overview"), "section, you can explore in details how the country of your choice trades with its partner countries around the world. The data is represented on a map, but you can also see more details about the data by clicking on the", em("Table"), "tab. Finally, see a chart of the selected country's main trading partners in the", em("Chart"), "tab."),
       #        p("We hope you enjoy this website. Click ", a(href="https://www.fao.org/fishery/en/statistics", "here", target="_blank"), "if you want to learn more about FAO's Fisheries and Aquaculture statistics."),
       #        h2("Notes"),
       #        p("Differences between figures given for total exports and total imports of any one commodity may be due to several factors, e.g. the time lapse between the dispatch of goods from the exporting country and their arrival in the importing country; the use of a different classification of the same product by different countries; or the fact that some countries supply trade data on general trade, while others give data on special trade."),
       #        p("Created by", a(href='https://pamdx.github.io/personal_site/index.html', 'Pierre Maudoux', target='_blank'), "(FAO). See the", a(href='https://github.com/pamdx/fishstat_trade_viz', 'source code', target='_blank'), "on GitHub.")
       #      )
       #   )
       # ),
       #  tabPanel("Global Overview",
       #    sidebarLayout(
       #      sidebarPanel(
       #        selectInput('flow_global','Trade flow', choices = unique(edata$trade_flow)), 
       #        selectInput('year_global','Year', choices = unique(edata$year)),
       #        selectInput('country_highlight','Country highlight (optional)', choices = c("(None)", unique(edata$reporting_country)), selected = "China"),
       #        radioButtons('radio_global', 'Filter by species group', choices=c('Yes', 'No'), selected = "No", inline = TRUE),
       #        conditionalPanel(
       #          condition = "input.radio_global == 'Yes'",
       #          uiOutput('isscaap_group_global'),
       #          helpText("Click ", a(href="https://www.fao.org/fishery/static/ASFIS/ISSCAAP.pdf", "here", target="_blank"), " for more information about the ISSCAAP classification.")
       #        ),
       #        width=2
       #      ),
       #      mainPanel(
       #        tabsetPanel(
       #          tabPanel("Intercontinental trade", highchartOutput('dependencywheel', height = "800px")),
       #          tabPanel("Heatmap", highchartOutput('heatmap', height = "700px"))
       #        )
       #      )
       #    )
       #  ),


       # output$isscaap_group_global<-renderUI({
  #   selectInput('isscaap_group_global','ISSCAAP Group', choices = unique(sort(edata$conc_isscaap_group)), selected = sample(unique(edata$conc_isscaap_group), 1), multiple = FALSE)
  # })

  # # Global overview
  # 
  # data_global <- reactive(edata %>%
  #                           filter(year == input$year_global) %>%
  #                           filter(trade_flow == input$flow_global) %>%
  #                           {if (input$radio_global == 'Yes') filter(., conc_isscaap_group %in% input$isscaap_group_global) else .} %>%
  #                           mutate(reporting_continent = ifelse(reporting_country %in% input$country_highlight, reporting_country, reporting_continent)) %>%
  #                           mutate(partner_continent = ifelse(partner_country %in% input$country_highlight, partner_country, partner_continent)) %>%
  #                           group_by(reporting_continent, partner_continent, unit, year, trade_flow) %>%
  #                           summarise(weight = sum(value)) %>%
  #                           ungroup() %>%
  #                           mutate(weight_formatted = addUnits(weight)) %>%
  #                           rename(from = reporting_continent, to = partner_continent) %>%
  #                           select(from, to , weight, weight_formatted)
  # )
  # 
  # output$dependencywheel <- renderHighchart(
  #   highchart() %>%
  #     hc_chart(type = 'dependencywheel') %>%
  #     hc_add_series(
  #       data = data_global(), 
  #       name = paste(input$flow_global, "flows (USD)"),
  #       dataLabels = JS("{
  #           color: '#333',
  #           textPath: {
  #               enabled: true,
  #               attributes: {
  #                   dy: 5
  #               }
  #           },
  #           distance: 7
  #       }"),
  #       tooltip = list(nodeFormat = paste('{point.name}: <b>{point.sum:,.0f}</b><br/>'),
  #                      pointFormat = paste('{point.fromNode.name}', if_else(input$flow_global == "Imports", "←", "→"),'{point.toNode.name}: <b>{point.weight_formatted}</b><br/>'))) %>%
  #     hc_caption(text = "Note: trade flows that start and end in the same continent (e.g. Europe → Europe) show the size of that continent's internal market (total trade among its countries). The 'Others' category refers to unspecified partners.") %>%
  #     hc_exporting(enabled = TRUE, 
  #                  buttons = list(
  #                    contextButton = list(
  #                      menuItems = hc_export_options
  #                    )
  #                  )
  #     )
  # )
  # 
  # output$heatmap <- renderHighchart({
  #   hchart(data_global(),
  #          "heatmap", 
  #          hcaes(x = to, y = from, value = weight),
  #          name = paste(input$flow_global, "flows (USD)"),
  #          tooltip = list(pointFormat = paste('{point.from}', if_else(input$flow_global == "Imports", "←", "→"),'{point.to}: <b>{point.weight:,.0f}</b><br/>'))
  #   ) %>%
  #     hc_xAxis(title = list(text = "Partner continent"), opposite = TRUE) %>%
  #     hc_yAxis(title = list(text = "Reporting continent"), reversed = TRUE) %>%
  #     hc_caption(text = "Note: trade flows that start and end in the same continent (e.g. Europe → Europe) show the size of that continent's internal market (total trade among its countries). The 'Others' category refers to unspecified partners.") %>%
  #     hc_exporting(enabled = TRUE, 
  #                  buttons = list(
  #                    contextButton = list(
  #                      menuItems = hc_export_options
  #                    )
  #                  )
  #     )
  # })