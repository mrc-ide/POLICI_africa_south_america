# source("load_data.R")

shinyServer(function(input, output, session){
  
  options(shiny.sanitize.errors = FALSE)
  
  '~~~~~~~~~~~~~~~~~~~~~~~ Plotting maps  ~~~~~~~~~~~~~~~~~~~~~~~'
  
  'Create country maps'
  #Plot map
  output$country_map <- renderLeaflet({res = 300
  country_map_gen(shp1 = shp1,
                  country_of_interest = input$country,
                  year_of_interest = input$year1,
                  ages_of_interest = isolate(input$age))
  })
  
  'Create endemic zone'
  output$endemic_map <- renderLeaflet({res = 300
  map_made<-endemic_map_gen(shp1 = shp1,
                            year_of_interest = input$year3,
                            ages_of_interest = isolate(input$age2))
  map_made
  
  })
  
  '~~~~~~~~~~~~~~~~~~~~~~~ Plotting graphs ~~~~~~~~~~~~~~~~~~~~~~~'
  
  'Create barplot'
  output$barplot <- renderLeaflet({res = 300
  
  #Subset to country, year and ages of interes
  country_of_interest <- input$country2
  year_of_interest <- input$year2
  
  #Generate df of coverage
  province <- "all"#if(is.null(input$country_df2_rows_selected) || any(input$country_df2_rows_selected == 1)) "all" else input$country_df2_rows_selected - 1

  plot_data <- coverage_by_age_aggregated(country = unique(shp1[shp1$NAME_0 == country_of_interest, ]$ISO),
                                          province = province,
                                          year = year_of_interest)
  print(plot_data)
  
  #Plot map
  plot_age_vc_barplot(plot_data)
  
  })
  
  
  'Create linegraph'
  output$linegraph <- renderLeaflet({res = 300
  
  #Subset to country, year and ages of interes
  country_of_interest <- input$country2
  year_of_interest <- input$year2
  
  #Generate df of coverage
  plot_data <- coverage_by_age(country = unique(shp1[shp1$NAME_0 == country_of_interest, ]$ISO),
                               year = year_of_interest)
  #Plot map
  province <- if(is.null(input$country_df2_rows_selected) || any(input$country_df2_rows_selected == 1)) "all" else input$country_df2_rows_selected - 1

  plot_age_vc_linegraph(plot_data, province)
  
  })
  
  
  '~~~~~~~~~~~~~~~~~~~~~~~ Creating dataframes ~~~~~~~~~~~~~~~~~~~~~~~'
  
  
  'Create data.table for country level maps'
  output$country_df <- DT::renderDataTable({
    country_df_gen(shp1 = shp1,
                   country_of_interest = input$country,
                   year_of_interest = input$year1,
                   ages_of_interest = isolate(input$age))
  })
  
  'Create data.table for country level age exploration'
  output$country_df2 <- DT::renderDataTable({
    country_df_gen(shp1 = shp1,
                   country_of_interest = input$country2,
                   year_of_interest = input$year2,
                   ages_of_interest = c(0, 100))
  })
  
  'Create data.table for endemic zone level'
  output$endemic_df <- DT::renderDataTable({
    endemic_df_gen(shp1 = shp1,
                   year_of_interest = input$year3,
                   ages_of_interest = isolate(input$age2))
    
  })
  
  
  '~~~~~~~~~~~~~~~~~~~~~~~ Proxy objects and observing events ~~~~~~~~~~~~~~~~~~~~~~~'
  '~~~~~~~~~~~~~~~~~~~~~~~ Map proxy ~~~~~~~~~~~~~~~~~~~~~~~'
  observeEvent(input$country_df_rows_selected, {
    if(input$country_df_rows_selected %in% 1:2){
      leafletProxy("country_map")
    } else {
      country_shp <- shp1[shp1$NAME_0 == input$country, ]
      polygon_add <- gUnaryUnion(country_shp[input$country_df_rows_selected - 2, ])
      leafletProxy("country_map") %>% clearGroup("country_outline") %>%
        addPolylines(data = polygon_add, fill = F, weight = 5, color = "#FF6347", group = "country_outline", opacity = 1)
    }
  })
  
  observeEvent(input$endemic_df_rows_selected, {
    if(input$endemic_df_rows_selected %in% 1:2){
      leafletProxy("endemic_map")
    } else {
      polygon_add <- gUnaryUnion(shp0[input$endemic_df_rows_selected -2, ])
      leafletProxy("endemic_map") %>% clearGroup("endemic_outline") %>% 
        addPolylines(data = polygon_add, fill = F, weight = 5, color = "#FF6347", group = "endemic_outline", opacity = 1)
    }
  })
  
  observeEvent(input$resetSelection1, {
    leafletProxy("country_map") %>% clearGroup("country_outline")
  })
  
  observeEvent(input$resetSelection3, {
    leafletProxy("endemic_map") %>% clearGroup("endemic_outline")
  })
  
  
  #Observe clicking the update age range button
  #Plot map
  observeEvent(input$update_range, {
    output$country_map <- renderLeaflet({res = 300
    country_map_gen(shp1 = shp1,
                    country_of_interest = input$country,
                    year_of_interest = input$year1,
                    ages_of_interest = isolate(input$age))
    })
  })

  observeEvent(input$update_range2, {
    output$country_map <- renderLeaflet({res = 300
    map_made<-endemic_map_gen(shp1 = shp1,
                              year_of_interest = input$year3,
                              ages_of_interest = isolate(input$age2))
    map_made
    })
  })
  
  '~~~~~~~~~~~~~ Update inputs based on other inputs ~~~~~~~~~~~~~'
  observeEvent(input$country, {
    updateSelectInput(session, "country2", selected = input$country)
  })
  observeEvent(input$country2, {
    updateSelectInput(session, "country", selected = input$country2)
  })
  
  observeEvent(input$year1, {
    updateSelectInput(session, "year2", selected = input$year1)
  })
  observeEvent(input$year2, {
    updateSelectInput(session, "year1", selected = input$year2)
  })

  '~~~~~~~~~~~~~~~~~~~~~~~ Datatable proxy ~~~~~~~~~~~~~~~~~~~~~~~'
  country_df_proxy <- dataTableProxy('country_df')
  country_df2_proxy <- dataTableProxy('country_df2')
  endemic_df_proxy <- dataTableProxy('endemic_df')
  
  observeEvent(input$resetSelection1,{
    selectRows(country_df_proxy, NULL)
  })
  
  observeEvent(input$resetSelection2,{
    selectRows(country_df2_proxy, NULL)
  })
  
  observeEvent(input$resetSelection3,{
    selectRows(endemic_df_proxy, NULL)
  })
  

})