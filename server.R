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
  
  progress <- shiny::Progress$new()
  on.exit(progress$close())
  progress$set(message = "Plotting",detail = "Max plotting time 
               ~5 seconds", value = 1000)
  
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
  province <- if(is.null(input$country_df2_rows_selected) || any(input$country_df2_rows_selected == 1)) "all" else if(input$country_df2_rows_selected == 2) 1 else input$country_df2_rows_selected - 1
  
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
    if(any(input$country_df_rows_selected %in% 1:2)){
      leafletProxy("country_map")
    } else {
      country_shp <- shp1[shp1$NAME_0 == input$country, ]
      polygon_add <- gUnaryUnion(country_shp[input$country_df_rows_selected - 2, ])
      leafletProxy("country_map") %>% clearGroup("country_outline") %>%
        addPolylines(data = polygon_add, fill = F, weight = 5, color = "#FF6347", group = "country_outline", opacity = 1)
    }
  })
  
  observeEvent(input$endemic_df_rows_selected, {
    if(any(input$endemic_df_rows_selected < 4)){
      leafletProxy("endemic_map")
    } else {
      polygon_add <- gUnaryUnion(shp0[input$endemic_df_rows_selected - 4, ])
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
    
    country_shp <- shp1[shp1$NAME_0 == input$country, ]
    if(!is.null(input$country_df_rows_selected)){
      polygon_add <- gUnaryUnion(country_shp[input$country_df_rows_selected - 2, ])
      
      country_map_gen(shp1 = shp1,
                      country_of_interest = input$country,
                      year_of_interest = input$year1,
                      ages_of_interest = isolate(input$age))  %>%
        addPolylines(data = polygon_add, fill = F, weight = 5, color = "#FF6347", group = "country_outline", opacity = 1)
      
    } else {
      country_map_gen(shp1 = shp1,
                      country_of_interest = input$country,
                      year_of_interest = input$year1,
                      ages_of_interest = isolate(input$age)) 
    }
    
    })
  })
  
  observeEvent(input$update_range2, {
    output$endemic_map <- renderLeaflet({res = 300
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
  
  # observeEvent(input$year1, {
  #   updateSelectInput(session, "year2", selected = input$year1)
  # })
  # observeEvent(input$year2, {
  #   updateSelectInput(session, "year1", selected = input$year2)
  # })
  
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
  
  
  observeEvent(input$update_range2,{
    output$endemic_df <- DT::renderDataTable({
      endemic_df_gen(shp1 = shp1,
                     year_of_interest = input$year3,
                     ages_of_interest = isolate(input$age2))
      
    })
  })
  
  observeEvent(input$update_range,{
    output$country_df <- DT::renderDataTable({
      country_df_gen(shp1 = shp1,
                     country_of_interest = input$country,
                     year_of_interest = input$year1,
                     ages_of_interest = isolate(input$age))
    })
  })
  
  
  observeEvent(input$year1,{
    selectRows(country_df_proxy, input$country_df_rows_selected)
  })
  
  observeEvent(input$year2,{
    selectRows(country_df2_proxy, input$country_df2_rows_selected)
  })
  
  observeEvent(input$year,{
    selectRows(endemic_df_proxy, input$endemic_df_rows_selected)
  })
  

  '~~~~~~~~~~~~~ Save maps and dataframes ~~~~~~~~~~~~~'
  output$country_map_download <- downloadHandler(
    filename = function() {
      paste(input$country ," - ", "ages ", input$age[1], " to ", input$age[2], " (", input$year1, ').png', sep = '')
    },
    content = function(file) {
      png(file, width = 12, height = 8.5, units = 'in', res = 300, bg = 'white')
      par(mar = c(0, 0, 2, 4), oma = c(0, 0, 2, 4))
      country_map_gen_download(shp1, input$country, input$year1, input$age)
      dev.off()
    }
  )
  
  output$endemic_map_download <- downloadHandler(
    filename = function() {
      paste("Endemic_zone - ", "ages ", input$age[1], " to ", input$age[2], " (", input$year1, ').png', sep = '')
    },
    content = function(file) {
      png(file, width = 12, height = 7, units = 'in', res = 300, bg = 'white')
      par(mar = c(0, 0, 0, 6), oma = c(0, 0, 0, 4))
      endemic_map_gen_download(shp1, input$year3, input$age2)
      dev.off()
    }
  )
  
  output$country_df_download <- downloadHandler(
    filename = paste(input$country, " - ", "ages ", input$age[1], " to ", input$age[2], " (", input$year1, ').csv', sep = ""),
    content = function(filename) {
      this_df <- flat_coverage_pop(shp1[shp1$NAME_0 == input$country, ], input$year1, input$age[1], input$age[2])
      this_df <- this_df[, c("ISO", "adm1_id", "adm1_name", "vc", "pop")]
      colnames(this_df) <- c("ISO", "ID", "Province", "Coverage (%)", "Population")
      this_df[, 4] <- round(this_df[, 4] * 100, 1)
      write.csv(this_df, filename, row.names = FALSE)
    }
  )
  
  output$endemic_df_download <- downloadHandler(
    filename = paste("Endemic_zone - ", "ages ", input$age[1], " to ", input$age[2], " (", input$year1, ').csv', sep = ""),
    content = function(filename) {
      this_df <- flat_coverage_pop_endemic(shp1, input$year1, input$age[1], input$age[2])
      this_df <- this_df[, c("ISO", "adm0_name", "vc", "pop")]
      colnames(this_df) <- c("ISO", "Country", "Coverage (%)", "Population")
      this_df[, 3] <- round(this_df[, 3] * 100, 1)
      write.csv(this_df, filename, row.names = FALSE)
    }
  )
  
  
  
  '~~~~~~~~~~~~~ Tooltips ~~~~~~~~~~~~~'
  
  #Tooltips
  #Country
  addPopover(session, "country", "", content = paste0("Select country of interest."), 
             placement = "right", trigger = "hover")
  
  addPopover(session, "country2", "", content = paste0("Select country of interest."), 
             placement = "right", trigger = "hover")
  
  #Year
  addPopover(session, "year1", "", content = paste0("Select year."), 
             placement = "right", trigger = "hover")
  
  addPopover(session, "year2", "", content = paste0("Select year."), 
             placement = "right", trigger = "hover")
  
  addPopover(session, "year3", "", content = paste0("Select year."), 
             placement = "right", trigger = "hover")
  
  #Age rane
  addPopover(session, "age", "", content = paste0("Select age range and click update range."), 
             placement = "right", trigger = "hover")
  
  addPopover(session, "age2", "", content = paste0("Select age range and click update range."), 
             placement = "right", trigger = "hover")
  
  #Reset select rane
  addPopover(session, "resetSelection1", "", content = paste0("Reset the rows selected on the dataframe."), 
             placement = "right", trigger = "hover")
  
  addPopover(session, "resetSelection2", "", content = paste0("Reset the rows selected on the dataframe."), 
             placement = "right", trigger = "hover")
  
  addPopover(session, "resetSelection3", "", content = paste0("Reset the rows selected on the dataframe."), 
             placement = "right", trigger = "hover")
  
  #Reset select rane
  addPopover(session, "update_range", "", content = paste0("Update the age range plotted."), 
             placement = "right", trigger = "hover")
  
  addPopover(session, "update_range2", "", content = paste0("Update the age range plotted."), 
             placement = "right", trigger = "hover")
  
  #Country table
  addPopover(session, "country_df", "", content = paste0("Select provinces to display", " on the map and in graphs."), 
             placement = "bottom", trigger = "hover", options = list(container = 'body', width = 500))
  
  addPopover(session, "country_df2", "", content = paste0("Select provinces to display", " on the map and in graphs."), 
             placement = "bottom", trigger = "hover", options = list(container = 'body', width = 500))
  
  #Endemic zone table
  addPopover(session, "endemic_df", "", content = paste0("Select countries to display", " on the map and in graphs."), 
             placement = "bottom", trigger = "hover", options = list(container = 'body', width = 500))
  #Map
  addPopover(session, "country_map", "", content = paste0("This map shows vaccination coverage", " at the first administrative level."), 
             placement = "right", trigger = "hover", options = NULL)
  #Barchart
  addPopover(session, "barplot", "", content = paste0("This graph shows the total population of each 5 year", " age band by vaccination status.", 
                                                      "</p><p> Select provinces from table to display values.", 
                                                      " By default the whole countries values are shown.",
                                                      "</p><p> To download click the camera icon in the plotly legend."), 
             placement = "right", trigger = "hover", options = NULL)
  #Multiple lines
  addPopover(session, "linegraph", "", content = paste0("This graph shows the vaccination coverage across age.", 
                                                        "</p><p> Select provinces from table to display values.", 
                                                        " By default the country average is shown.",
                                                        "</p><p> To download click the camera icon in the plotly legend."), 
             placement = "right", trigger = "hover", options = NULL)
  
  addPopover(session, "endemic_map", "", content = paste0("This map shows vaccination coverage", " at the first administrative level across the endemic zone.", 
                                                          "</p><p> Due to the size it may take around 5 seconds to load."), 
             placement = "right", trigger = "hover", options = NULL)
  
  #Download maps
  addPopover(session, "country_map_download", "", content = paste0("Download the country level map."), 
             placement = "right", trigger = "hover")
  
  addPopover(session, "endemic_map_download", "", content = paste0("Download the endemic zone map."), 
             placement = "right", trigger = "hover")
  
  #Download datatables
  addPopover(session, "country_df_download", "", content = paste0("Download the country level table"), 
             placement = "right", trigger = "hover")
  
  addPopover(session, "endemic_df_download", "", content = paste0("Download the endemic zone table"), 
             placement = "right", trigger = "hover")
  
  
  
})