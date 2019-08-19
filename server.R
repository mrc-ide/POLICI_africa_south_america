
shinyServer(function(input, output, session){
  
  options(shiny.sanitize.errors = FALSE)
  
  'Create country maps'
  output$country_map<-renderLeaflet({res = 300
  
  #Subset to country, year and ages of interes
  country_of_interest<-input$country
  year_of_interest<-input$year
  ages_of_interest<-input$age
  country_shape<-shp1[shp1$ISO == country_of_interest, ]
  
  #Generate df of coverage
  plot_data<-flat_coverage_pop(shp_file = country_shape, 
                               year = year_of_interest, 
                               min_age = ages_of_interest[1],
                               max_age = ages_of_interest[2])
  
  #Plot map
  plot_map_function(plot_data, country_shape)
  
  })
  
  'Create endemic zone'
  output$endemic_map<-renderLeaflet({res = 300
  
  #Subset to country, year and ages of interes
  year_of_interest<-input$year
  ages_of_interest<-input$age

  #Generate df of coverage
  plot_data<-flat_coverage_pop(shp_file = shp_file, 
                               year = year_of_interest, 
                               min_age = ages_of_interest[1],
                               max_age = ages_of_interest[2])
  
  #Plot map
  plot_map_function(plot_data, country_shape)
  
  })
  
  
  
  'Create barplot'
  
  'Create linegraph'
  
  'Create data.table for country level'
  
  'Create data.table for endemic zone level'
  
  
})