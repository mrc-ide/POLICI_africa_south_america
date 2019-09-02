#' Function to plot interactive maps of vaccination coverage
#' 
#' @param vc_pop_df vaccination coverage data frame of caluclated risk for each province in a country
#' @param shp_file shape file for one country
#' @param country_outline adm0 for plotting the outlines of the country
#' @return leaflet of vaccination coverage map
#' @export

plot_map_function <- function(vc_pop_df, shp_file, country_outline){
  
  #Sort so in the same order
  shp_file <- shp_file[order(shp_file$SPID), ]
  vaccination <- if(any(names(vc_pop_df) == "adm1_id")) vc_pop_df[order(vc_pop_df$adm1_id), ] else vc_pop_df[order(vc_pop_df$ISO), ]
  
  non_zero_df <- vaccination[which(vaccination$vc != 0), ]
  no_zero_shp <- shp_file[shp_file$SPID %in% non_zero_df$adm1_id,  ]
  country_outline <- shp0[shp0$ISO %in% shp_file[shp_file$SPID %in% vaccination$adm1_id,  ]$ISO, ]
  
  #Creating the popup 
  popup <- paste0(
    "Country: ", no_zero_shp$NAME_0, " - ", 
    if(any(names(vc_pop_df) == "adm1_id")) non_zero_df$adm1_name else non_zero_df$adm0_name, " - ",
               "Population immunity: ", paste0(round(non_zero_df$vc*100, 1), "%"), " - ",
               "Population: ", formatC(non_zero_df$pop, format = "d", big.mark = ","))
  
  
  #Colour scheme for plotting
  numpal <- colorNumeric("YlGn", 0:100, na.color = "#808080", alpha = FALSE)
  
  leaflet(no_zero_shp, options = leafletOptions(preferCanvas = TRUE)) %>%
    addProviderTiles("Esri.WorldGrayCanvas", options = providerTileOptions(maxZoom=9, updateWhenIdle = FALSE, updateWhenZooming = FALSE)) %>%
    addPolygons(
      stroke = TRUE, fillOpacity = 1, smoothFactor = 1,
      fillColor = ~numpal(non_zero_df$vc*100), col="black",
      weight = 2, label = (popup)) %>%
    addLegend("bottomright", pal = numpal, values = 0:100,
              title = "Coverage (%)",
              opacity = 1, bins = 10, layerId = "map") %>% addPolylines(data = country_outline, 
                                                                        fill = F, weight = 2, 
                                                                        color = "black", 
                                                                        group = "country_outline", 
                                                                        opacity = 1)
  
}


#' Function to output interactive country-level maps to the server
#' 
#' @param country_of_interest country name of interest
#' @param year_of_interest year of interest
#' @param ages_of_interest ages of interest, vector of 2 the min and max ages
#' @return leaflet of vaccination coverage map
#' @export
#' 
country_map_gen<-function(shp1, country_of_interest, year_of_interest, ages_of_interest){
  
  #Subset to country, year and ages of interes
  country_shape <- shp1[shp1$NAME_0 == country_of_interest, ]
  
  #Generate df of coverage
  plot_data <- flat_coverage_pop(shp_file = country_shape, 
                                 year = year_of_interest, 
                                 min_age = ages_of_interest[1],
                                 max_age = ages_of_interest[2])
  
  #Plot map
  plot_map_function(plot_data, country_shape, country_outline)
  
}


#' Function to output interactive endemic-zone maps to the server
#' 
#' @param year_of_interest year of interest
#' @param ages_of_interest ages of interest, vector of 2 the min and max ages
#' @return leaflet of vaccination coverage map
#' @export
#' 
endemic_map_gen<-function(shp1, year_of_interest, ages_of_interest){
  
  #Generate df of coverage
  plot_data <- flat_coverage_pop(shp_file = shp1,
                                 year = year_of_interest,
                                 min_age = ages_of_interest[1],
                                 max_age = ages_of_interest[2])
  
  #Plot map
  plot_map_function(plot_data, shp1, country_outline)

}


#' Function to plot interactive aggregated barplots of vaccination coverage
#' 
#' @param vc_age output of the function coverage_by_age_aggregated
#' @return barplot of 5 year aggregated vaccination coverage
#' @export
#' 
plot_age_vc_barplot <- function(vc_age){
  
  #Set up axis for barplots
  xlab <- c("0-4", "5-9", "10-14", "15-19", "20-24", "25-29", "30-34", "35-39", "40-44", "45-49", "50-54", "55-59", 
          "60-64", "65-69", "70-74", "75-79", "80-84", 
          "85-89", "90-94", "95-99", "100")
  xlab <- base::factor(xlab, levels = xlab)
  
  p <- plot_ly(x = xlab, y = round(vc_age[, 1]), name = "Vaccinated", type = "bar", marker = list(color = toRGB("dodgerblue"))) 
  p2 <- add_trace(p, x = xlab, y = round(vc_age[, 2]), type = "bar", name = "Unvaccinated", marker = list(color = toRGB("tomato1")))
  p3 <- layout(p2, barmode = "stack")
  x <- list(title = "Age group")
  y <- list(title = "Population")
  
  p3 %>%  layout(xaxis = x, yaxis = y)
  
}


#' Function to plot interactive line graphs of vaccination coverage
#' 
#' @param vc_age output of the function coverage_by_age
#' @return line graph of vaccination coverage
#' @export
#' 
plot_age_vc_linegraph <- function(vc_age, province){
  
  #Set up the margins for plotting
  m <- list(l = 50, r = 50, b = 50, t = 50, pad = 4)
  
  #Transform data in a way that plotly likes for plotting
  if(province == "all"){
    vc_subset<-vc_age
  } else if(length(province) == 1){
    vc_subset<-matrix(vc_age[province, ], nrow = 1)
    row.names(vc_subset)<-row.names(vc_age)[province]
  } else {
    vc_subset<-vc_age[province, ]
  }
  
  updated_format<-do.call(rbind, sapply(1:nrow(vc_subset), function(x) data.frame(SPID = row.names(vc_subset)[x], vc = vc_subset[x, ], 
                                                                               age = 1:length(vc_subset[x, ]),
                                                                               stringsAsFactors = F), simplify = F))
  #Transform to %
  updated_format$vc<-updated_format$vc*100
  
  #Plot
  plot_ly(updated_format, x = updated_format$age, y = updated_format$vc, color= updated_format$SPID) %>% add_lines(y = updated_format$vc) %>%
    layout(xaxis = list(title = "Age", range = c(0, 100)), yaxis = list(title = "Vaccinated (%)",
                                                                    range = c(0, 100)), showlegend=TRUE, margin = m) 
  
  
}

#' Function to output downloadable country-level map
#' 
#' @param country_of_interest country name of interest
#' @param year_of_interest year of interest
#' @param ages_of_interest ages of interest, vector of 2 the min and max ages
#' @return leaflet of vaccination coverage map
#' @export
#' 
country_map_gen_download<-function(shp1, country_of_interest, year_of_interest, ages_of_interest){
  
  #Subset to country, year and ages of interes
  country_shape <- shp1[shp1$NAME_0 == country_of_interest, ]
  
  #Generate df of coverage
  plot_data <- flat_coverage_pop(shp_file = country_shape, 
                                 year = year_of_interest, 
                                 min_age = ages_of_interest[1],
                                 max_age = ages_of_interest[2])
  
  #Plot map
  save_name <- paste(country_of_interest, year_of_interest, ages_of_interest[1], ages_of_interest[2], sep = "_")
  
  plot_map_function_download(plot_data, country_shape, country_outline, save_name)
  
}

#' Function to output downloadable endemiczone map
#' 
#' @param year_of_interest year of interest
#' @param ages_of_interest ages of interest, vector of 2 the min and max ages
#' @return leaflet of vaccination coverage map
#' @export
#' 
endemic_map_gen_download<-function(shp1, year_of_interest, ages_of_interest){
  
  #Subset to country, year and ages of interes

  #Generate df of coverage
  plot_data <- flat_coverage_pop(shp_file = shp1, 
                                 year = year_of_interest, 
                                 min_age = ages_of_interest[1],
                                 max_age = ages_of_interest[2])
  
  #Plot map
  save_name <- paste("Endemic_zone", year_of_interest, ages_of_interest[1], ages_of_interest[2], sep = "_")
  
  plot_map_function_download(plot_data, shp1, shp0, save_name)
  
}


#' Function to plot downloadable maps of vaccination coverage
#' 
#' @param vc_pop_df vaccination coverage data frame of caluclated risk for each province in a country
#' @param shp_file shape file for one country
#' @param country_outline adm0 for plotting the outlines of the country
#' @return leaflet of vaccination coverage map
#' @export

plot_map_function_download <- function(vc_pop_df, shp_file, country_outline, save_name){
  
  #Sort so in the same order
  shp_file <- shp_file[order(shp_file$SPID), ]
  vaccination <- if(any(names(vc_pop_df) == "adm1_id")) vc_pop_df[order(vc_pop_df$adm1_id), ] else vc_pop_df[order(vc_pop_df$ISO), ]
  
  non_zero_df <- vaccination[which(vaccination$vc != 0), ]
  no_zero_shp <- shp_file[shp_file$SPID %in% non_zero_df$adm1_id,  ]
  country_outline <- shp0[shp0$ISO %in% shp_file[shp_file$SPID %in% vaccination$adm1_id,  ]$ISO, ]
  
  #Colour scheme for plotting
  mybreaks <- seq(0, 100, length.out = 101)
  mycols <- colorRampPalette(brewer.pal(9, "YlGn"))(length(mybreaks) - 1)
  vcols <- findInterval(non_zero_df$vc*100, mybreaks)
  
  plot(no_zero_shp, col = mycols[vcols])
  plot(country_outline, add = TRUE, lwd = 2)
  mtext(side = 3, text = save_name, line = -2)
  image.plot(col.sub = "black", legend.only = T, breaks = mybreaks, col = mycols, 
             zlim = c(0, 100), cex = 1.5, axis.args  =  list(cex.axis  =  1.5), 
             legend.width  =  1.5, legend.shrink	 = 0.7)
  
}





