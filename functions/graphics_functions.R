#' Function to plot interactive maps of vaccination coverage
#' 
#' @param shp_file shape file for one country
#' @param vaccination vaccination coverage data frame of caluclated risk for each province in a country
#' @param shp_file_outline adm0 for plotting the outlines of the country
#' @return leaflet of vaccination coverage map
#' @export

plot_map_function <- function(vc_pop_df, shp_file){
  
  #Sort so in the same order
  shp_file <- shp_file[order(shp_file$SPID), ]
  vaccination <- vc_pop_df[order(vc_pop_df$adm1_id), ]
  
  #Creating the popup 
  popup <- paste0("Name: ", vaccination$adm1_name, " - ",
               "Population immunity: ", paste0(round(vaccination$vc, 1), "%"), " - ",
               "Population: ", formatC(vaccination$pop, format = "d", big.mark = ","))
  
  #Colour scheme for plotting
  numpal <- colorNumeric("YlGn", 0:100, na.color = "#808080", alpha = FALSE)
  
  leaflet(shp_file) %>%
    addProviderTiles("Esri.WorldGrayCanvas", options = tileOptions(maxZoom=9)) %>%
    addPolygons(
      stroke = TRUE, fillOpacity = 1, smoothFactor = 1,
      fillColor = ~numpal(vaccination$vc*100), col="black",
      weight = 2, label = (popup)) %>%
    addLegend("bottomright", pal = numpal, values = 0:100,
              title = "Coverage (%)",
              opacity = 1, bins = 10, layerId = "map")
  
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
plot_age_vc_linegraph <- function(vc_age){
  
  #Set up the margins for plotting
  m <- list(l = 50, r = 50, b = 50, t = 50, pad = 4)
  
  #Transform data in a way that plotly likes for plotting
  updated_format<-do.call(rbind, sapply(1:nrow(vc_age), function(x) data.frame(SPID = row.names(vc_age)[x], vc = vc_age[x, ], 
                                                                               age = 1:length(vc_age[x, ]),
                                                                               stringsAsFactors = F), simplify = F))
  #Transform to %
  updated_format$vc<-updated_format$vc*100
  
  #Plot
  plot_ly(updated_format, x = updated_format$age, y = updated_format$vc, color= updated_format$SPID) %>% add_lines(y = updated_format$vc) %>%
    layout(xaxis = list(title = "Age", range = c(0, 100)), yaxis = list(title = "Vaccinated (%)",
                                                                    range = c(0, 100)), showlegend=TRUE, margin = m) 
  
  
}



