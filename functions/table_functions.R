#' Function to output a dataframe of the population-level vaccination coverage by adm location
#' 
#' @param shp_file shape file for one country
#' @param year year of interest
#' @return dataframe of population-level vaccination coverage by adm location
#' @export

flat_coverage_pop<-function(shp_file, year, min_age, max_age){
  
  vac_year<-save_object[[1]][, year-1939, (min_age + 1):(max_age + 1)]
  pop_year<-save_object[[2]][, year-1939, (min_age + 1):(max_age + 1)]
  
  country<-paste(unique(shp_file$ISO), collapse = "|")
  
  if(min_age == max_age){
    
    vac_year<-vac_year[which(grepl(country, names(vac_year)))]
    pop_year<-pop_year[which(grepl(country, names(pop_year)))]
    
    pop_vaccinated<-vac_year*pop_year
    pop_vaccinated[is.nan(pop_vaccinated)]<-0
    
    pop_agg<-pop_year
    pop_vac_agg<-pop_vaccinated
    
    data.frame(ISO = gsub('[0-9]', "", names(pop_vaccinated)),
               adm1_id = names(pop_vaccinated),
               adm1_name = shp_file[shp_file$SPID %in% names(pop_vaccinated), ]$NAME_1,
               pop = pop_year,
               vc = (pop_vaccinated)/(pop_year),
               stringsAsFactors = F)
    
  } else {
    
    vac_year<-vac_year[which(grepl(country, row.names(vac_year))), ]
    pop_year<-pop_year[which(grepl(country, row.names(pop_year))), ]
    
    pop_vaccinated<-vac_year*pop_year
    pop_vaccinated[is.nan(pop_vaccinated)]<-0
    
    pop_agg<-rowSums(pop_year)
    pop_vac_agg<-rowSums(pop_vaccinated)
    
    data.frame(ISO = gsub('[0-9]', "", row.names(pop_vaccinated)),
               adm1_id = row.names(pop_vaccinated),
               adm1_name = shp_file[shp_file$SPID %in% row.names(pop_vaccinated), ]$NAME_1,
               pop = rowSums(pop_year),
               vc = (rowSums(pop_vaccinated)/rowSums(pop_year)),
               stringsAsFactors = F)
    
  }
  
  
  
  
}

#' Function to output a dataframe of the population-level vaccination coverage at the country level
#' 
#' @param shp_file shape file for all countries
#' @param year year of interest
#' @return dataframe of population-level vaccination coverage by country
#' @export

flat_coverage_pop_endemic<-function(shp_file, year, min_age, max_age){
  
  vac_year<-save_object[[1]][, year-1939, (min_age + 1):(max_age + 1)]
  pop_year<-save_object[[2]][, year-1939, (min_age + 1):(max_age + 1)]
  
  country<-paste(unique(shp_file$ISO), collapse = "|")
  
  if(min_age == max_age){
    
    vac_year<-vac_year[which(grepl(country, names(vac_year)))]
    pop_year<-pop_year[which(grepl(country, names(pop_year)))]
    
    pop_vaccinated<-vac_year*pop_year
    pop_vaccinated[is.nan(pop_vaccinated)]<-0
    
    pop_agg<-pop_year
    pop_vac_agg<-pop_vaccinated
    
  } else {
    
    vac_year<-vac_year[which(grepl(country, row.names(vac_year))), ]
    pop_year<-pop_year[which(grepl(country, row.names(pop_year))), ]
    
    pop_vaccinated<-vac_year*pop_year
    pop_vaccinated[is.nan(pop_vaccinated)]<-0
    
    pop_agg<-rowSums(pop_year)
    pop_vac_agg<-rowSums(pop_vaccinated)
    
    
  }
  
  names(pop_agg)<-gsub("[0-9]+", "", names(pop_agg))
  names(pop_vac_agg)<-gsub("[0-9]+", "", names(pop_vac_agg))
  
  pop_agg_df<-aggregate(pop_agg, by = list(names(pop_agg)), FUN = sum)  
  vac_agg_df<-aggregate(pop_vac_agg, by = list(names(pop_vac_agg)), FUN = sum)  
  
  name_dis<-unique(shp_file[shp_file$ISO %in% pop_agg_df[, 1], ]$NAME_0)
  data.frame(ISO = pop_agg_df[, 1],
             adm0_name = name_dis,
             pop = pop_agg_df[, 2],
             vc = vac_agg_df[, 2]/pop_agg_df[, 2],
             stringsAsFactors = F)
  
}


#' Function to output a matrix of the population level vaccination coverage by age for plotting in line
#' 
#' @param shp_file shape file for one country
#' @param year year of interest
#' @return dataframe of population-level vaccination coverage by adm location
#' @export

coverage_by_age<-function(country, year){
  
  vac_year<-save_object[[1]][, year-1939, ]
  pop_year<-save_object[[2]][, year-1939, ]
  
  if(country != "all"){
    vac_year<-vac_year[which(grepl(country, row.names(vac_year))), ]
    pop_year<-pop_year[which(grepl(country, row.names(pop_year))), ]
  }
  
  total<-vac_year*pop_year
  here<-matrix(colSums(round_any(total, 1, floor))/colSums(round_any(pop_year, 1, floor)), nrow = 1)
  
  row.names(here)<-"Average"
  
  full<-rbind(here, vac_year)
  
  full[is.nan(full)]<-0
  full
  
}


#' Function to output a dataframe of the population-level vaccination coverage by adm location aggregated for barplot
#' 
#' @param country country of interest
#' @param year year of interest
#' @return dataframe of population-level vaccination coverage by adm location
#' @export

coverage_by_age_aggregated<-function(country, province, year){
  
  vac_year<-save_object[[1]][which(grepl(country, row.names(save_object[[1]]))), year-1939, ]
  pop_year<-save_object[[2]][which(grepl(country, row.names(save_object[[2]]))), year-1939, ]
  
  if(all(province != "all")){
    vac_year<-matrix(vac_year[province, ], nrow = length(province), byrow = F)
    pop_year<-matrix(pop_year[province, ], nrow = length(province), byrow = F)
  }
  
  pop_vaccinated<-vac_year*pop_year
  pop_vaccinated[is.nan(pop_vaccinated)]<-0
  if(nrow(pop_vaccinated) > 1) pop_vaccinated<-colSums(pop_vaccinated)
  if(nrow(pop_year) > 1) pop_year<-colSums(pop_year)
  
  popvacmat<-matrix(c(pop_vaccinated, rep(0, 4)), ncol = 5, byrow = T)
  popyearmat<-matrix(c(pop_year, rep(0, 4)), ncol = 5, byrow = T)
  
  data.frame(pop_vac_agg = rowSums(popvacmat), pop_agg = rowSums(popyearmat), stringsAsFactors = F)
  
}


#' Function to output country-level dataframes to the server
#' 
#' @param country_of_interest country name of interest
#' @param year_of_interest year of interest
#' @param ages_of_interest ages of interest, vector of 2 the min and max ages
#' @return leaflet of vaccination coverage map
#' @export
#' 
country_df_gen<-function(shp1, country_of_interest, year_of_interest, ages_of_interest){
  
  country_shape <- shp1[shp1$NAME_0 == country_of_interest, ]
  
  #Generate df of coverage
  plot_data <- flat_coverage_pop(shp_file = country_shape,
                                 year = year_of_interest,
                                 min_age = ages_of_interest[1],
                                 max_age = ages_of_interest[2])
  
  #Create extra row of dataframe and format
  all <- data.frame(adm1_id = "All", adm1_name = "", pop = sum(plot_data$pop), vc = "", stringsAsFactors = F)
  average <- data.frame(adm1_id = "Average", adm1_name = "", pop = mean(plot_data$pop), vc = sum(plot_data$pop*plot_data$vc)/sum(plot_data$pop), stringsAsFactors = F)
  show_df <- rbind(all, average, plot_data[, 2:ncol(plot_data)])
  show_df$vc <- round(as.numeric(show_df$vc)*100, 1)
  show_df <- show_df[, c("adm1_id", "adm1_name", "vc", "pop")]
  
  DT::datatable(rownames = FALSE, show_df, options=list(pageLength = 14, searching = FALSE, processing = FALSE, mark = ",",
                                                        columnDefs = list(list(className = 'dt-center', targets = 2:3))), 
                colnames = c("ID", "Province", "Coverage (%)", "Population"), 
                selection = list(target='row')) %>% DT::formatRound(columns = "pop", mark = ",", digits = 0)
}


#' Function to output endemic-zone dataframes to the server
#' 
#' @param shp1 adm1 shapefile
#' @param year_of_interest year of interest
#' @param ages_of_interest ages of interest, vector of 2 the min and max ages
#' @return leaflet of vaccination coverage map
#' @export
#' 
endemic_df_gen<-function(shp1, year_of_interest, ages_of_interest){
  
  #Generate df of coverage
  plot_data <- flat_coverage_pop_endemic(shp_file = shp1,
                                         year = year_of_interest,
                                         min_age = ages_of_interest[1],
                                         max_age = ages_of_interest[2])
  
  #Create extra row of dataframe and format
  all<-data.frame(ISO = "All", adm0_name = "", pop = sum(plot_data$pop), vc = "", stringsAsFactors = F)
  africa <- data.frame(ISO = "Africa", adm0_name = "", pop = sum(plot_data[plot_data$ISO %in% africa_iso, ]$pop),
                       vc = sum(plot_data[plot_data$ISO %in% africa_iso, ]$pop * plot_data[plot_data$ISO %in% africa_iso, ]$vc) / sum(plot_data[plot_data$ISO %in% africa_iso, ]$pop),
                       stringsAsFactors = F)
  
  south_america <- data.frame(ISO = "South America", adm0_name = "", pop = sum(plot_data[plot_data$ISO %in% south_america_iso, ]$pop),
                             vc = sum(plot_data[plot_data$ISO %in% south_america_iso, ]$pop * plot_data[plot_data$ISO %in% south_america_iso, ]$vc) / sum(plot_data[plot_data$ISO %in% south_america_iso, ]$pop),
                             stringsAsFactors = F)
  average <- data.frame(ISO = "Average", adm0_name = "", pop = mean(plot_data$pop), vc = sum(plot_data$pop*plot_data$vc)/sum(plot_data$pop), stringsAsFactors = F)
  show_df <- rbind(all, average, africa, south_america, plot_data)
  show_df$vc <- round(as.numeric(show_df$vc)*100, 1)
  show_df<-show_df[, c("ISO", "adm0_name", "vc", "pop")]
  
  DT::datatable(rownames = FALSE, show_df, options=list(pageLength = 14, searching = FALSE, processing = FALSE, mark = ",",
                                                        columnDefs = list(list(className = 'dt-center', targets = 2:3))), 
                colnames = c("ISO", "Country", "Coverage (%)", "Population"), 
                selection = list(target='row')) %>% DT::formatRound(columns = "pop", mark = ",", digits = 0)
}