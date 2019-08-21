#' Function to output a dataframe of the population-level vaccination coverage by adm location
#' 
#' @param shp_file shape file for one country
#' @param year year of interest
#' @return dataframe of population-level vaccination coverage by adm location
#' @export

flat_coverage_pop<-function(shp_file, year, min_age, max_age){
  
  vac_year<-save_object[[1]][, year-1950, (min_age + 1):(max_age + 1)]
  pop_year<-save_object[[2]][, year-1950, (min_age + 1):(max_age + 1)]
  
  country<-paste(unique(shp_file$ISO), collapse = "|")
  
  vac_year<-vac_year[which(grepl(country, row.names(vac_year))), ]
  pop_year<-pop_year[which(grepl(country, row.names(pop_year))), ]

  pop_vaccinated<-vac_year*pop_year
  pop_vaccinated[is.nan(pop_vaccinated)]<-0
  
  data.frame(ISO = gsub('[0-9]', "", row.names(pop_vaccinated)),
             adm1_id = row.names(pop_vaccinated),
             adm1_name = shp_file[shp_file$SPID %in% row.names(pop_vaccinated), ]$NAME_1,
             pop = rowSums(pop_year),
             vc = (rowSums(pop_vaccinated)/rowSums(pop_year)),
             stringsAsFactors = F)
  
}


#' Function to output a matrix of the population level vaccination coverage by age for plotting in line
#' 
#' @param shp_file shape file for one country
#' @param year year of interest
#' @return dataframe of population-level vaccination coverage by adm location
#' @export

coverage_by_age<-function(country, year){
  
  vac_year<-save_object[[1]][, year-1950, ]
  pop_year<-save_object[[2]][, year-1950, ]
  
  if(country != "all"){
    vac_year<-vac_year[which(grepl(country, row.names(vac_year))), ]
    pop_year<-pop_year[which(grepl(country, row.names(pop_year))), ]
  }
  
  vac_year[is.nan(vac_year)]<-0
  vac_year
  
}


#' Function to output a dataframe of the population-level vaccination coverage by adm location aggregated for barplot
#' 
#' @param shp_file shape file for one country
#' @param year year of interest
#' @return dataframe of population-level vaccination coverage by adm location
#' @export

coverage_by_age_aggregated<-function(country, year){
  
  vac_year<-save_object[[1]][, year-1950, ]
  pop_year<-save_object[[2]][, year-1950, ]
  
  if(country != "all"){
    vac_year<-vac_year[which(grepl(country, row.names(vac_year))), ]
    pop_year<-pop_year[which(grepl(country, row.names(pop_year))), ]
  }
  
  pop_vaccinated<-vac_year*pop_year
  pop_vaccinated[is.nan(pop_vaccinated)]<-0
  
  pop_vac_agg<-do.call(cbind, by(t(pop_vaccinated), (seq(ncol(pop_vaccinated)) - 1) %/% 5, FUN = colSums))
  pop_agg<-do.call(cbind, by(t(pop_year), (seq(ncol(pop_year)) - 1) %/% 5, FUN = colSums))
  
  data.frame(pop_vac_agg = colSums(pop_vac_agg), pop_agg = colSums(pop_agg) - colSums(pop_vac_agg), stringsAsFactors = F)

}
