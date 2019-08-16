library(plyr)
library(RColorBrewer)
library(fields)
library(leaflet)
library(plotly)
library(Hmisc)
library(rgeos)
library(rgdal)
library(shinyBS)

shp0<-readOGR("data/shp/global_endemic_shp0.shp", stringsAsFactors = F)
shp1<-readOGR("data/shp/global_endemic_shp1.shp", stringsAsFactors = F)

load("data/vac_pop_data/pop_vac_data.Rdata")

country_vec<-shp0$ISO




sapply(1:101)


flat_coverage<-function(country, year){
  
  vac_year<-save_object[[1]][, year-1950, ]
  pop_year<-save_object[[2]][, year-1950, ]
  
  if(country != "all"){
    vac_year<-vac_year[which(grepl(country, row.names(vac_year))), ]
    pop_year<-pop_year[which(grepl(country, row.names(pop_year))), ]
  }
  
  pop_vaccinated<-vac_year*pop_year
  pop_vaccinated[is.nan(pop_vaccinated)]<-0
  
  rowSums(pop_vaccinated)/rowSums(pop_year)

}








