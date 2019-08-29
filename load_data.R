library(plyr)
library(RColorBrewer)
library(fields)
library(leaflet)
library(plotly)
library(Hmisc)
library(rgeos)
library(rgdal)
library(shinyBS)
library(raster)
library(data.table)
library(DT)

invisible(sapply(list.files("functions", full.names = T), function(x) source(x)))

shp0<-readOGR("data/shp/global_endemic_shp0.shp", stringsAsFactors = F)
shp0<-shp0[order(shp0$ISO), ]
shp1<-readOGR("data/shp/global_endemic_shp1.shp", stringsAsFactors = F)
shp1$SPID<-paste0(shp1$ISO, shp1$ID_1)
shp1<-shp1[!duplicated(shp1$SPID), ]

load("data/vac_pop_data/vacc_coverage_and_population_adm1.Rdata")
# load("data/vac_pop_data/pop_vac_data.Rdata")

country_vec<-shp0$NAME_ENGLI

shp1<-shp1[which(shp1$SPID %in% row.names(save_object[[1]])), ]

save_object[[1]]<-save_object[[1]][!duplicated(row.names(save_object[[1]])), , ]
save_object[[2]]<-save_object[[2]][!duplicated(row.names(save_object[[2]])), , ]

save_object[[1]]<-save_object[[1]][order(row.names(save_object[[1]])), , ]
save_object[[2]]<-save_object[[2]][order(row.names(save_object[[2]])), , ]
shp1<-shp1[order(shp1$SPID), ]






