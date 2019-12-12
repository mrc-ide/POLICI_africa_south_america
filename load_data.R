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
library(stringi)

invisible(sapply(list.files("functions", full.names = T), function(x) source(x)))

shp0<-readOGR("data/shp/global_endemic_shp0.shp", stringsAsFactors = F, encoding = 'UTF-8')
shp0<-shp0[order(shp0$ISO), ]
shp0<-shp0[-which(shp0$ISO %in% c("ERI", "ZMB")), ]

shp1<-readOGR("data/shp/global_endemic_shp1.shp", stringsAsFactors = F, encoding = 'UTF-8')
shp1$SPID<-paste0(shp1$ISO, shp1$ID_1)
shp1<-shp1[!duplicated(shp1$SPID), ]
shp1<-shp1[-which(shp1$ISO %in% c("ERI", "ZMB", "BDI", "RWA", "SOM", "TZA")), ]

shp0 <- shp0[which(shp0$ISO %in% unique(shp1$ISO)), ]


# load("data/vac_pop_data/endemic_countries_coverage_171019_adm1.Rdata")
load("data/vac_pop_data/final_vacc_coverage_stitched_full.Rdata")
# load("data/vac_pop_data/pop_vac_data.Rdata")

country_vec<-unique(shp1$NAME_0)

shp1<-shp1[which(shp1$SPID %in% row.names(save_object[[1]])), ]

save_object[[1]]<-save_object[[1]][!duplicated(row.names(save_object[[1]])), , ]
save_object[[2]]<-save_object[[2]][!duplicated(row.names(save_object[[2]])), , ]

save_object[[1]]<-save_object[[1]][order(row.names(save_object[[1]])), , ]
save_object[[2]]<-save_object[[2]][order(row.names(save_object[[2]])), , ]
shp1<-shp1[order(shp1$SPID), ]


#Africa
africa_iso <- c("AGO", "BDI", "BEN", "BFA", "CAF", "CIV", "CMR", "COD", "COG", "ETH", "ERI", "GAB", "GHA",
                "GIN", "GMB", "GNB", "GNQ", "KEN", "LBR", "MLI", "MRT", "NER", "NGA", "RWA", "SDN",
                "SEN", "SLE", "SOM", "SSD", "TCD", "TGO", "TZA", "UGA", "ZMB")
south_america_iso <- unique(shp0$ISO)[-which(unique(shp0$ISO) %in% africa_iso)]





