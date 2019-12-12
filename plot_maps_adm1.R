





pop <- save_object[[2]]
vac <- save_object[[1]]
smooth_shape <- readOGR("Y:/DATA/shapefiles/gadm28/All/gadm28_adm1.shp")
smooth_shape$SPID <- paste0(smooth_shape$ISO, smooth_shape$ID_1)
endemic_high_res <- smooth_shape[smooth_shape$ISO %in% shp1$ISO, ]
endemic_high_res <- endemic_high_res[!duplicated(endemic_high_res$SPID), ]
# endemic_high_res$SPID <- paste0(endemic_high_res$GID_0, gsub("_1|[a-zA-Z]|\\.", "", endemic_high_res$GID_1))
endemic_high_res <- endemic_high_res[endemic_high_res$SPID %in% shp1$SPID, ]
endemic_high_res <- endemic_high_res[order(endemic_high_res$SPID), ]

yeetos <- c(1950, 1985, 2005, 2010, 2015)
coverage_by_year <- sapply(yeetos, function(yeet){
  
  vac_2019 <- rowSums(pop[, yeet - 1940, ] * vac[, yeet - 1940, ], na.rm = T) / rowSums(pop[, yeet - 1940, ], na.rm = T)
  
  # smooth_shape <- readOGR("Y:/DATA/shapefiles/gadm36/gadm36_1.shp")
  vac_2019 <- vac_2019[order(names(vac_2019))]
  vac_2019[which(names(vac_2019) %in% endemic_high_res$SPID)]
  
  
}, simplify = FALSE)


names(coverage_by_year[[1]]) == endemic_high_res$SPID


# windows(width = 4, height = 12)
png("C:/Users/ah1114/Dropbox/PhD/Dissertation/doc/5_Results_population_level_vaccination_coverage/vaccination_coverage_years.png", 
    height = 12, width = 4, res = 200, units = 'in')
par(mfrow = c(5, 1), oma = c(0, 0, 1, 0), mar = c(0, 0, 0, 0))

mybreaks <- seq(0, 1, length.out = 101)
mycols <- colorRampPalette(brewer.pal(9, "YlGn"))(length(mybreaks)-1)

sapply(1:length(coverage_by_year), function(x){
  vac_this <- coverage_by_year[[x]]
  
  vcols <- findInterval(vac_this, mybreaks)
  
  plot(endemic_high_res, col = mycols[vcols], border = NA)
  plot(shp0, add = T)
  mtext(side = 3, text = yeetos[x], cex = 1.5, adj = 0, line = -2)
  
  # mybreaks <- c(seq(-.2, 0, length.out = 21), seq(0.01, 0.81, length.out = 81))
  # mycols <- c(rev(colorRampPalette(brewer.pal(9, "YlGn"))(21)), colorRampPalette(brewer.pal(9, "YlOrRd"))(80))
  # vcols <- findInterval(.8 - vac_2019, mybreaks)
  # 
  # plot(endemic_high_res, col = mycols[vcols], border = NA)
  # plot(shp0[-which(shp0$ISO == "ZMB"), ], add = T)
  # mtext(side = 3, text = "B", cex = 1.5, adj = 0, line = -1)
  # 
  # image.plot(legend.only = T, ticks = c(-.2, 0, 0.4, 0.6, .8), col = mycols, breaks = mybreaks,
  #            legend.shrink = 0.5)
})
# plot(1, 1, col = "white", bty = "n", yaxt = "n", xaxt = "n", ylab = "", xlab = "")

dev.off()


png("C:/Users/ah1114/Dropbox/PhD/Dissertation/doc/5_Results_population_level_vaccination_coverage/legend.png", 
    height = 6, width = 4, res = 200, units = 'in')
plot(1, 1, col = "white", bty = "n", yaxt = "n", xaxt = "n", ylab = "", xlab = "")

  image.plot(legend.only = T, ticks = c(0.25, 0.5, 0.75, 1), col = mycols, breaks = mybreaks,
             legend.shrink = 1, cex.axis = 2, cex.legend = 2)

dev.off()


