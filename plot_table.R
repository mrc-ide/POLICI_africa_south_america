
options(scipen=999)


print <- flat_coverage_pop_endemic(shp1, 2019, 0, 100)[, 2:4]
print$'Population un-vaccinated' <- print$pop * (1 - print$vc)
print[1, ]

print$`Population un-vaccinated` <- formatC(floor(print$`Population un-vaccinated`), big.mark = ",", format = "d")
print$pop <- formatC(floor(print$pop), big.mark = ",", format = "d")
print$vc <- round(print$vc*100, 1)

colnames(print) <- c("Country", "Population", "Vaccination coverage (%)", "Population unvaccinated")




write.csv(print, "table.csv", row.names = F)

# print[which(print$vc == 0), 4] <- print[which(print$vc == 0), 2]
