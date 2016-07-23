library(leaflet)
library(nzcensus)

# remove Chatham Islands 
tmp <- AreaUnits2013[AreaUnits2013$WGS84Longitude > 0 & !is.na(AreaUnits2013$MedianIncome2013), ]

# create colour palette function
pal <- colorQuantile("RdBu", NULL, n = 10)

# create labels for popups
labs <- paste0(tmp$AU_NAM, " $", format(tmp$MedianIncome2013, big.mark = ","))


# draw map:
leaflet() %>%
   addProviderTiles("CartoDB.Positron") %>%
   addCircles(lng = tmp$WGS84Longitude, lat = tmp$WGS84Latitude,
              color = pal(-tmp$MedianIncome2013),
              popup = labs,
              radius = 500) %>%
   addLegend(
      pal = pal,
      values = -tmp$MedianIncome2013,
      title = "Quantile of median<br>household income",
      position = "topleft",
      bins = 5)

