library(ggrepel)
library(leaflet)

ggplot(the_data, aes(x = MeanBedrooms, y = NumberInHH, colour = MedianIncome)) +
   geom_jitter(size = 4) +
#   geom_text(aes(label = row.names(the_data)), colour = "steelblue") +
   geom_abline(slope = 1, intercept = 0) +
   scale_colour_gradientn(colours = c("blue", "yellow", "red"), label = dollar)

tmp <- the_data %>%
   filter(WGS84Longitude > 0) 

pal <- colorQuantile("RdBu", NULL, n = 9)

labs <- paste0(row.names(the_data), " $", format(the_data$MedianIncome, big.mark = ","))

leaflet() %>%
   addProviderTiles("CartoDB.Positron") %>%
   addCircles(lng = tmp$WGS84Longitude, lat = tmp$WGS84Latitude,
              color = pal(-tmp$MedianIncome),
              popup = labs,
              radius = 500)

