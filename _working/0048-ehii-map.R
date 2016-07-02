library(httr) # for GET
library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)
library(RColorBrewer) # for brewer.pal(...)
library(openxlsx)
library(tweenr)
library(maps)
library(countrycode)
library(ggthemes) # for theme_map
library(rgdal)

library(cshapes)

# if we didn't already download it some previous day, download the UTIP EHII data:
if(!file.exists("ehii.xlsx")){
   download.file("http://utip.lbj.utexas.edu/data/EHII-UPDATED-10-30-2013.xlsx",
              destfile = "ehii.xlsx", mode = "wb")
}

ehii <- read.xlsx("ehii.xlsx")[ , -1] # don't need the first column

ehii_tidy <- ehii %>%
   gather(Year, Gini, -Country, -Code) %>%
   mutate(Year = as.numeric(Year)) %>%
   mutate(iso3c = ifelse(Code == "GER", "DEU", Code)) %>%
   left_join(countrycode_data[ , c("iso3c", "iso2c")], by = "iso3c")

# adapted from https://rud.is/b/2015/07/09/faceted-world-population-by-income-choropleths-in-ggplot/
try(GET("http://www.pewglobal.org/wp-content/lib/js/world-geo.json",
                  write_disk("world-geo.json")))

world <- readOGR("world-geo.json", "OGRGeoJSON")
world_wt <- spTransform(world, CRS("+proj=robin"))
world_map <- fortify(world_wt) %>%
   left_join(data_frame(id = rownames(world@data), iso2c =world@data$id)) 

limits = c(min(ehii_tidy$Gini) - 5, max(ehii_tidy$Gini) + 5)

i <- 1970
years <- (i - 2):(i + 2)

the_data <- ehii_tidy %>%
   filter(Year %in% years) %>%
   group_by(iso2c) %>%
   summarise(Gini = mean(Gini, na.rm = TRUE)) %>%
   right_join(world_map, by = "iso2c")

ggplot(the_data) +
   aes(x = long, y = lat, group = group, fill = Gini) +
   geom_polygon() +
   theme_map() +
   coord_equal() +
   scale_fill_gradientn(colours = brewer.pal(9, "Spectral")[9:1], limits = limits) +
   theme(legend.position = c(0.1, 0.1))



#================

cshp.data <- cshp()
head(cshp.data)
plot(cshp.data)
slotNames(cshp.data)



head(cshp.data@data)
