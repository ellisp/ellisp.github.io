library(showtext)
library(ggplot2)
library(scales)
library(grDevices)
library(stats)
library(extrafont)
library(beepr)
library(Cairo)
library(ggrepel)
library(svglite)
library(frs)

myfont <- "Roboto"
main_font <- "Roboto"
heading_font <- "Sarala"


my_theme <- theme_light(base_family = main_font) + 
             theme(legend.position = "bottom") +
             theme(plot.caption = element_text(colour = "grey50"),
                   strip.text = element_text(size = rel(1), face = "bold"),
                   plot.title = element_text(family = heading_font))

theme_set(my_theme)          
update_geom_defaults("text", list(family = main_font))
update_geom_defaults("text_repel", list(family = main_font))
update_geom_defaults("label", list(family = main_font))