setwd("_working")
proj_dir <- getwd()
library(showtext)
library(ggplot2)
library(scales)

font.add.google("Poppins", "myfont")
showtext.auto()
showtext.opts(dpi = 600)

theme_set(theme_light(base_family = "myfont") + 
             theme(legend.position = "bottom") +
             theme(plot.caption = element_text(colour = "grey50"))) 
update_geom_defaults("text", list(family = "myfont"))
