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
library(conflicted)

library(frs) # remotes::install_github("ellisp/frs-r-package/pkg")
conflict_prefer("filter", "dplyr")
conflict_prefer("select", "dplyr")

myfont <- "Roboto"
main_font <- "Roboto"
heading_font <- "Sarala"

# uncomment the next line if you want everything in Calibri instead
# main_font <- myfont <- heading_font <- "Calibri"

my_theme <- theme_light(base_family = main_font) + 
             theme(legend.position = "bottom") +
             theme(plot.caption = element_text(colour = "grey50"),
                   strip.text = element_text(size = rel(1), face = "bold"),
                   plot.title = element_text(family = heading_font))

theme_set(my_theme)          
update_geom_defaults("text", list(family = main_font))
update_geom_defaults("text_repel", list(family = main_font))
update_geom_defaults("label", list(family = main_font))