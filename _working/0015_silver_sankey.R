   library(riverplot)
   library(RColorBrewer)
   library(dplyr)
   library(grid)
   library(RSvgDevice)
   library(showtext) # for fonts
   library(stringr)
      
   font.add.google("Poppins", "myfont")
   showtext.auto()
   
   pal <- c("orange", "cyan", "grey95")
   
   nodes <- data.frame(
      x = c(2, 4, 6, 5, 4.7, 9, 8, 3,   5, 8.5),
      y = c(3, 6, 5, 7, 4,   5, 3, 4.5, 0,  4),
      ID = LETTERS[1:10],
      labels = c("America", "Western Europe", "Levant", "Baltic", "Cape of\nGood Hope", "Japan", "South &\nEast Asia", "Atlantic", "Pacific", ""),
      amount = c(368,        158.5,            38,       56,       15.5,                 "unknown",     "91-126",          268,       "17-51", "59.3"),
      col = pal[c(1, 2, 3, 3, 3, 1, 2, 3, 3, 3)],
      stringsAsFactors = FALSE) %>%
      mutate(labels = paste(labels, amount, sep = "\n"),
             labels = gsub("\n59.3", "59.3", labels))
   
   edges <- data.frame(
      N1 =    c("A", "I", "A", "H", "B", "B", "B", "D", "C",  "E",  "F", "J"),
      N2 =    c("I", "G", "H", "B", "D", "C", "E", "G", "G",  "G",  "J", "G"),
      Value = c(34,  34,  268,  268, 56,  38,  56, 38,   15.5, 15.5, 59.3, 59.3)
   )
   
   p <- makeRiver(nodes, edges)
   my_style <- default.style()
   my_style$textcol <- "grey20"
   my_style$srt = 0
   
   devSVG("test.svg", 8, 5)
   par(family = "myfont")
   
   plot(p, default_style = my_style, plot_area = 0.8)
   grid.text(x = 0.5, y = 0.97, "Intercontinental flows of silver, kilograms per year, 1600 to 1650",
             gp = gpar(fontfamily = "myfont", cex = 1.5))
#    grid.text(x = 0.77, y = 0.1, "Kilogrammes per year",
#              gp = gpar(fontfamily = "myfont", cex = 1))
   grid.text(x = 0.15, y = 0.15, str_wrap("Source: de Vries (2003), reproduced in Findlay and O'Rourke 'Power and Plenty'", 30),
             gp = gpar(fontfamily = "myfont", fontface = "italic", cex = 0.7))
   legend(7, 1, legend = c("Producer", "Consumer"), bty = "n", fill = pal[1:2], border = FALSE, cex=0.8)
   dev.off()

