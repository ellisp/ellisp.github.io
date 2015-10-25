library(riverplot)   # for Sankey charts
library(dplyr)
library(grid)
library(showtext)    # for fonts
library(stringr)
   
font.add.google("Poppins", "myfont")
showtext.auto()


silver <- function(node_values, edge_values, title, 
                   pal = c("orange", "cyan", "grey90"), family = "myfont",
                   cal_width = 650){
   # Function that draws a Sankey chart of intercontinental silver flows
   # @node_values - the number of production / consumption / travel to be added
   #   to America, Western Europe, Levant, Baltic, Cape, Japan, SE Asia, Atlantic, Pacific, Sea of Japan
   # @edge_values - the width of the connections in this order:
   #    America to Pacific to South East Asia
   #    America to Atlantic to Western Europe
   #    Western Europe to Baltic to South East Asia
   #    Western Europe to Levant to South East Asia
   #    Western Europe to Cape to South East Asia
   #    Japan to Sea of Japan to South East Asia
   # @title title for plot
   # @pal vector of colours for producers, consumers, and travel routes respectively
   # @family font family to use
   # @cal_width width of an invisible white ribbon to draw, implicitly the maximum, 
   #    so scale is constant for multiple plots
   
   # edge_values comes in as just a single number and we need two edges for 
   # each (eg America to Pacific to SE Asia is two edges)
   edge_values <- rep(edge_values, each = 2)
   
   # we don't know how much of the silver that goes via the Baltic and the Levant
   # actually gets to Asia (as opposed to staying in middle East and Russia),
   # so to represent this we halve the width of those two edges:
   edge_values[ c(6, 8)] <- edge_values[ c(6, 8)] / 2
   
   nodes <- data.frame(
      
      # The X and Y coordinates of the nodes (Americas, Atlantic, etc) are 
      # just chosen by hand:
      x = c(2, 4, 6, 5, 4.7, 9, 8, 3,   5, 8.5, 2, 9),
      y = c(3, 6, 5, 7, 4,   5, 3, 4.5, 0, 4,   8, 8),
      ID = LETTERS[1:12],
      labels = c("America", "Western Europe", "Levant", "Baltic", "Cape of\nGood Hope", "Japan", "South &\nEast Asia", 
                 "Atlantic", "Pacific", "", "", ""),
      amount = c(node_values, "", ""),
      col = c(pal[c(1, 2, 3, 3, 3, 1, 2, 3, 3, 3)], "white", "white"),
      stringsAsFactors = FALSE) %>%
      mutate(labels = paste(labels, amount, sep = "\n"),
             labels = gsub("\n59.3", "59.3", labels))
   
   edges <- data.frame(
      # Each N1 - N2 pair connects two nodes eg A-I connects America to Atlantic:
      N1 =    c("A", "I", "A", "H", "B", "D", "B", "C", "B", "E",  "F", "J", "K"),
      N2 =    c("I", "G", "H", "B", "D", "G", "C", "G", "E", "G",  "J", "G", "L"),
      Value = c(edge_values, cal_width)
   )
   
   p <- makeRiver(nodes, edges)
   my_style <- default.style()
   my_style$textcol <- "grey20"
   my_style$srt = 0
   
   par(family = family)
   plot(p, default_style = my_style, plot_area = 0.9)
   grid.text(x = 0.5, y = 0.94, title,
             gp = gpar(fontfamily = family, cex = 1.5))
   grid.text(x = 0.13, y = 0.12, 
             str_wrap("Source: DeVries (2003), reproduced in Findlay and O'Rourke (2007) 'Power and Plenty'", 34),
             gp = gpar(fontfamily = family, fontface = "italic", cex = 0.7))
   legend(7, 1, legend = c("Producer", "Trade route", "Consumer"), 
          bty = "n", fill = pal[c(1, 3, 2)], border = FALSE, cex=0.8)
}

# draw the two images
png("01.png", 800, 500, res = 100)
   silver(
      node_values = c(368, 158, 38, 56, 16, "?", "91-126", 268, "17-51", "59"), 
      edge_values = c(34,  268,  56,  38, 15.5, 59.3), 
      title = "Intercontinental flows of silver, tonnes per year, 1600 to 1650")
dev.off()

png("02.png", 800, 500, res = 100)
silver(
   node_values = c(650, 230, 60, 50, 160, "?", "175-211", 500, "15-51", "0"), 
   edge_values = c(33.1,  500,  50,  60, 160, 0.00), 
   title = "Intercontinental flows of silver, tonnes per year, 1725 to 1750")
dev.off()

# combine them into an animated GIF
system('"C:\\Program Files\\ImageMagick-6.9.1-Q16\\convert" -loop 0 -delay 250 *.png "silver.gif"')

# move the asset over to where needed for the blog
file.copy("silver.gif", "../img/0015-silver.gif", overwrite = TRUE)

# cleanup
unlink(c("0?.png", "silver.gif"))




