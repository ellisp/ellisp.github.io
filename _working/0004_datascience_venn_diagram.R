library(showtext)
library(Cairo)
library(gridExtra)
library(RColorBrewer)

font.add.google("Poppins", "myfont")
showtext.auto()
palette <- brewer.pal(3, "Set1")



radius <- 0.3
strokecol <- "grey50"
linewidth <- 4
fs <- 11

draw_diagram <- function(){
grid.newpage()
grid.circle(0.33, 0.67, radius, gp = 
               gpar(col = strokecol,
                    fill = palette[1],
                    alpha = 0.2,
                    lwd = linewidth))

grid.circle(0.67, 0.67, radius, gp =
            gpar(col = strokecol,
                 fill = palette[2],
                 alpha = 0.2,
                 lwd = linewidth))

grid.circle(0.5, 0.33, radius, gp =
               gpar(col = strokecol,
                    fill = palette[3],
                    alpha = 0.2,
                    lwd = linewidth))

grid.text("Hacking", 0.25, 0.75, rot = 45, gp =
             gpar(fontfamily = "myfont",
                  fontsize = fs * 2.3,
                  col = palette[1],
                  fontface = "bold"))

grid.text("Statistics", 0.75, 0.75, rot = -45, gp =
             gpar(fontfamily = "myfont",
                  fontsize = fs * 2.3,
                  col = palette[2],
                  fontface = "bold"))

grid.text("Content\nknowledge", 0.5, 0.25, rot = 0, gp =
             gpar(fontfamily = "myfont",
                  fontsize = fs * 2.3,
                  col = palette[3],
                  fontface = "bold"))

grid.text("Danger:\nno context", 0.5, 0.75,
          gp = gpar(fontfamily = "myfont",
                    fontsize = fs))

grid.text("Danger: no\nunderstanding\nof probability", 0.32, 0.48, rot = 45,
          gp = gpar(fontfamily = "myfont",
                    fontsize = fs))

grid.text("Traditional\nresearch", 0.66, 0.46, rot = -45,
          gp = gpar(fontfamily = "myfont",
                    fontsize = fs))


grid.text("Data science /\napplied\nstatistics", 0.5, 0.55, 
          gp = gpar(fontfamily = "myfont",
                    fontsize = fs * 1.2,
                    fontface = "bold"))
}

svg("../img/0004_venndiagram.svg", 6, 6)
   draw_diagram()
dev.off()

png("../img/0004_venndiagram.png", 6 * 100, 6 * 100, res = 100)
   draw_diagram()
dev.off()

