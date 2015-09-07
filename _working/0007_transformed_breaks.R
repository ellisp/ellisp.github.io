library(ggplot2)
library(scales) 
library(tidyr)
library(dplyr)
library(showtext)
font.add.google("Poppins", "myfont")
showtext.auto()

eg <- data.frame(x = c(exp(rnorm(100, 6, 1)), rnorm(50, -50, 60)))


p <- ggplot(eg, aes(x = x)) +
   geom_density() +
   geom_rug() +
   theme_minimal(base_family = "myfont")

p1 <- p + labs(x = "Original scale")
p2 <- p + scale_x_continuous("Logarithm scale", trans = "log", label = comma) 
p3 <- p + scale_x_continuous("Modulus transform scale", trans = modulus_trans(0.5), label = comma)

# Layout function from Wickham's ggplot2 book, not sure where it first came from
vplayout <- function (x, y) {
   viewport(layout.pos.row = x, layout.pos.col = y)
}

svg("../img/0007_density_plots.svg", 5, 7)
   grid.newpage()
   pushViewport(viewport(layout = grid.layout(3, 1)))
   print(p1, vp = vplayout(1,1))
   print(p2, vp = vplayout(2,1))
   print(p3, vp = vplayout(3,1))
dev.off()

#---------------breaks---------------
# helper functions
.mod_transform <- function(y, lambda){
   if(lambda != 0){
      yt <- sign(y) * (((abs(y) + 1) ^ lambda - 1) / lambda)
   } else {
      yt = sign(y) * (log(abs(y) + 1))
   }
   return(yt)
}


.mod_inverse <- function(yt, lambda){
   if(lambda != 0){
      y <- ((abs(yt) * lambda + 1)  ^ (1 / lambda) - 1) * sign(yt)
   } else {
      y <- (exp(abs(yt)) - 1) * sign(yt)
      
   }
   return(y)
}


prettify <- function(breaks){
   # round numbers, more aggressively the larger they are
   digits <- -floor(log10(abs(breaks))) + 1
   digits[breaks == 0] <- 0
   return(round(breaks, digits = digits))
}

mod_breaks <- function(lambda, n = 8, prettify = TRUE){
   function(x){
      breaks <- .mod_transform(x, lambda) %>%
         pretty(n = n) %>%
         .mod_inverse(lambda)
      if(prettify){
         breaks <- prettify(breaks)
      }
      return(breaks)
   }
}
   


p1 <- p + 
   scale_x_continuous(trans = modulus_trans(0.2), label = comma, 
                      breaks = mod_breaks(lambda = 0.2, prettify = FALSE)) +
   theme(panel.grid.minor = element_blank()) +
   ggtitle("Regular breaks")


p2 <- p + 
   scale_x_continuous(trans = modulus_trans(0.2), label = comma, 
                      breaks = mod_breaks(lambda = 0.2, prettify = TRUE)) +
   theme(panel.grid.minor = element_blank()) +
   ggtitle("Rounded breaks")



svg("../img/0007_density_plots_breaks.svg", 5, 5)
   grid.newpage()
   pushViewport(viewport(layout = grid.layout(2, 1)))
   print(p1, vp = vplayout(1,1))
   print(p2, vp = vplayout(2,1))
dev.off()

png("../img/0007_density_plots_breaks.png", 5 * 100, 5 * 100, res = 100)
grid.newpage()
pushViewport(viewport(layout = grid.layout(2, 1)))
print(p1, vp = vplayout(1,1))
print(p2, vp = vplayout(2,1))
dev.off()

#---------------apply to the NZIS--------------

library(RODBC)


# connect to database
PlayPen <- odbcConnect("PlayPen_prod")
sqlQuery(PlayPen, "use nzis11")


inc <- sqlQuery(PlayPen, "select * from vw_mainheader") 

svg("../img/0007_better_nzis_breaks.svg", 8, 5)
ggplot(inc, aes(x = income)) +
   geom_density() +
   geom_rug() +
   scale_x_continuous(trans = modulus_trans(0.25), label = dollar,
                      breaks = mod_breaks(0.25)) +
   theme_minimal(base_family = "myfont")
dev.off()

p1 <- ggplot(inc, aes(x = hours, y = income)) +
   facet_wrap(~occupation, ncol =2) +
   geom_point(alpha = 0.2) +
   scale_x_continuous(trans = modulus_trans(0.25), breaks = mod_breaks(0.25)) +
   scale_y_continuous(trans = modulus_trans(0.25), label = dollar, breaks = mod_breaks(0.25)) +
   theme_light(base_family = "myfont")

svg("../img/0007_income_by_occupation.svg", 7, 12)
   print(p1)
dev.off()
