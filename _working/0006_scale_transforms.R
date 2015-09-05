

#------------define my new transformation------------
library(scales) 


# John and Draper's modulus transformation
modulus_trans <- function(lambda){
   trans_new("modulus",
             transform = function(y){
                if(lambda != 0){
                   yt <- sign(y) * (((abs(y) + 1) ^ lambda - 1) / lambda)
                } else {
                   yt = sign(y) * (log(abs(y) + 1))
                }
                return(yt)
             },
             inverse = function(yt){
                if(lambda != 0){
                   y <- ((abs(yt) * lambda + 1)  ^ (1 / lambda) - 1) * sign(yt)
                } else {
                   y <- (exp(abs(yt)) - 1) * sign(yt)
                   
                }
                return(y)
             }
             )
}

#-------------analysis------------
library(RODBC)
library(showtext)
library(dplyr)
library(ggplot2)

# comnect to database
PlayPen <- odbcConnect("PlayPen_prod")
sqlQuery(PlayPen, "use nzis11")

# load fonts
font.add.google("Poppins", "myfont")
showtext.auto()

inc <- sqlQuery(PlayPen, "select * from vw_mainheader") 

svg("../img/0006_better_density_plot.svg", 8, 5)
ggplot(inc, aes(x = income)) +
   geom_density() +
   geom_rug() +
   scale_x_continuous(trans = modulus_trans(lambda = 0.25), label = dollar) +
   theme_minimal(base_family = "myfont")
dev.off()

ggplot(inc, aes(x = hours, y = income)) +
   geom_jitter(alpha = 0.2) +
   scale_x_continuous(trans = modulus_trans(lambda = 0.25)) +
   scale_y_continuous(trans = modulus_trans(lambda = 0.25), label = dollar) +
   theme_minimal(base_family = "myfont")



p1 <- ggplot(inc, aes(x = hours, y = income)) +
   facet_wrap(~region) +
   geom_point(alpha = 0.2) +
   scale_x_continuous(trans = modulus_trans(k = 0.25)) +
   scale_y_continuous(trans = modulus_trans(k = 0.25), label = dollar) +
   theme_light(base_family = "myfont")

svg("../img/0006_income_by_region.svg", 10, 8)
   print(p1)
dev.off()

png("../img/0006_income_by_region.png", 10 * 70, 8 * 70, res = 70)
   print(p1)
dev.off()
