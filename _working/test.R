library(ggseas)
library(ggthemes)
library(extrafont)

plot(ldeaths)

ggplot(mtcars, aes(x = disp, y = mpg, colour = as.factor(cyl))) +
   geom_point() +
   theme_economist(base_family = "Calibri")

var(mtcars$mpg)   
is.function("ggplot")

x <- NULL
if(TRUE){
	print(3:8 * 5)
}