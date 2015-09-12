library(knitr)

library(RODBC)


knit2html("../_knitr/2015-09-13-inequality-stats-distributions.rmd", 
          output = "../_posts/2015-09-13-inequality-stats-distributions.html",
          stylesheet = "",
          fragment.only = TRUE)
