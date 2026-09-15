# Weekly (or more frequent) build of the plots for the fuel crisis monitoring
# page.

setwd(here("_working"))


# an annotation rectangle we can use in multiple charts
war_rect <- annotate(
  "rect",
  xmin = as.Date("2026-02-28"),
  xmax = as.Date(Inf),
  ymin = -Inf,
  ymax = Inf,
  alpha = 0.5,
  fill = "grey80"
)


source("0325-cushing.R")
source("0327-us-oil-inventories.R")
source("0328-pacific-cpi.R")
source("0329-nz-us-petrol.R")

setwd(here())
system2("jekyll", "build")

# check to see if anything has change
setwd(here("_site"))
system("git status")

# Then in terminal navigate to _site and use gitk to check in more detail for changes.

setwd(here("_working"))
