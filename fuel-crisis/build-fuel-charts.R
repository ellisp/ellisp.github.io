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
