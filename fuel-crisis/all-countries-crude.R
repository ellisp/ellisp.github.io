library(tidyverse)

df <- here("fuel-crisis/jodi-2026")
url <- "https://www.jodidata.org/_resources/files/downloads/oil-data/annual-csv/primary/primaryyear2026.csv"
download.file(url, destfile = df)

# following line crashes R
# d <- read_csv(df)
d <- read.csv(df) |> 
  as_tibble()

