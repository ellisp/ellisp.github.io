library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)
library(grid)
library(gridExtra)
library(quantmod)


source("https://gist.github.com/ellisp/4002241def4e2b360189e58c3f461b4a/raw/0e5dc52262e86b5d21b914db3553e58b0b8d1d20/dualplot.R")     

# Issues
# improve automatic legend titles (done but needs regression testing)
# axis labels for x axis when it is a time (done but needs regression testing)
# indexing work even for xts or zoo objects

# different starting periods


# negative values

# Fonterra were publicly listed
fonterra <- getSymbols('FCG.NZ', src='yahoo', auto.assign = FALSE) 
airnz <- getSymbols('AIR.NZ', src='yahoo', auto.assign = FALSE) 

# default - a cross over point happen at the earliest point of the shorter series
dualplot(x1 = time(fonterra), x2 = time(airnz), y1 = fonterra$FCG.NZ.Close, y2 = airnz$AIR.NZ.Close)

# or the other way around:
dualplot(x2 = time(fonterra), x1 = time(airnz), y2 = fonterra$FCG.NZ.Close, y1 = airnz$AIR.NZ.Close)

# or can override eg - each one begins at the same vertical height on its earliest point
dualplot(x1 = time(fonterra), x2 = time(airnz), y1 = fonterra$FCG.NZ.Close, y2 = airnz$AIR.NZ.Close,
         ylim.ref = c(1, 1))

# or other ways of forcing the cross over point
dualplot(x1 = time(fonterra), x2 = time(airnz), y1 = fonterra$FCG.NZ.Close, y2 = airnz$AIR.NZ.Close,
         ylim.ref = c(nrow(fonterra), nrow(airnz)))

         
args(dualplot)

plot(time(fonterra), fonterra$FCG.NZ.Close)

# different frequencies and starting periods


# not for barcharts




# where to put dual plot


