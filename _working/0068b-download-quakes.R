
#================download data==================
urls <- readLines("../data/earthquake-data-urls.txt")

tmp <- list()

# this takes a few minutes to download all the data from GNS and isn't necessarily fair on them,
# so maybe download my pre-done version first, and find a way to just add the last year?
for(i in 1:length(urls)){
   cat(i)
   tmp[[i]] <- read.csv(urls[i])   
   Sys.sleep(1) # to avoid hitting the server before it's had a breather
}
quakes_orig <- tmp
save(quakes_orig, file = "../data/quakes_orig.rda")

quakes_bound <- do.call("rbind", quakes_orig)

# note they seem to be using -9 as an NA value
quakes_df <- quakes_bound %>%
   mutate(day = as.Date(substring(origintime, 1, 10))) %>%
   filter(longitude > 166 & longitude < 180 & latitude > -48 & latitude < -32) %>%
   select(longitude, latitude, magnitude, depth, day, evaluationmethod, eventtype) %>%
   mutate(magnitude = ifelse(magnitude < 0, NA, magnitude),
          energy = magn_to_megat(magnitude)) 
save(quakes_df, file = "../data/quakes_df.rda")
