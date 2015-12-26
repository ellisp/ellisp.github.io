library(dplyr)
library(tidyr)
library(showtext)
library(igraph)
library(networkD3)

# import fonts
font.add.google("Poppins", "myfont")
showtext.auto()

# download data from Statistics New Zealand
X <- read.csv("http://www.stats.govt.nz/~/media/Statistics/Census/2013%20Census/profile-and-summary-reports/commuter-view-interactive/2013-usual-residence-by-workplace-address-territorial-authority.csv",
              na.strings       = "..C",
              sep              = ",",
              stringsAsFactors = FALSE,
              check.names      = TRUE,
              header           = TRUE)

# Tidy into long form, remove clutter, make names consistent, 
# drop uninteresting data:
TravelAll <- X[1:67, ] %>% 
  rename(from = Usual.residence) %>%
  gather(to, value, -from) %>%
  mutate(from = gsub(" District", "", from),
         from = gsub(" City", "", from),
         from = gsub(" Territory", "", from),
         to = gsub(".", " ", to, fixed = TRUE),
         to = gsub(" Territory", "", to),
         to = gsub(" District", "", to),
         to = gsub(" City", "", to),
         to = gsub("Hawke s", "Hawke's", to, fixed = TRUE),
         to = gsub("Matamata Piako", "Matamata-Piako", to),
         to = gsub("Queenstown Lakes", "Queenstown-Lakes", to),
         to = gsub("Thames Coromandel", "Thames-Coromandel", to)) %>%
  filter(from != to &
           to != "Total workplace address" &
           to != "No Fixed Workplace Address" &
           to != "Area Outside Territorial Authority") %>%
  filter(!grepl("Not Further Defined", to)) %>%
  mutate(value = as.numeric(value),
         value = ifelse(is.na(value), 0, value)) 

# subset dropping the small values:
Travel <- TravelAll %>% 
  filter(value > 100)



draw_plot <- function(seed = 125){
   set.seed(seed) # there's some randomness in the graphing layout
   g <- Travel  %>%
      graph.data.frame(directed = TRUE)       
   par(family = "myfont", mai = c(.2, .2, .2, .2))  
   plot(g, edge.arrow.size = .4, layout = layout.davidson.harel,
        edge.width = sqrt(Travel$value) / 20,
        vertex.size = 0, edge.curved = TRUE,
        vertex.label.family = "myfont",
        vertex.label.cex = .8,
        main = "Commuting patterns 2013 (>100 people commuting)"
   )          
}

png("../img/0025-commuting.png", 830, 830, res = 100)
   draw_plot()
dev.off()

svg("../img/0025-commuting.svg", 8.3, 8.3)
   draw_plot()
dev.off()


#----------------D3 version-------------------

southisland <- c("Dunedin", "Tasman", "Nelson", "Marlborough", "Christchurch", 
                 "Queenstown-Lakes", "Invercargill", "Grey",
                 "Westland", "Waimakariri", "Hurunui", "Selwyn",
                 "Ashburton", "Timaru", "Mackenzie", "Waimate", "Central Otago",
                 "Waitaki", "Clutha", "Gore", "Southland", "Buller")

# create a look up table of TA names and ID 0:(n-1)
TAs <- data_frame(TA = unique(c(Travel$from, Travel$to))) 
TAs <- TAs %>%
   mutate(ID = 0:(nrow(TAs) - 1),
          size = 1,
          island = ifelse(TA %in% southisland, "South Island", "North Island"))


# create a data frame with numbers 0 : n-1 instead of names of TAS
Links <- Travel %>%
   left_join(TAs, by = c("from" = "TA")) %>%
   select(-from) %>%
   rename(from = ID) %>%
   left_join(TAs, by = c("to" = "TA")) %>%
   select(-to) %>%
   rename(to = ID) %>%
   mutate(value = value / 200)

# create some totals for more informative tooltips:
froms <- TravelAll %>%
   group_by(from) %>%
   summarise(From = sum(value)) %>%
   ungroup() %>%
   rename(TA = from)

tos <- TravelAll %>%
   group_by(to) %>%
   summarise(To = sum(value)) %>%
   ungroup() %>%
   rename(TA = to)

both <- full_join(froms, tos, by = "TA") %>%
   mutate(label = paste0(TA, ". From: ", From, "; To: ", To, ".")) 

TAs <- TAs %>%
   left_join(both, by = "TA")

pal <- 'd3.scale.ordinal()
         .domain(["South Island", "North Island"])
         .range(["#FF6900", "#694489"]);'

net <- forceNetwork(Links, Source = "from", Target = "to", Value = "value", 
             Nodes = TAs, NodeID = "label", Nodesize = "size", Group = "island",
             fontFamily = "Poppins", height = 350, width = 650, bounded = TRUE,
             colourScale = JS(pal))


saveNetwork(net, "../img/0025-networkD3.html", selfcontained = FALSE)

tmp <- readLines("../img/0025-networkD3.html")
tmp <- gsub('</head>', 
            '<link href="/css/bootstrap-theme.min.css" rel ="stylesheet"></head>', 
            tmp, fixed = TRUE)


tmp <- gsub('<div id="htmlwidget_container">',
            '<h3>Residents in one area, working in another</h3>
            <div id="htmlwidget_container">',
            tmp, fixed = TRUE)
writeLines(tmp, "../img/0025-networkD3.html")


