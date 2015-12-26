##
##    Name:       plot_commuting_patterns.R
##
##    Objective:  Creates a network graph illustrating the major commuting connectivity between
##                Territorial Authories based on the 2013 Census.
##
##    Authors:    Peter Ellis, Sector Performance, Ministry of Business, Innovation & Employment
##
##    Date:       2015-06-26
##

##
##  1. Read in the commuting data from the 2013 Census
##
X <- read.csv("data_raw/2013-usual-residence-by-workplace-address-territorial-authority.csv",
         na.strings       = "..C",
         sep              = ",",
         stringsAsFactors = FALSE,
         check.names      = TRUE,
         header           = TRUE)

Travel <- X[1:67, ] %>% 
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
  mutate(value = as.numeric(value)) %>%
  filter(value > 100)




g <- Travel  %>%
  graph.data.frame(directed = TRUE)       

png("exploratory_output/commuting.png", 5000, 5000, res = 600)
par(family = "Calibri", mai = c(.2, .2, .2, .2))  
plot(g, edge.arrow.size = .4, layout = layout.davidson.harel,
       edge.width = sqrt(Travel$value) / 20,
       vertex.size = 0, edge.curved = TRUE,
       vertex.label.family = "Calibri",
       vertex.label.cex = .8,
       main = "Commuting patterns 2013 (>100 people commuting)"
       )       
dev.off()