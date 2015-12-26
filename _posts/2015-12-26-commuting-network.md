---
layout: post
title: Network charts of commuting in New Zealand with R and D3
date: 2015-12-26
tag: 
   - DataFromTheWeb
   - NewZealand
   - OpenData
   - R
description: Commuting patterns between districts and cities in New Zealand are used to illustrate static (for printing) and interactive (for the web) network charts with R and D3.
image: /img/0025-commuting.svg
socialimage: http://ellisp.github.io/img/0025-commuting.png
category: R
---
## Commuting between districts and cities in New Zealand
<iframe src="/img/0025-networkD3.html" style = "overflow-y: hidden;" width = "100%" height = "430px"></iframe>

At this year's New Zealand Statisticians Association conference I gave a talk on [Modelled Territorial Authority Gross Domestic Product](http://www.mbie.govt.nz/info-services/sectors-industries/regions-cities/research/modelled-territorial-authority-gross-domestic-product).  One thing I'd talked about was the impact on the estimates of people residing in one Territorial Authority (district or city) but working in another one.  This was important because data on earnings  by place of residence formed a crucial step in those particular estimates of modelled GDP, which needs to be based on place of production.  I had a slide to visualise the "commuting patterns", which I'd prepared for that talk but isn't used elsewhere, and thought I'd share it and a web version here on this blog.

The web version is the one in the frame above this text.  It's designed to be interacted with - try hovering over circles, or picking them up and dragging them around.

## Data
The source data for this come from 2013 Census and are [published by Statistics New Zealand](http://www.stats.govt.nz/Census/2013-census/profile-and-summary-reports/commuter-view-visualisation.aspx), who have their own [rather nifty map-based visualisation](http://www.stats.govt.nz/datavisualisation/commuterview/index.html).  The data are published on the visualisation's information page and it's easy to grab the CSV and tidy it up in R:

{% highlight R lineanchors %}
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
{% endhighlight %}

## Drawing a static plot
![static](/img/0025-commuting.svg)
The static plot I used in my presentation is made with the excellent [{igraph} package](https://cran.r-project.org/web/packages/igraph/index.html) by Gabor Csadi.  There are lots of options for layouts; not being an expert in network graphs I used trial and error until I got something sufficiently visually striking.  Note that the locations of districts and cities are chosen to maximise use of white space given the various connections between them - they don't represent physical locations (which is possible, but less impact for my purpose).  

The eventual code is simple enough.  The "Travel" object I created in the previous chunk of code is in the right shape, with its "from" and "to" columns enough to define both the nodes and edges (ie links connecting nodes), and the "value" column is used to define the width of each edge.  There was a bit of finesse required with size and font settings to get it looking useful.  As it sits, it's still difficult to tell which way the arrows are pointing (most relationships are both ways of course, and all the arrows merge together into blobs for significant "target" authorities like Auckland), but it gets the job down of picturing which districts and cities are linked to which.
{% highlight R lineanchors %}
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
{% endhighlight %}

## Drawing an interactive plot
<iframe src="/img/0025-networkD3.html" style = "overflow-y: hidden;" width = "100%" height = "430px"></iframe>

The interactive plot is a bit fiddlier, but still a fairly straightforward task with the help of the [{networkD3} package](https://cran.r-project.org/web/packages/networkD3/index.html) by Christopher Gandrud et al.  This package uses the [htmlwidgets paradigm](http://www.htmlwidgets.org/) to make JavaScript D3 network graph visualisations easy to create.  This gives us access to the data management and analysis capabilities of R as well as the web visualisations of JavaScript.

There's a bit of extra work to be done here and it's not quite such an R-native feel as using {igraph}.  Most significantly, JavaScript (like most computer languages I know other than R) indexes its arrays starting at zero, not one, and the data frame that holds the "from" and "to" information for the edges needs to refer to nodes by number from zero to (n - 1).  There are many ways to do this but I find the easiest is to use Wickham's dplyr pipeline - as in the code that creates the "Links" data frame below.  Note that I'm also classifying the territorial authorities by island.

One of the strengths of an interactive plot is we can impart extra information by hover-over tooltips that would be way too cluttered in a static chart.  In this case, I've done a slightly ugly summary of an authority's total "to" and "from" movements from residence to place of work.  The code below creating "froms", "tos", "both" and "TAs" data frames is for this purpose, so the "label" column can be used in the visualisation for the actual tooltips.

{% highlight R lineanchors %}
# vector of south island districts so we can colour nodes by island:
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
{% endhighlight %}

Drawing a good looking network chart is easy enough, and just a matter of getting the data in the right shape and telling the forceNetwork() R function which columns to use as Source and Target for each edge / link, and which columns of a second data frame to use for Group (which controls colour), Nodesize, and NodeID (which controls the mouse hover tooltips).  Controlling colours requires us to create a D3 scale as a palette which needs a bit of basic JavaScript, which gets passed to forceNetwork via the colourScale = JS(...) snippet below.
{% highlight R lineanchors %}
pal <- 'd3.scale.ordinal()
         .domain(["South Island", "North Island"])
         .range(["#FF6900", "#694489"]);'

net <- forceNetwork(Links, Source = "from", Target = "to", Value = "value", 
             Nodes = TAs, NodeID = "label", Nodesize = "size", Group = "island",
             fontFamily = "Poppins", height = 350, width = 650, bounded = TRUE,
             colourScale = JS(pal))

saveNetwork(net, "../img/0025-networkD3.html", selfcontained = FALSE)
{% endhighlight %}

In the code above I've saved the resulting HTML to a file (and folders containing the various other assets eg JavaScript).  This file is basically ready to go, but as a final touch I want to insert a line into the header, just above the </head> tag, calling in the Cascading Style Sheets used in my blog, including the "Poppins" font from Google Fonts.  You'll have seen in the code above that I told the D3 network chart to use Poppins font, but unless it knows where to find it it can't do anything with this.  I also wanted to insert a heading into the body; my intended use for this HTML page was to be placed in an iframe in this blog post, and for visual coherence it needs a heading inside the iframe.

So I read the HTML back into R, insert the lines I want by find and replace with the gsub() function, and write it back to file, and I've managed to avoid editing any HTML by hand (useful if there are going to be lots of iterations during development).  

These last few snippets of code would need to be adapted to your particular folder structure, fonts and CSS if you want to copy them.

{% highlight R lineanchors %}
tmp <- readLines("../img/0025-networkD3.html")

tmp <- gsub('</head>', 
            '<link href="/css/bootstrap-theme.min.css" rel ="stylesheet"></head>', 
            tmp, fixed = TRUE)

tmp <- gsub('<div id="htmlwidget_container">',
            '<h3>Residents in one area, working in another</h3>
            <div id="htmlwidget_container">',
            tmp, fixed = TRUE)
            
writeLines(tmp, "../img/0025-networkD3.html")
{% endhighlight %}

If anyone's wondering why Wellington has so many inbound commuters compared to Auckland, which is much larger, it's just an artefact of boundaries.  Wellington Region is divided into a number of smaller cities and regions which show up in this sort of data, whereas the Auckland region is a single territorial authority.

C'est tout.

### Edits
Edited 27/12/2015 so the numbers in the tooltip labels include the sums of commuters even when less than 100 - incorrectly omitted in the first version.  I now use the pre-filtered "TravelAll" object for creating the labels.  This doesn't make any visual difference, but the numbers were understated before.  