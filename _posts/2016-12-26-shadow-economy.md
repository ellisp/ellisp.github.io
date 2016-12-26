---
layout: post
title: Extracting data on shadow economy from PDF tables
date: 2016-12-26
tag: 
   - R
   - OpenData
   - Timeseries
description: The shadow economy as a percentage of GDP in wealthier countries is in decline; and had a spike in 2009 with the economic crisis.  More research is needed to adequately understand it.  Along the way I experiment with extracting data frames from PDF tables; and show it's always worthwhile looking at the same data in different ways, which can be as simple as freeing up the vertical axes of graphics.
image: /img/0075-shadow-line.svg
socialimage: http://ellisp.github.io/img/0075-shadow-line.png
category: R
---

## Data on the shadow economy?
I'm reading Kenneth Rogoff's [The Curse of Cash](http://press.princeton.edu/titles/10798.html).  It was one of Bloomberg's Best Books of 2016 and the Financial Times' Best Economics Books of 2016, and I recommend it.  It's an excellent and convincing book, making the case for getting rid of large denomination notes for three reasons:

* to put the squeeze on the shadow economy (everything from breaches of labour standards, visa requirements, and tax evasion)
* make life harder for organised crime
* make negative interest rates a credible weapon in the armoury of central banks.  

If you're interested, there's also a [talk at the London School of Economics](http://www.lse.ac.uk/website-archive/newsAndMedia/videoAndAudio/channels/publicLecturesAndEvents/player.aspx?id=3663) from 23 November 2016 that's worth listening to or watching; actually that talk was what prompted me to buy the book for holiday reading.

The book has prompted me to follow up something I've wondered about for a while, which is "how do people try to measure the shadow economy?". One of the broadest definitions of the shadow economy is: 

> ["...those economic activities, and the income derived from them, that circumvent or otherwise avoid government regulation, taxation or observation"](http://ftp.iza.org/dp6423.pdf).  

This includes both legal and illegal economic activities.  What I am interested in here is the measurement of otherwise legal economic activities, such as cash payments for the purpose of tax-evasion, rather than intrinsically illegal activities, such as drug trafficking.

Estimating the size of this shadow economy is fraught with obvious difficulties and there is no universally accepted way of doing it.  Rogoff cites research by Friedrich Schneider and colleagues.  A bit of googling shows that their approach seems to be pretty systematic and mature, and takes into account a range of previous efforts.  It looks like Schneider et al's 2010 [*New Estimates for the Shadow Economies all over the World*](https://www.researchgate.net/profile/Friedrich_Schneider/publication/227346997_New_Estimates_for_the_Shadow_Economies_All_over_the_World/links/09e415061fa38ccf13000000.pdf) is a seminal paper, with the data reproduced or cited by a range of researchers and NGOs.  An updated consideration of methodological issues is in Schneider and Buehn's 2016 [*Estimating the Size of the Shadow
Economy: Methods, Problems and Open Questions*](http://ftp.iza.org/dp9820.pdf).

Figures in the 2010 paper were improved and updated in the short January 2015 paper [*Size and Development of the Shadow Economy of
31 European and 5 other OECD Countries from 2003 to 2015: Different Developments*](http://www.econ.jku.at/members/schneider/files/publications/2015/shadeceurope31.pdf).  It includes tables with estimates from 2003 to 2015 of the shadow economy for 28 European Union (EU), three non-EU, and five non-European countries.  There's only a basic barchart of average values in that paper, but here's a graphic I made that I think summarises the data:

![plot](/img/0075-shadow-line.svg)

It's pretty self-explanatory.  The universal (for these generally wealthier countries) downwards trend over time is the obvious feature.  The inter-country variations won't surprise anyone aware of the different institutional arrangements and cultural practices across these countries.  Worth noting is that value-added taxes in the USA are low by international standards, with proportionately more government revenue coming from income tax.  Rogoff and others argue that lower incentives to avoid sales and value-added taxes can partly explain the relatively low level of this kind of shadow economy activity in countries like the USA.  A cursory glance at [VAT rates in wikipedia](https://en.wikipedia.org/wiki/Value-added_tax) suggests this factor, if it exists, is not a particularly important one in explaining differences across countries.  A question I may follow up one day.

A close up view of the same data, by allowing a tailored vertical axis for each country, draws attention to micro structural influences on this kind of shadow economic activity.  Note the upward blips across countries around the time of the recent international financial crises in 2008-2009.  

![plot2](/img/0075-shadow-line-free.svg)


According to Schneider the decrease since the 2008-2009 economic crisis can be explained as follows:

> "...the most important reason for this decrease is that if the official economy is recovering or booming, people have fewer incentives to undertake additional activities in the shadow economy and to earn extra "black" money."

I'm sure Schneider is correct that economic boom and bust has a short term impact, but the general trend downwards over the longer period (2003 onwards) makes it clear that something more substantive is going on.

## Data extraction from PDF files

Extracting the data was a pain because it is in three tables in a PDF document.  My strategy to make this easier was to: 

* download and save the PDF
* use the extremely excellent and useful [`pdftools`](http://ropensci.org/blog/2016/03/01/pdftools-and-jeroen) R package to import the article into R as a list of character vectors, with one element of the list per page of the PDF
* use regular expressions to extract and tidy up tables 1, 2 and 3 from pages 6 and 7 (most importantly, replace sequences of adjacent spaces with a single "pipe" `|`)
* take advantage of those `|` delimiters to save each table as its own pipe-delimited text file
* re-import back to R as three different data frames, combine and tidy up for analysis
* check final results against the original PDF.

As far as I'm aware there's no general and robust way of converting the wide variety of tables in PDF documents into machine-usable data, but this approach could be used in quite a wide variety of situations.  There's a bit of manual adjustment required (for example, dealing with the spaces in "New Zealand" and "Czech Republic", which I do in quite an ad hoc way given there are only a few to take into account).  Here's the code for the data import and table extraction:

{% highlight R %}
#-----functionality------------------
library(pdftools)
library(dplyr)
library(tidyr)

#------download file and bring into R------------
download.file("http://www.econ.jku.at/members/schneider/files/publications/2015/shadeceurope31.pdf",
              destfile = "shadeceurope31.pdf", mode = "wb")

shade <- pdf_text("shadeceurope31.pdf")

#---------Import and clean up page six, which is Table 1------------
tab1 <- shade[[6]]
# replace spaces in country names with under scores
tab1 <- gsub("Luxembourg (Grand", "Luxembourg_(Grand", tab1, fixed = TRUE)
tab1 <- gsub("Czech Republic", "Czech_Republic", tab1)
tab1 <- gsub("United Kingdom", "United_Kingdom", tab1)
# replace all other sets of 1 or more spaces with a single | delimieter
tab1 <- gsub(" * ","|", tab1)

# save as a text file
writeLines(tab1, "tab1.txt")

# read back in
eur <- read.delim("tab1.txt", sep = "|", nrows = 28, skip = 1)
names(eur) <- c("empty", "country", 2003:2015)
eur <- eur[,2:15] 
eur$country <- gsub("Luxembourg.*", "Luxembourg", eur$country)

#--------Import and clean up page seven, which is Tables 2 and 3--------------
page7 <- shade[[7]]
tab2 <- gsub("Source:.*", "", page7)



# Table 2 clean up, export, import
tab2 <- gsub(" * ", "|", tab2)
writeLines(tab2, "tab2.txt")
noneu <- read.table("tab2.txt", skip = 2, nrows = 3, sep = "|")
names(noneu) <- names(eur)

# Table 3 clean up, export, import
tab3 <- gsub(".*Table 3", "", page7)
tab3 <- gsub("New Zealand", "New_Zealand", tab3)
tab3 <- gsub("United States USA", "USA", tab3)
tab3 <- gsub(" * ", "|", tab3)

writeLines(tab3, "tab3.txt")
noneur <-   read.table("tab3.txt", skip = 2, nrows = 5, sep = "|")
names(noneur) <- names(eur)

# clean up:
unlink(c("tab1.txt", "tab2.txt", "tab3.txt"))
{% endhighlight %}

## Analysis

Once the data are available in the same shape (in the three data frames `eur`, `noneu` and `noneur`), combining and tidying up for analysis is straightforward.

{% highlight R %}
#======set up data for analysis=======
# create long, tidy version of the basic data
shadow <- rbind(eur, noneu, noneur) %>%
   mutate(classification = rep(c("European Union (EU)", "Non-EU European", "Non-European"), times = c(28, 3, 5))) %>%
   mutate(country = gsub("_", " ", country)) %>%
   gather(year, value, -country, -classification) %>%
   mutate(value = value / 100,
          year = as.numeric(year))

# totals, latest, average - summary version, use for ordering factors in the graphic
totals <- shadow %>%
   group_by(country) %>%
   summarise(ave = mean(value),
             latest = value[length(value)]) %>%
   mutate(label = paste0(country, " ", round(latest * 100), "%"))
   arrange(desc(ave))

# create factors with ordered levels for using in the graphic
shadow <- shadow %>%
   mutate(country = factor(country, levels = totals$country),
          countrylab = factor(as.numeric(country), labels = totals$label))

#=================analysis=================
p1 <- ggplot(shadow, aes(x = year, y = value, colour = classification)) +
   facet_wrap(~countrylab) +
   geom_line(size = 1.1) +
   scale_y_continuous(label = percent) +
   scale_x_continuous(breaks = c(2005, 2010, 2015)) +
   scale_colour_brewer("", palette = "Set2") +
   ggtitle("Slowly decreasing shadow economy over time",
           "31 European and five non-European countries.   The percentage in each title is the value in 2015.") +
   labs(x = "", y = "Estimated shadow economy as percent of official GDP",
        caption = "Source: Schneider 2015\nhttp://www.econ.jku.at/members/schneider/files/publications/2015/shadeceurope31.pdf")
		
p2 <- p1 + facet_wrap(~countrylab, scales = "free_y") + 
   ggtitle("Shadow economy spike in 2008-2009",   
           "31 European and five non-European countries.   The percentage in each title is the value in 2015.")

print(p1)		   
print(p2)
{% endhighlight %}
