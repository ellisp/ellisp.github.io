---
layout: post
title: Getting started in applied statistics / datascience
date: 2015-08-22
tag: 
   - StatsEducation
   - R
description: Advice for people who want to work in big data, data science, applied statistics, etc (or managers of such people).  As an aside, I show how to draw with R Drew Conway's famous (well, it should be famous) Data Science Venn Diagram.
image: /img/0005_venndiagram.svg
category: R
---

## Advice when you don't know what to ask for
I was recently asked by a colleague manager from another organisation what direction they could give to a staff member interested in building skills in the whole "big data" thing.  A search of the web shows hundreds if not thousands of sites and blog posts aimed at budding data scientists, but most of them seem (to my admittedly very non-rigorous glance) to be collections of resources and techniques; too detailed and specific for my purposes, and aimed at people already a bit into the journey.  So here's something oriented a bit more to someone who's still wondering what this thing is you might be going to get into.

First, while a lot of the publicity you hear is about "big data", the real revolution in recent decades is bigger than big data.  It's about data creation, storage, access, analytical techniques and tools.  A lot of it is web-related (eg the revolution in ease of creating interactive statistical graphics), but not necessarily about enormous piles of Twitter and Facebook data.  So I'd be careful not to focus on "big data" from the start but instead build a core of statistics and computing skills which can then be applied to larger data.  The techniques actually specific to big data are relatively small compared to the core skillset, and should be fairly easy to learn if they get a solid grounding.

My thinking is heavily influenced by [Drew Conway's data science Venn diagram](http://www.dataists.com/2010/09/the-data-science-venn-diagram/), a modified version of which is below (at the bottom of this post is the R code that drew this):
![Datascience Venn diagram](/img/0005_venndiagram.svg)

Basically, a good applied statistician or datascientist (I'm not going to argue about language here) needs to combine computing, statistical and content knowledge skills.  It's the growth in computing power that's changing capabilities in the field, but knowing stuff and techniques is important too.

This series of [John Hopkins Coursera online courses](https://www.coursera.org/specialization/jhudatascience/1/courses) online courses has had good recommendations:  and covers the full range of things, using up to date tools.  It's a commitment, but the fact is there's a lot to learn.  I'd suggest maybe getting enrolled in that or a similar course to see if they've got the stomach for it.  If they haven't written computer code before, for example, there's probably a particular psychological hurdle to overcome before they decide this is for them (and you can’t handle data properly without doing it in code).

The "range of things" as I would see them (which is pretty much identical to that course linked above) would be:

### Statistics
Learning statistics properly takes effort which may well turn into formal tertiary education at some point.  One problem is a lot of the statistics learnt at university in non-statistics degrees teaches techniques rather than principles, and often dated at that.   

* As a starter (reflecting my own learning preferences of course ) I advise reading some books. Like [Wilcox's Modern Statistics for the Social and Behavioral Sciences](http://www.amazon.com/Modern-Statistics-Social-Behavioral-Sciences/dp/1439834563)  and [Andy Field's Discovering Statistics Using R](http://www.amazon.com/Discovering-Statistics-Using-Andy-Field/dp/1446200469), both of which I've used successfully with people and learnt stuff myself on the way.  Those are really good, modern introductory texts, particularly Wilcox's which played a big part in nudging my own statistical approaches into the modern world.   
* Use the amazing [Cross-Validated Q&A site](http://stats.stackexchange.com/) to search for answers to statistical questions 
* [Wikipedia](https://en.wikipedia.org/wiki/Statistics) for excellent technical definitions and descriptions


### A computing tool for statistics
A choice needs to be made for a computer language in which to start learning.  If you spend more than an hour thinking about R v SAS, R v Python, or R v Julia you're wasting your time because the reality is if you're going to get any good at this, you need to be multilingual.   However, you have to start somewhere and my recommendation is for R.  It's free, easy to get, forms the lingua franca in the academy, and its open source approach means new techniques get operationalised in it far quicker than in SAS, as do bindings to other languages like JavaScript (pretty much essential for fancy modern data presentation).   Ideally you learn R and statistics together – R is a computer language written by statisticians for statisticians, so it helps you fall into a statistical way of thinking.

* Download R from [CRAN](https://cran.r-project.org/)  and RStudio from [RStudio](https://www.rstudio.com/products/RStudio/).   
* Follow this blog aggregation site: [http://www.r-bloggers.com/](http://www.r-bloggers.com/)
* Use this [Stack Overflow Q&A](http://stackoverflow.com/)
* Join your local R users group and other analytics groups.  Where I live, that means the [Wellington R users group](http://www.meetup.com/Wellington-R-Users-Group-WRUG/)  and the (language-agnostic) [New Zealand analytics forum](http://analytics.org.nz/), both of which have 3 or 4 events a year to hear what others around are up to and make contacts

### Reproducible research eg version control, making things reproducible end to end, etc.

* Download [Git](http://www.git-scm.com/) and once you've started to get comfortable with writing code, learn to use Git to do version control.  It integrates well with RStudio.
* Read about [reproducible research](https://en.wikipedia.org/wiki/Reproducibility#Reproducible_research) and find ways to set up your code so others can repeat what you've done - for peer review, quality control, scalability, and updating stuff.
* Install LaTeX and learn how to use it.
* As you get further into larger projects you need to start borrowing techniques from software developers, and read up on [software development methods](https://en.wikipedia.org/wiki/Software_development_process) like [Extreme Programming](https://en.wikipedia.org/wiki/Extreme_programming).  When analysts realise they are writing computer programs, not just interacting with a statistical tool, they move up a level in the power of what they can do.

### Databases, data management, SQL, tidying and cleaning data

* Down the track, you need database skills.  This can wait until familiar with stats and R but at some point you need to be able to set up a database.  MySQL https://www.mysql.com/ is the easiest one to do this on a home system.
* Within R, {dplyr} and {tidyr} are relatively new but are king for data tidying, reshaping and accessing.  Most recent online courses and blogs will use these.
* Only after everything else is sorted and are familiar with medium size data (eg traditional relational databases like MySQL), can think about big-data-specific things like Hadoop clusters.

### More
So that's only a beginning.  More goes here


### Drawing that diagram
Finally, here's the code that drew my own version of the Drew Conway data science diagram.   I wanted to tweak his original 

* to avoid the argument "that's just what applied statisticians do".
* to make clearer than Conway's original that hacking in combination with statistics is still a danger zone


{% highlight R lineanchors %}
library(showtext)
library(grid)
library(RColorBrewer)

font.add.google("Poppins", "myfont")
showtext.auto()
palette <- brewer.pal(3, "Set1")

radius <- 0.3
strokecol <- "grey50"
linewidth <- 4
fs <- 11

draw_diagram <- function(){
grid.newpage()
grid.circle(0.33, 0.67, radius, gp = 
               gpar(col = strokecol,
                    fill = palette[1],
                    alpha = 0.2,
                    lwd = linewidth))

grid.circle(0.67, 0.67, radius, gp =
            gpar(col = strokecol,
                 fill = palette[2],
                 alpha = 0.2,
                 lwd = linewidth))

grid.circle(0.5, 0.33, radius, gp =
               gpar(col = strokecol,
                    fill = palette[3],
                    alpha = 0.2,
                    lwd = linewidth))

grid.text("Hacking", 0.25, 0.75, rot = 45, gp =
             gpar(fontfamily = "myfont",
                  fontsize = fs * 2.3,
                  col = palette[1],
                  fontface = "bold"))

grid.text("Statistics", 0.75, 0.75, rot = -45, gp =
             gpar(fontfamily = "myfont",
                  fontsize = fs * 2.3,
                  col = palette[2],
                  fontface = "bold"))

grid.text("Content\nknowledge", 0.5, 0.25, rot = 0, gp =
             gpar(fontfamily = "myfont",
                  fontsize = fs * 2.3,
                  col = palette[3],
                  fontface = "bold"))

grid.text("Danger:\nno context", 0.5, 0.75,
          gp = gpar(fontfamily = "myfont",
                    fontsize = fs))

grid.text("Danger: no\nunderstanding\nof probability", 0.32, 0.48, rot = 45,
          gp = gpar(fontfamily = "myfont",
                    fontsize = fs))

grid.text("Traditional\nresearch", 0.66, 0.46, rot = -45,
          gp = gpar(fontfamily = "myfont",
                    fontsize = fs))


grid.text("Data science /\napplied\nstatistics", 0.5, 0.55, 
          gp = gpar(fontfamily = "myfont",
                    fontsize = fs * 1.2,
                    fontface = "bold"))
}

draw_diagram()


{% endhighlight %}