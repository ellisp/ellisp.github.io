
# landmark 2010 paper https://www.researchgate.net/profile/Friedrich_Schneider/publication/227346997_New_Estimates_for_the_Shadow_Economies_All_over_the_World/links/09e415061fa38ccf13000000.pdf
# updated for just 36 richer countries:
# http://www.econ.jku.at/members/schneider/files/publications/2015/shadeceurope31.pdf

Use pdftools
http://ropensci.org/blog/2016/03/01/pdftools-and-jeroen


install.packages("pdftools")

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

svg("../img/0075-shadow-line.svg", 8.5, 8)
print(p1)
dev.off()



p2 <- p1 + facet_wrap(~countrylab, scales = "free_y") + 
   ggtitle("Shadow economy spike in 2008-2009",   
           "31 European and five non-European countries.   The percentage in each title is the value in 2015.")

svg("../img/0075-shadow-line-free.svg", 10, 8)
print(p2)
dev.off()

#=========convert to png=========

setwd("../img")
files <- list.files()
files <- files[grepl("^0075.+svg$", files)]
for(i in files){
   output <- gsub("svg$", "png", i)
   cmd <- paste0('\"C:\\Program Files\\ImageMagick-7.0.2-Q16\\magick\" -size 850x800', " ", i, " ", output)
   system(cmd)
   
}
setwd("../_working")
