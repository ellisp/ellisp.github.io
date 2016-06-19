library(showtext)
library(RMySQL)
library(ggplot2)
library(scales)
library(dplyr)
library(tidyr)
library(Cairo)
library(stringr)
library(ca)     # for ca
library(vegan)  # for the cca alternative
library(gplots) # for balloon plot
library(FactoMineR) # for the CA function
library(factoextra)
PlayPen <- dbConnect(RMySQL::MySQL(), username = "analyst", dbname = "nzis11")

font.add.google("Poppins", "myfont")
showtext.auto()

# I draw on http://www.sthda.com/english/wiki/correspondence-analysis-in-r-the-ultimate-guide-for-the-analysis-the-visualization-and-the-interpretation-r-software-and-data-mining



dtf <- dbGetQuery(PlayPen,
                  "SELECT
                  ROUND(COUNT(1) * 117.44, 0) as population,
                  ethnicity,
                  occupation
                  FROM f_mainheader m
                  JOIN f_ethnicity e ON m.survey_id = e.survey_id
                  JOIN d_ethnicity d ON e.ethnicity_id = d.ethnicity_id
                  JOIN d_occupation o on m.occupation_id = o.occupation_id
                  GROUP BY ethnicity, occupation")


with(dtf, balloonplot(ethnicity, occupation, population))

dtf %>%    spread(occupation, population)

tab <- dtf %>%
   mutate(ethnicity = gsub( "Residual Categories", "Other", ethnicity),
          ethnicity = ifelse(grepl("Latin American", ethnicity), "MELAA", ethnicity),
          ethnicity = gsub(" Ethnicity", "", ethnicity),
          ethnicity = gsub(" Peoples", "", ethnicity),
          occupation = gsub( "Residual Categories", "Other", occupation),
          occupation = ifelse(grepl("Machinery", occupation), "Machine & Drivers", occupation),
          occupation = ifelse(grepl("Community", occupation), "Cmnty & Personal", occupation),
          occupation = ifelse(grepl("Clerical", occupation), "Clerical & Admin", occupation),
          occupation = ifelse(grepl("Technician", occupation), "Technician & Trades", occupation)) %>%
   # we now have 2 "Other" ethnicities: the original "Other" and the "Residual Ethnicities"
   group_by(ethnicity, occupation) %>%
   summarise(population = sum(population)) %>%
   ungroup() %>%
   spread(occupation, population) 

tab_mat <- as.matrix(tab[ , -1])
rownames(tab_mat) <- tab$ethnicity
# some NAs in the 'residual categories' can reasonably be turned into zeroes
tab_mat[is.na(tab_mat)] <- 0

mosaicplot(tab_mat, shade = TRUE, las = 2)

# there are NA vfalues for MELAA, Pacific and residual ethnicity with reisdual occupation.
# convert to zeros


# from ca package
res1 <- ca(tab_mat)
summary(res1)
pal <- c("orange", "steelblue")

par(bty = "l")
plot(res1, col.lab = pal, col = pal)

# from vegan package
res2 <- cca(as.data.frame(tab_mat))
plot(res2, scaling = 3)
# if you want to extract them by hand:
scores(res2, scaling = 3)
summary(res2)

# from FactoMineR
res3 <- CA(tab_mat, graph = FALSE)
summary(res3)
plot(res3)

# ggplot version. To polish fonts need to make it the ggplot2 default of geom_text
p1 <- fviz_ca_biplot(res3, repel = TRUE, arrows = c(TRUE, FALSE),
                     col.col = "orange", col.row = "steelblue") + 
   theme_minimal(base_family = "Calibri" ) +
   ggtitle("") +
   coord_equal()
print(p1)

# good description here http://stats.stackexchange.com/questions/3270/interpreting-2d-correspondence-analysis-plots

# we can use the first dimenstion, which  explains 78% of the variance, 
# to sequence ethnicity and occupation and give a better looking mosaic
# plot:

eth_ordered <- names(sort(scores(res2)$sites[ , 1], decreasing = TRUE))
occ_ordered <- names(sort(scores(res2)$species[ , 1], decreasing = TRUE))

tab_mat2 <- tab_mat[eth_ordered , occ_ordered]


svg("../img/0045-mosaic-plus-ca.svg", 11,11)
# mosaicplot uses the order of the columns and rows (not eg
# like ggplot, which uses the order of levels of factors)
par(family = "Calibri", mfrow = c(2, 1), cex = 0.9)
update_geom_defaults("text",   list(family = "Calibri"))


mosaicplot(tab_mat2, shade = TRUE, las = 1, 
           main = "Relationship of ethnicity and occupation in the New Zealand Income Survey 2011",
           border = "grey90")
# todo - solidify the dashed lines. Probably would need to hack graphics::mosaicplot to do this.
#      - change legend title from "Standardized residuals" to "Standardized difference from average"

cp2 <- grid::viewport(0.5, 0.3, h = 0.55, w = 1)
print(p1, vp = cp2)
dev.off()