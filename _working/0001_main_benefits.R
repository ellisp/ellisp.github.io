library(XLConnect)
library(dplyr)
library(tidyr)
library(shinyapps)

#-----------------import data---------------
# Original URL. Use of paste0 here is just to make readable in narrow screen:
url <- paste0(
   "https://www.msd.govt.nz/documents/about-msd-and-our-work/publications-resources",
   "/statistics/benefit/2015/quarterly-benefit-fact-sheets-national-benefit-tables-june-2015.xls"
   )

# Note use of wb mode (for binary file)
download.file(url = url, destfile = "benefits.xls", mode = "wb")

# Import into R
wb <- loadWorkbook("benefits.xls")
benefits_orig <- readWorksheet(wb, "Main benefits - last 5 years",
                          startRow = 4, endRow = 21,        # excluding total deliberately
                          startCol = 2, endCol = 23,
                          dateTimeFormat = "%Y-%m")


#-----------------tidy----------------
#

# First we need to deal with where merged cells cause problems.  For example, the "Gender" row
# has the values for "Male", and "Male" row is all NA.  We get round this by changing the word "Gender"
# to "Male" and adding a new Category column with "Gender" in it, until we get to the next heading.
# Meanwhile we note the original rows of NAs to knock them out
benefits <- benefits_orig
knockout <- numeric()
Headings <- c("Gender", "Ethnic Group", "Age Group", "Continuous Duration")

for(i in 1:nrow(benefits_orig)){
   
   if(benefits_orig[i, "Col1"] %in% Headings){
      
      benefits[i, "Col1"] <- benefits[i + 1, "Col1"]
      knockout <- c(knockout, i + 1)
      last_category <- benefits_orig[i, "Col1"]
      }
   benefits[i, "Category"] <- last_category
   }
benefits <- benefits[-knockout, ] # at this point should visually check with View(benefits) matches Excel version

# put into tidy format and clean up the inconsistent Dates
benefits <- benefits %>%
   gather(Date, Value, -Col1, -Category) %>%
   rename(Variable = Col1) %>%
   mutate(Date = as.character(Date),
          Date = gsub("X", "", Date),
          Date = gsub("Jun.15", "2015.06", Date, fixed = TRUE),
          Date = gsub("Mar.15", "2015.03", Date, fixed = TRUE),
          CentreMonth = as.numeric(substring(Date, 6, 7)) - 1, # central month of each quarter
          Year = as.numeric(substring(Date, 1, 4)),
          Period = Year + (CentreMonth - 0.5) / 12)  %>%          # decimal representation of mid-quarter
   select(-CentreMonth, -Date, -Year)

#---------------------analyse--------------------
save(benefits, file = "_output/0001-shiny-benefits/benefits.rda")

deployApp("_output/0001-shiny-benefits", appName = "0001-shiny-benefits", account = "ellisp")
