library(dplyr)
library(RODBC)
library(tidyr)
library(mbie) # for AskCreds, which is alternatively directly available 
              # https://github.com/nz-mbie/mbie-r-package/blob/master/pkg/R/Creds.R 
library(xtable)

# imports, clean up, and save to database the data from
# http://www.stats.govt.nz/tools_and_services/microdata-access/nzis-2011-cart-surf.aspx

url <- "http://www.stats.govt.nz/~/media/Statistics/services/microdata-access/nzis11-cart-surf/nzis11-cart-surf.csv"
nzis <- read.csv(url)


#-----------------fact tables------------------
# Create a main table with a primary key

f_mainheader <- nzis %>%
   mutate(survey_id = 1:nrow(nzis))

# we need to normalise the multiple ethnicities, currently concatenated into a single variable
cat("max number of ethnicities is", max(nchar(nzis$ethnicity)), "\n")

f_ethnicity <- f_mainheader %>%
   select(ethnicity, survey_id) %>%
   mutate(First = substring(ethnicity, 1, 1),
          Second = substring(ethnicity, 2, 2)) %>%
   select(-ethnicity) %>%
   gather(ethnicity_type, ethnicity_id, -survey_id) %>%
   filter(ethnicity_id != "") 
   
# drop the original messy ethnicity variable and tidy up names on main header
f_mainheader <- f_mainheader %>%
   select(-ethnicity) %>%
   rename(region_id = lgr,
          sex_id = sex,
          agegrp_id = agegrp,
          qualification_id = qualification,
          occupation_id = occupation)


#-----------------dimension tables------------------
# all drawn from the data dictionary available at the first link given above
d_sex <- data_frame(sex_id = 1:2, sex = c("male", "female"))

d_agegrp <- data_frame(
   agegrp_id = seq(from = 15, to = 65)) %>%
   mutate(agegrp = ifelse(agegrp_id == 65, "65+", paste0(agegrp_id, "-", agegrp_id + 4)))

d_ethnicity <- data_frame(ethnicity_id = c(1,2,3,4,5,6,9),
                          ethnicity = c(
                             "European",
                             "Maori",
                             "Pacific Peoples",
                             "Asian",
                             "Middle Eastern/Latin American/African",
                             "Other Ethnicity",
                             "Residual Categories"))


d_occupation <- data_frame(occupation_id = 1:10,
                       occupation = c(
                          "Managers",
                          "Professionals",
                          "Technicians and Trades Workers",
                          "Community and Personal Service Workers",
                          "Clerical and Adminsitrative Workers",
                          "Sales Workers",
                          "Machinery Operators and Drivers",
                          "Labourers",
                          "Residual Categories",
                          "No occupation"                          
                       ))


d_qualification <- data_frame(qualification_id = 1:5,
                        qualification = c(
                           "None",
                           "School",
                           "Vocational/Trade",
                           "Bachelor or Higher",
                           "Other"
                        ))

d_region <- data_frame(region_id =1:12,
                       region = c("Northland", "Auckland", "Waikato", "Bay of Plenty", "Gisborne / Hawke's Bay",
                                  "Taranaki", "Manawatu-Wanganui", "Wellington", 
                                  "Nelson/Tasman/Marlborough/West Coast", "Canterbury", "Otago", "Southland"))


#---------------save to database---------------
creds <- AskCreds("Credentials for someone who can create databases")

PlayPen <- odbcConnect("PlayPen_prod", uid = creds$uid, pwd = creds$pwd)
try(sqlQuery(PlayPen, "create database nzis11") )
sqlQuery(PlayPen, "use nzis11")

sqlQuery(PlayPen, "DROP TABLE IF EXISTS f_mainheader")
sqlQuery(PlayPen, "DROP TABLE IF EXISTS f_ethnicity")
sqlQuery(PlayPen, "DROP TABLE IF EXISTS d_sex")
sqlQuery(PlayPen, "DROP TABLE IF EXISTS d_agegrp")
sqlQuery(PlayPen, "DROP TABLE IF EXISTS d_ethnicity")
sqlQuery(PlayPen, "DROP TABLE IF EXISTS d_occupation")
sqlQuery(PlayPen, "DROP TABLE IF EXISTS d_qualification")
sqlQuery(PlayPen, "DROP TABLE IF EXISTS d_region")

# fact tables.  These take a long time to load up with sqlSave (which adds one row at a time)
# but it's easier (quick and dirty) than creating a table and doing a bulk upload from a temp 
# file.  Any bigger than this you'd want to bulk upload though - took 20 minutes or more.
sqlSave(PlayPen, f_mainheader, addPK = FALSE, rownames = FALSE)
sqlSave(PlayPen, f_ethnicity, addPK = TRUE, rownames = FALSE) 
                                            # add a primary key on the fly in this case.  All other tables
                                            # have their own already created by R.

# dimension tables
sqlSave(PlayPen, d_sex, addPK = FALSE, rownames = FALSE)
sqlSave(PlayPen, d_agegrp, addPK = FALSE, rownames = FALSE)
sqlSave(PlayPen, d_ethnicity, addPK = FALSE, rownames = FALSE)
sqlSave(PlayPen, d_occupation, addPK = FALSE, rownames = FALSE)
sqlSave(PlayPen, d_qualification, addPK = FALSE, rownames = FALSE)
sqlSave(PlayPen, d_region, addPK = FALSE, rownames = FALSE)

#----------------indexing----------------------

sqlQuery(PlayPen, "ALTER TABLE f_mainheader ADD PRIMARY KEY(survey_id)")

sqlQuery(PlayPen, "ALTER TABLE d_sex ADD PRIMARY KEY(sex_id)")
sqlQuery(PlayPen, "ALTER TABLE d_agegrp ADD PRIMARY KEY(agegrp_id)")
sqlQuery(PlayPen, "ALTER TABLE d_ethnicity ADD PRIMARY KEY(ethnicity_id)")
sqlQuery(PlayPen, "ALTER TABLE d_occupation ADD PRIMARY KEY(occupation_id)")
sqlQuery(PlayPen, "ALTER TABLE d_qualification ADD PRIMARY KEY(qualification_id)")
sqlQuery(PlayPen, "ALTER TABLE d_region ADD PRIMARY KEY(region_id)")



#---------------create an analysis-ready view-------------------
# In Oracle we'd use a materialized view, which MySQL can't do.  

sql1 <-
   "CREATE VIEW vw_mainheader AS SELECT sex, agegrp, occupation, qualification, region, income FROM
      f_mainheader a   JOIN
      d_sex b          on a.sex_id = b.sex_id JOIN
      d_agegrp c       on a.agegrp_id = c.agegrp_id JOIN
      d_occupation e   on a.occupation_id = e.occupation_id JOIN
      d_qualification f on a.qualification_id = f.qualification_id JOIN
      d_region g       on a.region_id = g.region_id"

sqlQuery(PlayPen, sql1)


#---------------------test against summary statistics in data dictionary-------------

tab1 <- sqlQuery(PlayPen, "SELECT 
                              sex,
                              ROUND(AVG(income))   as Mean, 
                              COUNT(1)       as Sample
                           FROM vw_mainheader
                           GROUP BY sex")

tab1


tab2 <- sqlQuery(PlayPen, "SELECT 
                              agegrp,
                              ROUND(AVG(income))   as Mean, 
                              COUNT(1)       as Sample
                           FROM vw_mainheader
                           GROUP BY agegrp")

tab2


# qualification summary in data dictionary uses a different classification to that in data
tab3 <- sqlQuery(PlayPen, "SELECT 
                              qualification,
                              ROUND(AVG(income))   as Mean, 
                              COUNT(1)       as Sample
                           FROM vw_mainheader
                           GROUP BY qualification
                           ORDER BY Mean")

tab3

# occupation summary not given in data dictionary
tab4 <- sqlQuery(PlayPen, "SELECT 
                             occupation,
                             ROUND(AVG(income))   as Mean, 
                             COUNT(1)       as Sample
                           FROM vw_mainheader
                           GROUP BY occupation
                           ORDER BY Mean")

tab4

# region summary not given in data dictionary
tab5 <- sqlQuery(PlayPen, "SELECT 
                             region,
                             ROUND(AVG(income))   as Mean, 
                             COUNT(1)       as Sample
                           FROM vw_mainheader
                           GROUP BY region
                           ORDER BY Mean ")

tab5


tab6 <- sqlQuery(PlayPen,
                 "SELECT
                     ethnicity,
                     ROUND(AVG(income)) as Mean,
                     COUNT(1) as Sample
                  FROM f_mainheader m
                  JOIN f_ethnicity e ON m.survey_id = e.survey_id
                  JOIN d_ethnicity d ON e.ethnicity_id = d.ethnicity_id
                  GROUP BY ethnicity
                  ORDER BY Mean")
                 
tab6                 
