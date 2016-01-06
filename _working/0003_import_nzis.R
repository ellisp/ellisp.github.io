# Script loads NZIS data from the Stats NZ website into a MySQL database
# Peter Ellis, August 2015, updated January 2016 to work with RMySQL (*much* faster than RODBC)
library(dplyr)
library(tidyr)
library(RMySQL)

# set up credentials
# need to replace the XXX s with credentials of someone who can create databases
creds <- list(uid = "XXXXX", pwd = "XXXXX")


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
f_ethnicity$PK <- 1:nrow(f_ethnicity)
   
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
                          "Clerical and Administrative Workers",
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
PlayPen <- dbConnect(RMySQL::MySQL(), username = creds$uid, password = creds$pwd)



try(dbSendQuery(PlayPen, "create database nzis11") )

PlayPen <- dbConnect(RMySQL::MySQL(), dbname = "nzis11", username = creds$uid, password = creds$pwd)


dbSendQuery(PlayPen, "DROP TABLE IF EXISTS f_mainheader")
dbSendQuery(PlayPen, "DROP TABLE IF EXISTS f_ethnicity")
dbSendQuery(PlayPen, "DROP TABLE IF EXISTS d_sex")
dbSendQuery(PlayPen, "DROP TABLE IF EXISTS d_agegrp")
dbSendQuery(PlayPen, "DROP TABLE IF EXISTS d_ethnicity")
dbSendQuery(PlayPen, "DROP TABLE IF EXISTS d_occupation")
dbSendQuery(PlayPen, "DROP TABLE IF EXISTS d_qualification")
dbSendQuery(PlayPen, "DROP TABLE IF EXISTS d_region")
dbSendQuery(PlayPen, "DROP VIEW IF EXISTS vw_mainheader")

# fact tables.  These load up quickly in DBI, compared to RODBC.
dbWriteTable(PlayPen, "f_mainheader", f_mainheader, row.names = FALSE, overwrite = TRUE)
dbWriteTable(PlayPen, "f_ethnicity", f_ethnicity, row.names = FALSE, overwrite = TRUE) 

# dimension tables
dbWriteTable(PlayPen, "d_sex", as.data.frame(d_sex), row.names = FALSE, overwrite = TRUE)
dbWriteTable(PlayPen, "d_agegrp", as.data.frame(d_agegrp), row.names = FALSE, overwrite = TRUE)
dbWriteTable(PlayPen, "d_ethnicity", as.data.frame(d_ethnicity), row.names = FALSE, overwrite = TRUE)
dbWriteTable(PlayPen, "d_occupation", as.data.frame(d_occupation), row.names = FALSE, overwrite = TRUE)
dbWriteTable(PlayPen, "d_qualification", as.data.frame(d_qualification), row.names = FALSE, overwrite = TRUE)
dbWriteTable(PlayPen, "d_region", as.data.frame(d_region), row.names = FALSE, overwrite = TRUE)

#----------------indexing----------------------

dbSendQuery(PlayPen, "ALTER TABLE f_mainheader ADD PRIMARY KEY(survey_id)")

dbSendQuery(PlayPen, "ALTER TABLE d_sex ADD PRIMARY KEY(sex_id)")
dbSendQuery(PlayPen, "ALTER TABLE d_agegrp ADD PRIMARY KEY(agegrp_id)")
dbSendQuery(PlayPen, "ALTER TABLE d_ethnicity ADD PRIMARY KEY(ethnicity_id)")
dbSendQuery(PlayPen, "ALTER TABLE d_occupation ADD PRIMARY KEY(occupation_id)")
dbSendQuery(PlayPen, "ALTER TABLE d_qualification ADD PRIMARY KEY(qualification_id)")
dbSendQuery(PlayPen, "ALTER TABLE d_region ADD PRIMARY KEY(region_id)")



#---------------create an analysis-ready view-------------------
# In Oracle we'd use a materialized view, which MySQL can't do.  But
# the below is fast enough anyway:

sql1 <-
   "CREATE VIEW vw_mainheader AS SELECT sex, agegrp, occupation, qualification, region, hours, income FROM
      f_mainheader a   JOIN
      d_sex b          on a.sex_id = b.sex_id JOIN
      d_agegrp c       on a.agegrp_id = c.agegrp_id JOIN
      d_occupation e   on a.occupation_id = e.occupation_id JOIN
      d_qualification f on a.qualification_id = f.qualification_id JOIN
      d_region g       on a.region_id = g.region_id"

dbSendQuery(PlayPen, sql1)


