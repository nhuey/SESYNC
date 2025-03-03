library(rnaturalearth)
library(ggplot2)
library(sf)
library(rgeos)
library(geosphere)
library(purrr)
library(sp)
library(ggthemes)
library(RColorBrewer)
library(stringr)
library(choroplethr)
library(choroplethrMaps)
library(wordcloud)
library(dplyr)
library(tidyr)
library(RMySQL)
library(maps)
library(sf)
library(ggmap)
library(mapdata)
library(RMariaDB)



####################################### Read in data from SEADMIN database and do initial cleaning ############################################

##### Read participants and demographics views from seadmin database #####

con = dbConnect(RMariaDB::MariaDB(), user = "reporting",
                dbname="seadmin",
                host= "127.0.0.1",
                            password="wMoUKSjrooNKxH4HypCC")

# FIXME latin1 encoded MySQL varchar are not all coming through correctly, or are entered wrong
# FIXME the following line is a workaround that reduces but does not eliminate weird chars
character_set <- dbGetQuery(con, 'set character set utf8')
event <- dbReadTable(con, 'v_event')
projects <- dbReadTable(con, 'v_projects')
participants <- dbReadTable(con, 'v_participants')
demographics <- dbReadTable(con, 'v_demographics')
dbDisconnect(con)

dim(demographics)

participants[1:5, 1:9]
projects[1:5, 1:14]
demographics[1:5, 10:20]

names(projects)
dim(projects)

names(event)
which(participants$lastName == "Todd")
participants[9810,]
