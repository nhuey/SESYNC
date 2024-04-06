library(readxl)
library(fedmatch)
library(openxlsx)
library(xlsx)
library(dbplyr)
library(data.table)
library(stringdist)
library(writexl)
library(tidyverse)

# Read in the Data
sesync <- read_excel("~/Dropbox/SESYNC/SESYNC Data.xlsx")
table(sesync$STRIKE_REAL)

# We start by removing all rows that have strike lines through them. I facilitated this by adding a column to the excel file with a logical value for whether the last name cell was striken or not
# I did this in the Google Sheet via Excel-like coding and named the variable "STRIKE_REAL"
# Remove rows with "strike-through" formatting as flagged by variable "STRIKE_REAL"

sesync <- sesync[which(sesync$STRIKE_REAL == FALSE),]

# For Testing 

#sesync <- subset(sesync, sesync$`Last Name` == "Johnson")

# Clean up Career variable (all lowercase, remove special characters)
sesync$`Career Stage` <- clean_strings(sesync$`Career Stage`)

# Clean SESYNC mispells
sesync$Institute[which(sesync$Institute == "SEYNC")] <- "SESYNC"

# Specify the possible career values for fuzzy matching
possible.careers <- c("academic", "early career", "mid career", "nonprofit", "postdoc", "senior career", "student")

# Note there are many missing values for 'Career Stage' in this data
sum(is.na(sesync$`Career Stage`)) # 2519 NA

# Temporarily separate those rows with missing 'Career Stage' values so the fuzzy matching can work
sesync.na.removed <- sesync[which(!is.na(sesync$`Career Stage`)),]
sesync.na <- sesync[which(is.na(sesync$`Career Stage`)),]

# Fuzzy matching for 'Career Stage' - only those values in the variable 'possible.careers' can be chosen
result_1 <- merge_plus(data.table(id = 1:nrow(sesync.na.removed), sesync.na.removed$`Career Stage`), data.table(id2 = 1:length(possible.careers), possible.careers), 
           match_type = "fuzzy",
           unique_key_1  = "id",
           unique_key_2  = "id2",
           by.x = "V2",
           by.y = "possible.careers",
           fuzzy_settings = build_fuzzy_settings(maxDist = .5))

matches <- (result_1$matches)

# A reordering is necessary due to the internal sorting of the merge_plus function - look at the 'matches' object before running the following line if you want to see why and look at the id and id2 columns
matches$possible.careers <- matches$possible.careers[order(matches$id)]

# Set the fuzzy matches as the 'Career Stage' variable
sesync.na.removed$`Career Stage` <- matches$possible.careers

## Need to check what "s" is, but otherwise everything matched up appropriately

# Put the data frame back together with missing 'Career Stage' observations at the end
sesync <- rbind(sesync.na.removed, sesync.na)


# In this table, we see that all the possible variable values have been standardized
table(sesync$`Career Stage`) # Career Stage cleaned - checked

# All non-missing Academic areas to "Academic" - this was specified in my Instructions file
sesync$`Academic or Non-Academic?`[which(!is.na(sesync$`Academic Areas – Assigned JAN 2024`))] <- "Academic"
table(sesync$`Academic or Non-Academic?`[which(!is.na(sesync$`Academic Areas – Assigned JAN 2024`))]) # Checked

################# Fixing Postdoc entries - see Instructions file for details
# First change all postdoc entries from 'Institutional Status' and 'Career Stage' to have 'Career Stage' of "EARLY_CAREER"
postdoc_match.1 <- grep("postdoc", clean_strings(sesync$`Institutional Status`))
postdoc_match.2 <- grep("postdoc", clean_strings(sesync$`Career Stage`))
postdoc_match.3 <- grep("postdoc", clean_strings(sesync$Title))

postdoc_match <- union(postdoc_match.1, postdoc_match.2)
postdoc_match <- union(postdoc_match, postdoc_match.3)

for(i in 1:length(postdoc_match)){
  if(is.na(sesync$Institute[postdoc_match[i]])){sesync$`Career Stage`[postdoc_match[i]] <- "EARLY_CAREER"}else{
  if(sesync$Institute[postdoc_match[i]] == "SESYNC"){sesync$`Career Stage`[postdoc_match[i]] <- "POSTDOC"}else{
    sesync$`Career Stage`[postdoc_match[i]] <- "EARLY_CAREER"
  }
  }
}

# Second, match all entries in Institutional Status with "grad" anywhere in the name and change Career Stage 
# variable to "student"

grad_match <- grep("grad", clean_strings(sesync$`Institutional Status`))
sesync$`Career Stage`[grad_match] <- "student"


################# Fixing Postdoc entries - End

################# Removing Duplicate Entries for Individuals with the same values for the variables 'Last Name' and 'First Name/Middle\n'
### This will leave multiple rows for the same individual if they have different values for these variables
### The row with the least missing values is retained for each individual

sesync$missing <- apply(sesync, 1, function(x) sum(is.na(x)))
sesync_filtered <- sesync %>% group_by(`Last Name`, `First Name/Middle\n` ) %>%
  slice(which.min(missing))


################# Removing Duplicate Entries for Individuals - End
################# Cleaning and Fuzzy Matching 'Country' Variable

sesync_filtered$Country <- clean_strings(sesync_filtered$Country)

#Add in all incorrect country labels 
possible.countries <- sort(unique(clean_strings(sesync_filtered$Country)[-length(sesync_filtered$Country)]))[-c(107,106,103,102,105, 101,4,15,18,50,56,70,80,90,93,95)]

# Remove missing values for fuzzy matching
sesync.na.removed <- sesync_filtered[which(!is.na(sesync_filtered$Country)),]
sesync.na <- sesync_filtered[which(is.na(sesync_filtered$Country)),]


# Fuzzy Matching
result_1 <- merge_plus(data.table(id = 1:nrow(sesync.na.removed), sesync.na.removed$Country), data.table(id2 = 1:length(possible.countries), possible.countries), 
                       match_type = "fuzzy",
                       unique_key_1  = "id",
                       unique_key_2  = "id2",
                       by.x = "V2",
                       by.y = "possible.countries",
                       fuzzy_settings = build_fuzzy_settings(maxDist = .5))

matches <- (result_1$matches)

# Reordering matches - see first instance of fuzzy matching in this code for comments
matches$possible.countries <- matches$possible.countries[order(matches$id)]
sesync.na.removed$Country <- matches$possible.countries


# Putting dataset back together
sesync_filtered <- rbind(sesync.na.removed, sesync.na)

# Specific fixes
sesync_filtered$Country[which(sesync_filtered$Country %in% c("academic", "africa"))] <- NA
sesync_filtered$Country[which(sesync_filtered$Country == "uk")] <- "united kingdom"
sesync_filtered$Country[which(sesync_filtered$Country %in% c("united states", "virginia"))] <- "usa"


table(sesync_filtered$Country) # Country cleaned - checked

################# Cleaning and Fuzzy Matching 'Country' Variable - End
################# Cleaning the 'Race" variable - See Instructions file

# This cleaning is only to be completed for US-based individuals, so we split these off

states.in.sesync <- names(table(sesync_filtered$`US State`))[-c(1,2,5,6,7,10,12,14,15,16,17,19,20,23,24,25,28,31,32,36,44,49,50,56,62,63,70,71,77,80,81,82,85,87,88,92,93:95, 97,105,110:112)]

for(i in 1:nrow(sesync_filtered)){
  print(i)
  print(sesync_filtered$`US State`[i])
  if(is.na(sesync_filtered$`US State`[i])){next}
  if(sesync_filtered$`US State`[i] %in% states.in.sesync){sesync_filtered$Country[i] <- "usa"}
}

usa.index <- which(sesync_filtered$Country== "united states" |sesync_filtered$Country == "usa" |sesync_filtered$Country == "estados unidos")
sesync_filtered_usa <- sesync_filtered[usa.index,]
sesync_filtered_nonusa <- sesync_filtered[-usa.index,]


# Basic cleaning of 'Ethnicity' and 'Race' variables
sesync_filtered_usa$Ethnicity <- clean_strings(sesync_filtered_usa$Ethnicity)
sesync_filtered_usa$Race <- clean_strings(sesync_filtered_usa$Race)



# Helper function to change "male" and "no answer" to NA for 'Race'
race.na <- function(x){
  if(is.na(x)){return(NA)}else{
  if(x == "male" | x == "no answer"){return(NA)}else{return(x)}
  }
}

# Helper function to change "none" and "no answer" to NA for 'Ethnicity'

ethnicity.na <- function(x){
  if(is.na(x)){return(NA)}else{
    if(x == "none" | x == "no answer"){return(NA)}else{return(x)}
  }
}

# Using the helper functions to re-code some responses
sesync_filtered_usa$Race <- sapply(sesync_filtered_usa$Race, race.na)
sesync_filtered_usa$Ethnicity <- sapply(sesync_filtered_usa$Ethnicity, ethnicity.na)

# Fuzzy matches for Race
# Add in all incorrect 'Race' labels so they are not options for fuzzy matching ("africanamerican", "ehite", "white no answer)
possible.races <- sort(unique((sesync_filtered_usa$Race)))[-c(4,9,19)]

sum(is.na(sesync_filtered_usa$Race)) # 697 NA

# Remove any NA values before fuzzy matching as before
sesync.na.removed <- sesync_filtered_usa[which(!is.na(sesync_filtered_usa$Race)),]
sesync.na <- sesync_filtered_usa[which(is.na(sesync_filtered_usa$Race)),]

# Fuzzy matching
result_1 <- merge_plus(data.table(id = 1:nrow(sesync.na.removed), sesync.na.removed$Race), data.table(id2 = 1:length(possible.races), possible.races), 
                       match_type = "fuzzy",
                       unique_key_1  = "id",
                       unique_key_2  = "id2",
                       by.x = "V2",
                       by.y = "possible.races",
                       fuzzy_settings = build_fuzzy_settings(maxDist = .5))

matches <- (result_1$matches)

# Reordering
matches$possible.races <- matches$possible.races[order(matches$id)]
# Setting 'Race' equal to fuzzy matches
sesync.na.removed$Race <- matches$possible.races
# Rejoining USA data
sesync_filtered_usa <- rbind(sesync.na.removed, sesync.na)
# Re-coding the value "african american" to "black"
sesync_filtered_usa$Race[which(sesync_filtered_usa$Race == "african american")] <- "black"
table(sesync_filtered_usa$Race) # Race variable cleaned - checked


#######  We want anyone with multiple races listed as a "Multirace" category
possible_categories <- c("black", "asian", "white", "hisp/lat", "nat amer", "other", "multi")

# Recode pacific islander to "other"
sesync_filtered_usa$Race<- gsub("pacific islander", "other", sesync_filtered_usa$Race)
# Anyone with 3 or more words is "multi"
sesync_filtered_usa$Race <- gsub("\\b\\w+\\s+\\w+\\s+\\w+\\b", "multi", sesync_filtered_usa$Race)
sesync_filtered_usa$Race <- gsub(".*\\bmulti\\b.*", "multi", sesync_filtered_usa$Race)
# "asian white" and "white other" are "multi"
sesync_filtered_usa$Race <- gsub("asian white", "multi", sesync_filtered_usa$Race)
sesync_filtered_usa$Race <- gsub("white other", "multi", sesync_filtered_usa$Race)

table(sesync_filtered_usa$Race )
sesync_filtered_usa$Race <- gsub("native american", "natam", sesync_filtered_usa$Race)

table(sesync_filtered_usa$Race, sesync_filtered_usa$Ethnicity) # Going to have more individuals changed to "multi"

# If anyone lists "hisp/lat" in Ethnicity, add to new variable
sesync_filtered_usa$hisplat <- (sesync_filtered_usa$Ethnicity == "hispanic" | sesync_filtered_usa$Ethnicity == "latino")
table(sesync_filtered_usa$hisplat)

for(i in 1:nrow(sesync_filtered_usa)){
if(is.na(sesync_filtered_usa$hisplat[i])){next}
if(sesync_filtered_usa$hisplat[i]){
  sesync_filtered_usa$Race[i] <- paste(sesync_filtered_usa$Race[i], sesync_filtered_usa$Ethnicity[i])
  }
}

table(sesync_filtered_usa$Race)
# Remove NA from "Race" names
sesync_filtered_usa$Race <- gsub("NA ", "", sesync_filtered_usa$Race)
table(sesync_filtered_usa$Race)

# Now 2 words means "multi", e.g. "asian hispanic", "black hispanic", "white latino", etc.
sesync_filtered_usa$Race <- gsub("\\b\\w+\\s+\\w+\\b", "multi", sesync_filtered_usa$Race)
sesync_filtered_usa$Race <- gsub(".*\\bmulti\\b.*", "multi", sesync_filtered_usa$Race)

table(sesync_filtered_usa$Race)

# Cleaning up some "Race" category names
sesync_filtered_usa$Race <- gsub("hispanic", "hisp/lat", sesync_filtered_usa$Race)
sesync_filtered_usa$Race <- gsub("latino", "hisp/lat", sesync_filtered_usa$Race)
sesync_filtered_usa$Race <- gsub("natam", "nat amer", sesync_filtered_usa$Race)

table(sesync_filtered_usa$Race) # "Race" variable cleaned for USA individuals

# Putting the dataset back together
sesync_filtered <- rbind(sesync_filtered_usa, sesync_filtered_nonusa)

# Remove junk row

sesync_filtered <- sesync_filtered[-which(sesync_filtered$missing == 65),]
################# Cleaning the 'Race" variable - End

####### Now get Project Codes and Years

project_codes <- read_excel("~/Dropbox/SESYNC/SESYNC Participants Overall.xlsx")

names(project_codes)[1:3] <- c("code", "first", "last")

names(sesync_filtered)[3] <- "first"

sesync_filtered$first <- clean_strings(sesync_filtered$first) 
sesync_filtered$`Last Name` <-  clean_strings(sesync_filtered$`Last Name`)
project_codes$first <- clean_strings(project_codes$first)
project_codes$last <- clean_strings(project_codes$last)

project_codes[which(project_codes$last == "olsen"),]

is_fuzzy_match <- function(string1, possible.strings, threshold = 0.2) {
  possible.strings[is.na(possible.strings)] <- "xxxxxxxxxx"
  dis <- (sapply(possible.strings, stringdist, string1,method = "jw"))
  index.min <- which(dis == min(dis))
  if (min(dis) <= threshold) {
    return(index.min)
  } else {
    return(FALSE)
  }
}

###### Testing
which(project_codes$code == "2019W-085")
######

is_fuzzy_match(sesync_filtered$`Last Name`[1], project_codes$last, threshold = 0.1)

extras <- tibble()
for(i in 1:nrow(sesync_filtered)){
  print(i)
  #if(!(sesync_filtered$`Last Name`[i] %in% project_codes$last)){next}
  #last <- which(project_codes$last == sesync_filtered$`Last Name`[i])
  if(is.na(sesync_filtered$`Last Name`[i])){next}
  if(!(is_fuzzy_match(sesync_filtered$`Last Name`[i], project_codes$last))[1]){next}
  #if(!(sesync_filtered$first[i] %in% project_codes$first)){next}
  last <- is_fuzzy_match(sesync_filtered$`Last Name`[i], project_codes$last)
  if(is.na(sesync_filtered$first[i])){next}
  if(!(is_fuzzy_match(sesync_filtered$first[i], project_codes$first))[1]){next}
  #if(!(sesync_filtered$first[i] %in% project_codes$first)){next}
  first <- is_fuzzy_match(sesync_filtered$first[i], project_codes$first)
  #first <- which(project_codes$first == sesync_filtered$first[i])

  index <- intersect(first, last)
  
  if(is_empty(index)){next}
  
  if(length(index) > 1){
    extra.to.add <- sesync_filtered[rep(i, length(index) - 1),]
    extra.to.add$`Project Code` <- project_codes$code[index[-1]]
    extras <- rbind(extras, extra.to.add)
    sesync_filtered$`Project Code`[i] <- project_codes$code[index[1]]
  }else{
    if(length(index) == 1){sesync_filtered$`Project Code`[i] <- project_codes$code[index]}
    }
}

sesync_filtered <- rbind(sesync_filtered, extras)

sum(is.na(sesync_filtered$`Project Code`))
nrow(sesync_filtered) # Only 1817 records found 4639-2822


# There are a couple other things to do to try to get more of the project codes, i.e. 
# Pulling from the database for names attached to project codes
## projects, demographics, participants, etc.


## projects file from database, "trackingCode" variable has number and "PI" and "CoPI" have names
# first I'll try to match on the PI's name - ask about CoPI names

extract_last_word <- function(string) {
  words <- strsplit(string, "\\s+")[[1]]
  last_word <- tail(words, 1)
  return(last_word)
}
extract_all_but_last_word <- function(string) {
  words <- strsplit(string, "\\s+")[[1]]
  all_but_last_word <- head(words, -1)
  return(paste(all_but_last_word, collapse = " "))
}


projects$last <- clean_strings(sapply(projects$PI, extract_last_word))
projects$firstmid <- clean_strings(sapply(projects$PI, extract_all_but_last_word))

### For testing ###
projects[which(projects$last == "olsen"),]
projects[which(projects$trackingCode == "2019W-085"),]
participants[which(participants$trackingCode == "2019W-085"),]
### For testing - End ###


extras <- tibble()
for(i in 1:nrow(sesync_filtered)){
  print(i)
  #last <- which(projects$last == sesync_filtered$`Last Name`[i])
  if(!(is_fuzzy_match(sesync_filtered$`Last Name`[i], projects$last))[1]){next}
  #if(!(sesync_filtered$first[i] %in% projects$firstmid)){next}
  last <- is_fuzzy_match(sesync_filtered$`Last Name`[i], projects$last)
  if(is.na(sesync_filtered$first[i])){next}
  if(!(is_fuzzy_match(sesync_filtered$first[i], projects$firstmid))[1]){next}
  #if(!(sesync_filtered$first[i] %in% projects$firstmid)){next}
  first <- is_fuzzy_match(sesync_filtered$first[i], projects$firstmid)
  #first <- which(projects$firstmid == sesync_filtered$first[i])
  
  
  index <- intersect(first, last)
  
  if(is_empty(index)){next}
  if(!is.na(sesync_filtered$`Project Code`[i])){
    # Instead of moving on, add a new line
    extra.to.add <- sesync_filtered[i,]
    extra.to.add$`Project Code` <- projects$trackingCode[index[1]]
  }
  if(length(index) > 1){
    extras.to.add <- sesync_filtered[rep(i, length(index) - 1),]
    extras.to.add$`Project Code` <- projects$trackingCode[index[-1]]
    if(!is.na(sesync_filtered$`Project Code`[i])){extras.to.add <- rbind(extra.to.add, extras.to.add)}
    extras <- rbind(extras, extras.to.add)
    if(is.na(sesync_filtered$`Project Code`[i])){sesync_filtered$`Project Code`[i] <- projects$trackingCode[index[1]]}
  }else{
    if(length(index) == 1){
      if(is.na(sesync_filtered$`Project Code`[i])){sesync_filtered$`Project Code`[i] <- projects$trackingCode[index]}
      if(!is.na(sesync_filtered$`Project Code`[i])){extras <- rbind(extras, extra.to.add)}
    }
  }
}

#projects[143,]
#projects$trackingCode[143]
sesync_filtered <- rbind(sesync_filtered, extras)

sesync_filtered[sesync_filtered$first == "leah",]


sum(is.na(sesync_filtered$`Project Code`))
nrow(sesync_filtered) # 4732 - 2719.
# fuzzy matching on last names??


demographics$last <- clean_strings(demographics$lastName)
demographics$firstNameMiddle <- clean_strings(demographics$firstNameMiddle)

### For Testing ###
demographics[which(demographics$firstNameMiddle == "leah")[2],]
### For Testing - End ###

which(sesync_filtered$`Last Name` == "johnson" & sesync_filtered$first == "leah")

# So this should have worked -> somewhere maybe it was overridden? April 4 NH


extras <- tibble()
for(i in 1:nrow(sesync_filtered)){
  print(i)
  if(!(is_fuzzy_match(sesync_filtered$`Last Name`[i], demographics$last))[1]){next}
  last <- is_fuzzy_match(sesync_filtered$`Last Name`[i], demographics$last)
  if(is.na(sesync_filtered$first[i])){next}
  if(!(is_fuzzy_match(sesync_filtered$first[i], demographics$firstNameMiddle))[1]){next}
  first <- is_fuzzy_match(sesync_filtered$first[i], demographics$firstNameMiddle)
  index <- intersect(first, last)
  if(is_empty(index)){next}
  if(!is.na(sesync_filtered$`Project Code`[i])){
    # Instead of moving on, add a new line
    extra.to.add <- sesync_filtered[i,]
    extra.to.add$`Project Code` <- demographics$trackingCodes[index[1]]
  }
  if(length(index) > 1){
    extras.to.add <- sesync_filtered[rep(i, length(index) - 1),]
    extras.to.add$`Project Code` <- demographics$trackingCodes[index[-1]]
    if(!is.na(sesync_filtered$`Project Code`[i])){extras.to.add <- rbind(extra.to.add, extras.to.add)}
    extras <- rbind(extras, extras.to.add)
    if(is.na(sesync_filtered$`Project Code`[i])){sesync_filtered$`Project Code`[i] <- demographics$trackingCodes[index[1]]}
  }else{
    if(length(index) == 1){
      if(is.na(sesync_filtered$`Project Code`[i])){sesync_filtered$`Project Code`[i] <- demographics$trackingCodes[index]}
      if(!is.na(sesync_filtered$`Project Code`[i])){extras <- rbind(extras, extra.to.add)}
    }
  }
}
sesync_filtered <- rbind(sesync_filtered, extras)

sum(is.na(sesync_filtered$`Project Code`)) # 14 missing values still
nrow(sesync_filtered) 

#write_xlsx(sesync_filtered, "~/Dropbox/SESYNC/verylong.xlsx")
#sesync_filtered <- unique(sesync_filtered)
#sesync_filtered[sesync_filtered$first == "leah",]

sesync_filterwhich(sesync_filtered$`Last Name` == "olson")

demographics[which(demographics$last == "olson"),]

i = which(is.na(sesync_filtered$`Project Code`))

last <- is_fuzzy_match(sesync_filtered$`Last Name`[i[14]], demographics$last)
first <- is_fuzzy_match(sesync_filtered$first[i[14]], demographics$firstNameMiddle)

intersect(last, first)
sesync_filtered[i[14],c(3:6,13:14)]
demographics[3744,]
sesync_filtered[i[7],]

which(last == "olsen")

######### Individual Fixes
sesync_filtered[i[1],"first"] <- demographics[1297,]$firstNameMiddle
sesync_filtered[i[1],1] <- demographics[1297,]$trackingCodes

sesync_filtered[i[2],'Last Name'] <- demographics[2233,]$last
sesync_filtered[i[2],1] <- demographics[2233,]$trackingCodes


sesync_filtered[i[3],'Last Name'] <- demographics[3098,]$lastName
sesync_filtered[i[3],1] <- demographics[3098,]$trackingCodes


sesync_filtered[i[4],'first'] <- demographics[5243,]$firstNameMiddle
sesync_filtered[i[4],1] <- demographics[5243,]$trackingCodes

sesync_filtered[i[5],'first'] <- demographics[2396,]$firstNameMiddle
sesync_filtered[i[5],1] <- demographics[2396,]$trackingCodes

sesync_filtered[i[6],'first'] <- "michael"
sesync_filtered[i[6],1] <- demographics[3571,]$trackingCodes

sesync_filtered[i[7],1] <- "XXXXXX" # meaningless row, but I'll keep it until the end so the indices aren't thrown off

# Kamala Todd and Loretta Todd don't appear to be in the other parts of the database I've checked
sesync_filtered[i[8],]$City 
sesync_filtered[i[9],]

sesync_filtered[i[10],'Last Name'] <- demographics[4916,]$lastName
sesync_filtered[i[10],1] <- demographics[4916,]$trackingCodes

sesync_filtered[i[11],1] <- paste0(demographics[5189,]$trackingCodes, ",", demographics[5190,]$trackingCodes)

sesync_filtered[i[12],1] <- demographics[176,]$trackingCodes

sesync_filtered[i[13],1] <- demographics[1174,]$trackingCodes

sesync_filtered[i[14],1] <- demographics[3743,]$trackingCodes

# Remove the Todds
sesync_filtered <- sesync_filtered[-i[8:9],]

######### Individual Fixes - End

test.update <- read_csv("~/Dropbox/SESYNC/updatedsesync.csv")

sesync_filtered[which(sesync_filtered$first == "leah" & sesync_filtered$`Last Name` == "johnson"),]

# Split observations into multiple rows if there are multiple Project Codes associated with an individual

project_codes <- sapply(sesync_filtered$`Project Code`, str_split, pattern = ",")
extra <- data.frame()
for(i in 1:length(project_codes)){
  print(i)
  if(length(project_codes[[i]]) > 1){
    extra.lines <- length(project_codes[[i]])
    temp <-  sesync_filtered[rep(i, extra.lines - 1),]
    temp$`Project Code` <- project_codes[[i]][-1]
    extra <- rbind(extra, temp)
    sesync_filtered$`Project Code`[i] <- project_codes[[i]][1]
  }
}


sesync_filtered_long <- rbind(sesync_filtered, extra)
first_four_digits <- as.numeric(substr(gsub("\\D", "", sesync_filtered_long$`Project Code`), 1, 4))
first_four_digits[(first_four_digits < 2000 | first_four_digits > 2024)] <- NA

sesync_filtered_long$`Project Funding Year` <- first_four_digits



#write_xlsx(sesync_filtered_long, "~/Dropbox/SESYNC/updatedsesync_long_april4.xlsx")



# Check that all SESYNC postdocs are listed as postdocs in Career Stage
sesync.postdocs.index <- which((sesync_filtered_long$Institute == "SESYNC") & ((sesync_filtered_long$`Career Stage` == "POSTDOC" | sesync_filtered_long$`Institutional Status` == "POSTDOC")) )
sesync.people <- sesync_filtered_long[sesync_filtered_long$Institute == "SESYNC",]
sesync.postdocs <- sesync.people[(sesync.people$`Career Stage` == "POSTDOC" | sesync.people$`Institutional Status` == "POSTDOC"),]

table(sesync_filtered_long[sesync.postdocs.index,18])

e.c. <- sesync_filtered_long[which(sesync_filtered_long$`Career Stage` == "early career" ),]
e.c.ses <- e.c.[which(e.c.$Institute == "SESYNC"),]

postdoc.index.4 <- grep("postdoc", clean_strings(sesync_filtered_long$`Project Code`))
sesync.index <- which(sesync_filtered_long$Institute == "SESYNC")

sesync.postdoc.proj <- intersect(postdoc.index.4, sesync.index)

sesync_filtered_long[sesync.postdoc.proj,"Career Stage"] <- "postdoc"

sesync_filtered_long$`Career Stage` <- clean_strings(sesync_filtered_long$`Career Stage`)

#write_xlsx(sesync_filtered_long_unique, "~/Dropbox/SESYNC/updatedsesync_long_april1.xlsx")
# we'll want to do a little QA by randomly checking lines with the demographics database
# Also we can clean the other important entries with fuzzy matching as well

# Now I'll want to go through and clean up other parts 

sesync_filtered_long$Country[which(sesync_filtered_long$Country %in% c("virginia", "united states"))] <- "usa"

table(sesync_filtered_long$Country)

  
table(sesync_filtered_long$`US State`)

sesync_filtered_long$`Other States`[which(!(sesync_filtered_long$`US State` %in% states.in.sesync))]

# if other state is empty and US state not empty and not in us state list -> move us state to other state and set us state to na
# if other state is not empty and US state empty, do nothing
# if us state is not in list, but other state is already there, just clear us state

empty.states.index <- which(is.na(sesync_filtered_long$`Other States`) &  (!(sesync_filtered_long$`US State` %in% states.in.sesync)) & (!is.na(sesync_filtered_long$`US State`)))
nonempty.states.index <- which((!is.na(sesync_filtered_long$`Other States`)) &  (!(sesync_filtered_long$`US State` %in% states.in.sesync)) & (!is.na(sesync_filtered_long$`US State`)))
academic.index <- which(sesync_filtered_long$`Other States` == "Academic")

# jasny.index <- which(sesync_filtered_long$`Last Name` == "Jasny")
# jasny <- sesync_filtered_long[jasny.index,]
# jasny.row <- sesync[which(sesync$`First Name/Middle\n` == "Lorien"),]
# jasny.row$`Last Name` <- "jasny"
# 
# sesync_filtered_long[jasny.index,3:31] <- jasny.row[rep(1, length(jasny.index)),3:31]


sesync_filtered_long[empty.states.index, c("Other States")] <- sesync_filtered_long[empty.states.index, c("US State")]
sesync_filtered_long[empty.states.index, c("US State")] <- NA

sesync_filtered_long[nonempty.states.index, c("US State")] <- NA

nonaca.index <- which(sesync_filtered_long$`Academic or Non-Academic?` == "Non-Academic")
#table(clean_strings(nonaca$`Institutional Status`))

fix.inst.status <- function(x){
  if(is.na(x)){return(NA)}
  if(x %in% c("govern e t", "government")){return("government")}
  if(x %in% c("industry nonprofit", "non profit", "nonprofitr", "nonprofit")){return("nonprofit")}
  if(x %in% c("industry")){return("industry")}
  return(NA)
}


sesync_filtered_long[nonaca.index,]$`Institutional Status` <- sapply(clean_strings(sesync_filtered_long[nonaca.index,]$`Institutional Status`), fix.inst.status)

table(clean_strings(sesync_filtered_long[nonaca.index,]$`Institutional Status`))

#table(clean_strings(sesync_filtered_long$Gender))
#table(clean_strings(sesync_filtered_long$`Career Stage`))
#table(clean_strings(sesync_filtered_long$`Academic Areas – Assigned JAN 2024`))

# Clean Gender
sesync_filtered_long$Gender <- clean_strings(sesync_filtered_long$Gender)
sesync_filtered_long$Gender[which(sesync_filtered_long$Gender == "femaile")] <- "female"
sesync_filtered_long$Gender[which(sesync_filtered_long$Gender == "nonbin")] <- NA

################ Cleaning and Fuzzy Matching 'Academic Area' Variable

sesync_filtered_long$`Academic Areas – Assigned JAN 2024`<- clean_strings(sesync_filtered_long$`Academic Areas – Assigned JAN 2024`)

#Add in all incorrect AA labels 
possible.aa <- sort(unique(clean_strings(sesync_filtered$`Academic Areas – Assigned JAN 2024`)))[-c(2,3,5,6,7,8,10,15,16,17,21,26,27,29,30,31,33,36,40,43,44,45,46,52)]
possible.aa <- c(possible.aa, "health")

# Remove missing values for fuzzy matching
sesync.na.removed <- sesync_filtered_long[which(!is.na(sesync_filtered_long$`Academic Areas – Assigned JAN 2024`)),]
sesync.na <- sesync_filtered_long[which(is.na(sesync_filtered_long$`Academic Areas – Assigned JAN 2024`)),]


# Fuzzy Matching
result_1 <- merge_plus(data.table(id = 1:nrow(sesync.na.removed), sesync.na.removed$`Academic Areas – Assigned JAN 2024`), data.table(id2 = 1:length(possible.aa), possible.aa), 
                       match_type = "fuzzy",
                       unique_key_1  = "id",
                       unique_key_2  = "id2",
                       by.x = "V2",
                       by.y = "possible.aa",
                       fuzzy_settings = build_fuzzy_settings(maxDist = .5))

matches <- (result_1$matches)

# Reordering matches - see first instance of fuzzy matching in this code for comments
matches$possible.aa <- matches$possible.aa[order(matches$id)]
sesync.na.removed$`Academic Areas – Assigned JAN 2024` <- matches$possible.aa


# Putting dataset back together
sesync_filtered_long <- rbind(sesync.na.removed, sesync.na)

# Putting "other" categories in
sesync_filtered_long$`Academic Areas – Assigned JAN 2024`[which((!sesync_filtered_long$`Academic Areas – Assigned JAN 2024` %in% possible.aa[-c(1,5,12,13,20,21,25,30)]) & (!is.na(sesync_filtered_long$`Academic Areas – Assigned JAN 2024`)))] <- "other"

# Changing "Public Health" and "Health Sciences" to "Health" & "oceanography" to "ocean sciences oceanography"
sesync_filtered_long$`Academic Areas – Assigned JAN 2024`[which((sesync_filtered_long$`Academic Areas – Assigned JAN 2024`%in% c("public health", "health sciences")))] <- "health"
sesync_filtered_long$`Academic Areas – Assigned JAN 2024`[which((sesync_filtered_long$`Academic Areas – Assigned JAN 2024`%in% c("oceanography")))] <- "ocean sciences oceanography"

# All looks good
table(sesync_filtered_long$`Academic Areas – Assigned JAN 2024`)

# Removing the one "nonprofit" career stage
sesync_filtered_long$`Career Stage`[which(sesync_filtered_long$`Career Stage` == "nonprofit")] <- NA
#table(sesync_filtered_long$`Career Stage`)

#table(sesync_filtered_long$`Other States`)
sesync_filtered_long$Institute[which(sesync_filtered_long$Institute == "Mr.")] <- NA

sesync_filtered_long <- sesync_filtered_long[-which(sesync_filtered_long$first == "academic"),]

aansf <- read_xlsx("~/Dropbox/SESYNC/aansf.xlsx")
aansf$`Academic Areas` <- clean_strings(aansf$`Academic Areas`)
aansf$ABREV <- clean_strings(aansf$ABREV)

na.index.nsf <- which(!is.na(sesync_filtered_long$`Academic Areas – Assigned JAN 2024`))

for(i in 1:length(na.index.nsf)){
  nsf <- aansf$ABREV[which(aansf$`Academic Areas` == sesync_filtered_long$`Academic Areas – Assigned JAN 2024`[na.index.nsf[i]] )]
  sesync_filtered_long$`New NSF Directorate`[na.index.nsf[i]] <- nsf
}

#table(sesync_filtered_long$`New NSF Directorate`)

#which(!clean_strings(sesync$`Last Name`) %in% sesync_filtered_long$`Last Name`)

#sesync_filtered_long$`Last Name`[which(!sesync_filtered_long$`Last Name`%in% clean_strings(sesync$`Last Name`))]
#which(sesync_filtered_long$Institute == "SESYNC" & sesync_filtered_long$`Career Stage` == "postdoc") + 1


### 
#write_xlsx(sesync_filtered_long, "~/Dropbox/SESYNC/SESYNC_April4Update.xlsx")
#sesync_filtered_long <- read_excel("~/Dropbox/SESYNC/SESYNC_DataUpdateMarch2024_Long.xlsx")

#sesync_filtered_long[which(sesync_filtered_long$`Project Code` == "2017W-065"),]
more_codes <- read_xlsx("~/Dropbox/SESYNC/Project Names & Codes Reference Sheet.xlsx")
more_codes$PIs

j <- sapply(more_codes$PIs, str_split, pattern = "\n")

more_codes_long <- data.frame()
for(i in 1:nrow(more_codes)){
  pis <- j[[i]]
  length.pis <- length(pis)
  more_codes_long.temp <- more_codes[rep(i, length.pis),]
  more_codes_long.temp$PIs <- pis
  more_codes_long <- rbind(more_codes_long, more_codes_long.temp)
}

PI_names <- str_split(more_codes_long$PIs, " ")
last <- firstmiddle <- vector(length = length(PI_names))

for(i in 1:length(PI_names)){
  print(i)
  last[i] <- clean_strings(PI_names[[i]][length(PI_names[[i]])])
  firstmid <- PI_names[[i]][-length(PI_names[[i]])]
  firstmiddle[i] <- clean_strings(paste(firstmid, collapse = " "))
}


#length(last)
#length(more_codes_long$`Tracking Code`)

# So I want to go through this new list, pull all the project codes for each person on the list,
# and add a new line for any project codes that are not present in the master file

# What is happening is that both of the names match exactly somewhere in the file, but they are not on the same line.
# The new strategy will be to now take the *union* instead of the intersection and then check for any matches within a certain fuzzy threshold.
# Only if there are no matches after this fuzzy threshold will a new line be added.
# This should avoid the repeats that we are seeing

# So first we check for substrings either way, and pull codes based on that,
# If no exact sub-matches, do fuzzy matching w/ threshold 0.3

is_either_substring <- function(str1, str2) {
  return(grepl(str1, str2) || grepl(str2, str1))
}

names(sesync_filtered_long)[which(names(sesync_filtered_long) == "first")] <- "First Name"
sesync_filtered_long <- sesync_filtered_long[,1:31]
extra.new <- tibble()
for(i in 1:nrow(more_codes_long)){
  print(i)
  if(is.na(last[i])){next}
  if(!(is_fuzzy_match(last[i], sesync_filtered_long$`Last Name`))[1]){next}
  last.temp <- is_fuzzy_match(last[i], sesync_filtered_long$`Last Name`)
  if(!(is_fuzzy_match(firstmiddle[i], sesync_filtered_long$`First Name`))[1]){next}
  first.temp <- is_fuzzy_match(firstmiddle[i], sesync_filtered_long$`First Name`)
  sub.first <- sapply(sesync_filtered_long$`First Name`[last.temp], is_either_substring, firstmiddle[i])
  sub.last <- sapply(sesync_filtered_long$`Last Name`[first.temp], is_either_substring, last[i])

  # Look for any exact substrings
  if(any(sub.first)){
    new.row <- sesync_filtered_long[last.temp[which(sub.first == TRUE)[1]],]
    new.row$`Project Code` <- more_codes_long$`Tracking Code`[i]
    extra.new <- rbind(extra.new, new.row)
    next
  }
  
  if(any(sub.last)){
    new.row <- sesync_filtered_long[first.temp[which(sub.last == TRUE)[1]],]
    new.row$`Project Code` <- more_codes_long$`Tracking Code`[i]
    extra.new <- rbind(extra.new, new.row)
    next
  }
  
  # Look for fuzzy matches if exact substrings not present
  dis.first <- (sapply(sesync_filtered_long$`First Name`[last.temp], stringdist, firstmiddle[i], method = "jw"))
  dis.last <- (sapply(sesync_filtered_long$`Last Name`[first.temp], stringdist, last[i], method = "jw"))
  
  if(any(dis.first < 0.3)){
    new.row <- sesync_filtered_long[last.temp[which(dis.first < 0.3)[1]],]
    new.row$`Project Code` <- more_codes_long$`Tracking Code`[i]
    extra.new <- rbind(extra.new, new.row)
    next
  }
  
  if(any(dis.last < 0.3)){
    new.row <- sesync_filtered_long[last.temp[which(dis.last < 0.3)[1]],]
    new.row$`Project Code` <- more_codes_long$`Tracking Code`[i]
    extra.new <- rbind(extra.new, new.row)
    next
  }
  
  index <- intersect(first.temp, last.temp)
  # Only if we get through the substrings and fuzzy matches will we add a new piece
  if(is_empty(index)){
    print("adding new name")
    new.index <- nrow(sesync_filtered_long)+1
    sesync_filtered_long[new.index,] <- NA
    sesync_filtered_long[new.index,1] <- more_codes_long$`Tracking Code`[i]
    sesync_filtered_long[new.index,3] <- firstmiddle[i]
    sesync_filtered_long[new.index,4] <- last[i]
    next}
  pcs <- sesync_filtered_long[index,1]
  if(more_codes_long$`Tracking Code`[i] %in% pcs$`Project Code`){next}
  new.row <- sesync_filtered_long[index[1],]
  new.row$`Project Code` <- more_codes_long$`Tracking Code`[i]
  extra.new <- rbind(extra.new, new.row)
}
sesync_filtered_long <- rbind(sesync_filtered_long, extra.new)

first_four_digits <- as.numeric(substr(gsub("\\D", "", sesync_filtered_long$`Project Code`), 1, 4))
first_four_digits[(first_four_digits < 2000 | first_four_digits > 2024)] <- NA

sesync_filtered_long$`Project Funding Year` <- first_four_digits

# Check to make sure no one has multiple lines for the same project

duplicates <- duplicated(sesync_filtered_long[, c("Last Name", "First Name", "Project Code")]) | 
  duplicated(sesync_filtered_long[, c("Last Name", "First Name", "Project Code")], fromLast = TRUE)


dups <- sesync_filtered_long[which(duplicates),]


dups[which(dups[, 1] == dups$`Project Code`[105]),]

sesync_filtered_long_unique <- unique(sesync_filtered_long)
sesync_filtered_long_unique[which(sesync_filtered_long_unique$`Last Name` == "ahalt"),]



#write_xlsx(sesync_filtered_long_unique, "~/Dropbox/SESYNC/updatedsesync_long_april4.xlsx")


april.update <- sesync_filtered_long_unique
#april.update <- read_xlsx("~/Dropbox/SESYNC/updatedsesync_long_april4.xlsx")
duplicates <- duplicated(april.update[, c("Last Name", "First Name", "Project Code")]) | 
  duplicated(april.update[, c("Last Name", "First Name", "Project Code")], fromLast = TRUE)


dups <- april.update[which(duplicates),]


dups[which(dups[, 1] == dups$`Project Code`[1]),]

#april.update.unique <- unique(april.update, by = c("Last Name", "First Name", "Project Code"), fromLast = TRUE)
#write_xlsx(april.update.unique, "~/Dropbox/SESYNC/updatedsesync_long_april1.xlsx")

#april.update <- read_xlsx("~/Dropbox/SESYNC/updatedsesync_long_april4.xlsx")

# Next, I want to group by project code and then calculate distances between last names, keeping the subset of individuals that have a distance less than 0.3

## Ok new strategy is less elegant, but I'll try it. I'll go through each project code and group all the observations together
# Then I'll calculate the minimum distance for each Last Name with the other names in the group
# Then I'll only keep those with a low minimum distance, putting them back together


project.codes <- unique(april.update$`Project Code`)
extra.dis <- data.frame()
for(i in 1:length(project.codes)){
  print(i)
  sub.by.pc <- april.update[which(april.update$`Project Code` == project.codes[i]),]
  sub.by.pc$min.dist <- NA
  if(nrow(sub.by.pc) == 1){next}
  for(j in 1:nrow(sub.by.pc)){
    print(j)
    the.last.name <- sub.by.pc[j, "Last Name"]$`Last Name`
    all.but.the.last.name <- (sub.by.pc[-j, "Last Name"])$`Last Name`
    sub.by.pc[j,"min.dist"] <- min(sapply(1:(nrow(sub.by.pc)-1), function(x) stringdist(the.last.name, all.but.the.last.name[x], method = "jw")))
  }
  extra.dis <- rbind(extra.dis,sub.by.pc[which(sub.by.pc$min.dist < 0.1),])
}

# write_xlsx(extra.dis,"~/Dropbox/SESYNC/oddcases.xlsx")
# The above cases should be investigated to make sure only the correct ID's were given

# After cleaning the file based on the above cases, I have the following file
april_6 <- read_xlsx("~/Dropbox/SESYNC/SESYNC_April4_cleaned.xlsx")

# I'm picking up from here to do whatever Alaina said in her last email


