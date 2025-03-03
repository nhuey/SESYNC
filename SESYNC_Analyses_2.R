# This file is for running the Analyses requested in the file "Analyses Needed 2-29-24"
# The demographic analysis will be run with individuals in the dataset each being represented once
# Right now I'll just take their first row
{
library(tidyverse)
library(fedmatch)
library(usmap)
library(readxl)
library(writexl)
library(ggplot2)
library(data.table)
library(scales)
library(ggrepel) 
library(usmap)
library(MASS)
}
sesync <- read_xlsx("~/Dropbox/SESYNC/SESYNC - Jan19.xlsx")

# Some extra pre-analysis cleaning steps

# Index of Postdoctoral Fellowships
pf.index <- which(sesync$`Project Type` == "Postdoctoral Fellowship")
# Subset containing only Postdoctoral Fellowships
pf <- sesync[pf.index,]

# All rows labelled as Postdoctoral Fellowships need to have SESYNC as Institute
sesync$Institute[pf.index] <- "SESYNC"

# Index of Postdoc Immersions
pi.index <- which(sesync$`Project Type` == "Postdoc Immersion")
# Last Names of known non-SESYNC Postdocs
non.sesync.post <- c("Dreyer", "Fiorella", "Galli", "Hillis", "Howard",
                     "Johnson", "McCord", "Rapacciuolo", "Sobocinski", "Ulibarri",
                     "Benessaiah", "Nenadovic", "Pittman", "Schramski", "Shrum")

# Index for Postdoc Immersion projects of non-SESYNC postdocs
pi.non.sesync.index <- which(sesync$`Project Type` == "Postdoc Immersion" & (sesync$`Last Name` %in% clean_strings(non.sesync.post)))

#Subset of Postdoc Immersion projects of non-SESYNC postdocs
pi.non.sesync <- sesync[pi.non.sesync.index,]
# Changing Career Stage of non-SESYNC postdocs to "early career"
sesync$`Career Stage`[pi.non.sesync.index] <- "early career"

# I want to figure out why the postdocs are mostly removed
as.vector(unique(sesync[which(sesync$`Career Stage` == "postdoc"),"Last Name"]))
unique(unique.sesync[which(unique.sesync$`Career Stage` == "postdoc"),"Last Name"])




####### Addressing Question 1 ####### 
# Cleaning up variable "Academic or Non-Academic?"
sesync$`Academic or Non-Academic?` <- clean_strings(sesync$`Academic or Non-Academic?`)

# Boolean of rows that have duplicates somewhere in the data 

duplicates <- duplicated(sesync[, c("Last Name", "First Name")]) 
dups <- sesync[which(duplicates),]

# Subset of SESYNC data where each First/Last Name combo is represented only ONCE - if people appear in different roles, only the first will be counted
unique.sesync <- sesync[-which(duplicates),]

# The number of responses for the Academic or Non-Academic Question
sum(!is.na(clean_strings(unique.sesync$`Academic or Non-Academic?`))) #4457 responses
# The number of "academic" responses 
sum(unique.sesync$`Academic or Non-Academic?`[which(!is.na(clean_strings(unique.sesync$`Academic or Non-Academic?`)))] == "academic") # 3369

# The percentage of "academic" responses from the total number of responses to the Academic or Non-Academic Question
sum(unique.sesync$`Academic or Non-Academic?`[which(!is.na(clean_strings(unique.sesync$`Academic or Non-Academic?`)))] == "academic") /sum(!is.na(clean_strings(unique.sesync$`Academic or Non-Academic?`))) # 0.756 academic, so non-academic is 0.244.

# Academic vs. Non-academic individuals (counts and percentages by year)
# See Table below for yearly breakdown
df.1 <- table(clean_strings(unique.sesync$`Academic or Non-Academic?`), unique.sesync$`Project Funding Year`)
df <- data.frame(year = rep(colnames(df.1), each = 2), total = as.vector(df.1), type = rep(rownames(df.1), length(colnames(df.1))) )

# Barplot of Number of Academic/Non-Academic Participants by Year
plot <- ggplot(df, aes(year, total, fill=type)) + geom_bar(stat = "identity", position = 'dodge') +
  xlab("Year") + ylab("Number of Participants")+ guides(fill=guide_legend(title="Type"))

plot

# Total responses of Academic or Non-Academic Question by Year
counts<- table(unique.sesync$`Project Funding Year`)

# Barplot of Percentage of Academic/Non-Academic Participants by Year
df %>% group_by(year) %>% 
  mutate(total_counts = sum(total)) %>%
  mutate(percent = total/total_counts) %>%  ggplot( aes(year, percent, fill=type)) + geom_bar(stat = "identity", position = 'dodge') +
  xlab("Year") + ylab("% of Participants")+ guides(fill=guide_legend(title="Type"))


# Academic/Non-Academic Pie chart Across all Years
# Create data frame - Note that these numbers are *not* dynamic (need to be manually changed to change the plot)
aca.pie <- data.frame(
  Type = c("Academic", "Non-Academic"),
  percent = c(0.756, 0.244)
)

# Compute label positions
aca.pie <- aca.pie %>%
  mutate(
    #ypos = cumsum(percent) - (percent / 2),  # Midpoint of each slice
    label = paste0(Type, "\n", percent(percent, accuracy = 0.1))  # Category + % label
  )

# Create pie chart with adjusted label positions
ggplot(aca.pie, aes(x = "", y = percent, fill = Type)) +
  geom_bar(stat = "identity", width = 1, color = "black") +  
  coord_polar("y", start = 0) +  
  # Add labels inside slices with better positioning
  geom_text(aes(label = label),  position = position_stack(vjust = 0.6), color = "white", size = 6, fontface = "bold") +
  
  # Theme adjustments
  theme_void() +  
  theme(
    legend.position = "none"
  )


####### Addressing Question 2 ####### 

# Just pulling those individuals who marked themselves as "academic" from Academic or Non-Academic? question
academic.index <- which(unique.sesync$`Academic or Non-Academic?` == "academic")
academics <- unique.sesync[academic.index,]


# Combining "gep" into "geo" category
academics$`New NSF Directorate`[which(academics$`New NSF Directorate` == "gep")] <- "geo"

# Print out the percentages for the academic areas of the academics
sort(table(clean_strings(academics$`Academic Areas – Assigned JAN 2024`)))/sum(table(academics$`Academic Areas – Assigned JAN 2024`)) * 100

# Barplot of the above percentages across all years
barplot(sort(table(academics$`New NSF Directorate`))/sum(table(academics$`New NSF Directorate`)), 
        ylim = c(0, 0.5), col = "cornflowerblue", ylab = "% of Academic Participants", xlab = "NSF Directorate")

###### This is the start of a large code block to make the NSF areas pie chart ###### 
nsf.pie <- data.frame(Directorate = names(table(academics$`New NSF Directorate`))
, percent = prop.table(table(academics$`New NSF Directorate`)))

# Identify the top 4 largest slices
nsf.pie <- nsf.pie %>%
  arrange(desc(percent.Freq)) %>%
  mutate(
    is_large = row_number() <= 4,  # Mark top 4 slices as "large"
    label = paste0(Directorate, "\n", percent(percent.Freq, accuracy = 0.1))  # Label with category + %
  )
# Compute label positions
nsf.pie <- nsf.pie %>%
  mutate(
    #ypos = cumsum(percent) - (percent / 2),  # Midpoint of each slice
    label = paste0(Directorate, "\n", percent(percent.Freq, accuracy = 0.1))  # Category + % label
  )
nsf.pie$Directorate2 <- factor(nsf.pie$Directorate, levels = (as.character(nsf.pie$Directorate)))

nsf.pie$label2 <- nsf.pie$label
nsf.pie$label2[5:8] <- NA
nsf.pie$label[1:4] <- NA
nsf.pie$pos = (cumsum(c(0, nsf.pie$percent.Freq)) + c(nsf.pie$percent.Freq, .01))[1:nrow(nsf.pie)]


# Create pie chart with adjusted label positions
ggplot(nsf.pie, aes(x = "", y = percent.Freq, fill = Directorate2)) +
  geom_bar(stat = "identity", width = 1, color = "black") +  
  coord_polar("y", start = ) +  
  
  # Add labels inside slices with better positioning
  geom_text(aes(label = label2),  position = position_stack(vjust = 0.5), color = "white", size = 6, fontface = "bold") +
  # Outside labels for small slices
  geom_text_repel(
    #position = position_stack(vjust = 1),
    aes(y = (1- pos) + 0.01, label = label, x = 1.4),  
    size = 5, fontface = "bold",
    nudge_x= 0.5,  # Pushes text away from the pie
    #direction = "y",  # Spreads labels vertically
    segment.size = 0.5  # Draws leader lines
  ) +
  # Theme adjustments
  theme_void() +  
  theme(
    legend.position = "none"
  ) 
###### This is the end of the code block to make the NSF areas pie chart ###### 

# Addressing Question 3

# Gender percentages by NSF Area among Academics
prop.table(table(academics$Gender,academics$`New NSF Directorate`), margin = 2)
# Overall percentages of NSF Area among Academics
prop.table(table(academics$`New NSF Directorate`))
# Gender counts by NSF Area among Academics
table(academics$Gender,academics$`New NSF Directorate`)
# Gender counts by NSF Area among all SESYNC particpants (note many non-academics likely do not have NSF area)
table(unique.sesync$Gender,unique.sesync$`New NSF Directorate`)
# Gender counts by Project Type across all SESYNC participants
table(sesync$Gender,sesync$`Project Type`)
# Percentage of "female" across all SESYNC participants
sum(unique.sesync$Gender == "female", na.rm  = TRUE)/nrow(unique.sesync)

### Start of a code block to make Gender percentage pie charts by NSF area ###
# Create data frame
nsf_data <- data.frame(
  Gender = c("Female", "Male"),
  bio = c(555,500),
  cise = c(88,166),
  edu = c(62, 24),
  geo = c(105, 107),
  hum = c(14, 21),
  mps = c(39, 33),
  oth = c(36, 45),
  sbe = c(602, 650)
)

# Convert from wide to long format
nsf_long <- nsf_data %>%
  pivot_longer(-Gender, names_to = "Category", values_to = "Count") %>%
  group_by(Category) %>%
  mutate(Percent = Count / sum(Count))  # Compute percentage within each category

# Create pie charts for each category
ggplot(nsf_long, aes(x = "", y = Percent, fill = Gender)) +
  geom_bar(stat = "identity", width = 1, color = "black") +
  coord_polar("y", start = 0) +
  facet_wrap(~Category, ncol = 4) +  # Arrange pie charts in a grid
  geom_text(aes(label = scales::percent(Percent, accuracy = 0.1)), 
            position = position_stack(vjust = 0.5), color = "white", size = 5, fontface = "bold") +
  theme_void() +  
  theme(legend.position = "bottom")  # Moves legend below the charts

### End of a code block to make Gender percentage pie charts by NSF area ###



# Addressing Question #4

# Cleaning up the Academic Areas variable for the academics dataset
academics$`Academic Areas – Assigned JAN 2024` <- clean_strings(academics$`Academic Areas – Assigned JAN 2024`)
#x <- rep(apply(table(academics$`Academic Areas – Assigned JAN 2024`, academics$`Project Funding Year`),2, sum), each = length(unique(academics$`Academic Areas – Assigned JAN 2024`)))

# Overall Percentages of Academic Areas for all SESYNC academics
prop.table(table(academics$`Academic Areas – Assigned JAN 2024`))

# Table of Academic Area percentages of all SESYNC academics by year
df.4 <- as.data.frame(prop.table(table(academics$`Academic Areas – Assigned JAN 2024`, academics$`Project Funding Year`), margin = 2))

# Counts of Academic Projects Across Funding Years
counts <- table(academics$`Project Funding Year`)

# Counts of Gender breakdown across Project Types
table(sesync$Gender, sesync$`Project Type`)

# Re-naming for clarity
names(df.4)[1] <- "Academic Area"
# Line plot of Academic Area participants % through the years
df.4 %>%  ggplot(aes(Var2, Freq, col = `Academic Area`)) + geom_point() + geom_line(group = df.4$`Academic Area`) +
# Add n for each year
  geom_text(data = as.data.frame(counts), aes(label = paste("n=",Freq), x = Var1, y = 0.4), color = "black") + xlab("Year")



######## Subsetting the prior plot by certain Academic Areas ######## 
index.1 <- which(df.4$`Academic Area` %in% c("ecology", "agricultural sciences", "chemistry", "ocean sciences", 
                 "atmospheric sciences", "hydrology", "engineering"))
df.5 <- df.4[index.1,]

df.5 %>%  ggplot(aes(Var2, Freq, col = `Academic Area`)) + geom_point() + geom_line(group = df.5$`Academic Area`) +
  # Add n for each year
  geom_text(data = as.data.frame(counts), aes(label = paste("n=",Freq), x = as.numeric(Var1), y = 0.4), color = "black") + xlab("Year")

df.6 <- df.4[-index.1,]

df.6 %>%  ggplot(aes(Var2, Freq, col = `Academic Area`)) + geom_point() + geom_line(group = df.6$`Academic Area`) +
  # Add n for each year
  geom_text(data = as.data.frame(counts), aes(label = paste("n=",Freq), x = as.numeric(Var1), y = 0.25), color = "black") + xlab("Year")

######## End of Subsetting the prior plot by certain Academic Areas ######## 

# Addressing Question # 5
# Cleaning up Race variable
unique.sesync$Race <- clean_strings(unique.sesync$Race)
# Collecting combinations to be classified as "multi"
multi <- c("african american white","asian african american white", "asian white", "native american white")
unique.sesync$Race[which(unique.sesync$Race %in% multi)] <- "multi"

# Cleaning up "no answer" responses and changing to NA
unique.sesync$Race<-sub(" no answer", "", unique.sesync$Race)
unique.sesync$Race[which(unique.sesync$Race == "no answer")] <- NA

unique.sesync$Race[which(unique.sesync$Race == "native american")] <- "nat amer"
unique.sesync$Race[which(unique.sesync$Race == "african american")] <- "black"

# Cleaning up Ethnicity variable
unique.sesync$Ethnicity <- tolower(unique.sesync$Ethnicity)
unique.sesync$Ethnicity[which(unique.sesync$Ethnicity == "no_answer")] <- NA

# A specific change that was requested
unique.sesync$Race[which((unique.sesync$Race == "multi") & (unique.sesync$Ethnicity %in% c("hispanic", "latino")))] <- "hisp lat"

# Counting the # of individuals with missing values for Race
sum(is.na(unique.sesync$Race))
# Overall counts of the Race variable
table(unique.sesync$Race)
# % of Race categories
prop.table(table(unique.sesync$Race))

####### This makes a nice barplot of Race % #######
# Create data frame
race_data <- data.frame(
  Category = c("Other", "Nat Amer", "Multi", "Hisp Lat", "Black", "Asian", "White"),
  Percent = c(0.0009185548, 0.0055113288, 0.0156154317, 0.0477648500, 
              0.0489895897, 0.1031843233, 0.7780159216)
)

# Create bar plot
ggplot(race_data, aes(x = reorder(Category, -Percent), y = Percent, fill = Category)) +
  geom_bar(stat = "identity", color = "black", width = 0.7) +  
  geom_text(aes(label = scales::percent(Percent, accuracy = 0.1)), 
            vjust = -0.5, size = 5, fontface = "bold") +  # Add percentage labels above bars
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +  # Format y-axis as percentages
  labs(x = "Race/Ethnicity", y = "Percentage", title = "Distribution by Race/Ethnicity") +
  theme_minimal() +  
  theme(legend.position = "none", 
        text = element_text(size = 14),
        axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for readability

####### End of nice barplot of Race % #######

# Addressing Question 6

# Cleaning Country data
unique.sesync$Country <- clean_strings(unique.sesync$Country)

# Get world map data
world <- map_data("world")

# Filter world map data for countries in vector "x"
x <- c("united states", unique(unique.sesync$Country), "uk", "Solomon Islands")
highlighted_countries <- world %>%
  filter(clean_strings(region) %in% x)

# Plot world map highlighting countries with SESYNC participants
ggplot() +
  geom_map(data = world, map = world,
           aes(long, lat, map_id = region),
           color = "black", fill = "lightgray", size = 0.25) +
  geom_map(data = highlighted_countries, map = highlighted_countries,
           aes(long, lat,fill = "highlighted", map_id = region), color = "black", linewidth = 0.25) +
  scale_fill_manual(name = NULL, values = c("highlighted" = "cornflowerblue"),
                    guide = "none") +
  coord_fixed()

# Creating a variable for whether the country is the US or not
unique.sesync$USAorno <- (unique.sesync$Country == "usa")
# Subset of non-US participants
nonusa <- unique.sesync[-which(unique.sesync$USAorno),]
# %'s of non-US countries
table(unique.sesync$USAorno)/sum(table(unique.sesync$USAorno))
nonus.table <- table(nonusa$Country)/sum(table(nonusa$Country))

# Indicies of various regions
africa.index <- which(names(nonus.table) %in% c("burkina faso", "democratic republic of the congo", "east africa", "egypt", "eritrea", "ethiopia", "kenya", "madagascar", "malawi", "mozambique", "niger","nigeria", "rwanda", "sierra leone", "south africa", "tanzania", "uganda", "zambia", "zimbabwe"))

south.america.index <- c("argentina", "bolivia", "brazil", "chile", "colombia", "paraguay", "peru", "uruguay", "venezuela")

europe.index <- c("austria", "belgium", "czech republic", "denmark", "finland", "france", "germany", "hungary", "iceland", "ireland", "italy", "latvia", "luxembourg", "netherlands", "norway", "poland", "portugal", "spain", "sweden", "switzerland", "united kingdom")

central.index <- c("antigua", "aruba", "barbados","costa rica", "dominican republic", "el salvador", "guatemala", "honduras","jamaica", "mexico", "nicaragua","panama", "puerto rico","trinidad and tobago" )

# Overall %'s from specific regions
sum(nonus.table[africa.index])
sum(nonus.table[south.america.index])
sum(nonus.table[europe.index])
sum(nonus.table[c(central.index,south.america.index)])

###### Plotting the US Map showing participant densities ######
usa.index <- which(unique.sesync$Country == "usa") 
usa <- unique.sesync[usa.index,]
usa$`US State` <- clean_strings(usa$`US State`)
# Some fixes 
{
usa$`US State`[which(usa$`US State` == "arizona")] <- "az"
usa$`US State`[which(usa$`US State` == "bc wa")] <- "wa"
usa$`US State`[which(usa$`US State` == "california")] <- "ca"
usa$`US State`[which(usa$`US State` == "florida")] <- "fl"
usa$`US State`[which(usa$`US State` == "georgia")] <- "ga"
usa$`US State`[which(usa$`US State` == "maryland")] <- "md"
usa$`US State`[which(usa$`US State` == "massachusetts")] <- "ma"
usa$`US State`[which(usa$`US State` == "ohio")] <- "oh"
usa$`US State`[which(usa$`US State` == "texas")] <- "tx"
usa$`US State`[which(usa$`US State` == "vermont")] <- "vt"
usa$`US State`[which(usa$`US State` == "virginia")] <- "va"
usa$`US State`[which(usa$`US State` == "washington")] <- "wa"
usa$`US State`[which(usa$`US State` == "wisconsin")] <- "wi"
usa$`US State`[which(usa$`US State` == "ka")] <- "ks"

}


state.df <- data.frame(state = names(table(usa$`US State`)), values = table(usa$`US State`))
state.df <- state.df[,-2]
colnames(state.df)[2]<- "values"

# Just showing the counts isn't that useful since certain states dominate
plot_usmap(data = state.df[state.df$state != "md",]) + 
  scale_fill_continuous(
    low = "white", high = "red", name = "SESYNC US Participants", label = scales::comma
  ) + theme(legend.position = "right")

# You can see that here
plot(sort(state.df$values))
# But taking the log will smooth things out
plot(log(sort(state.df$values)))
# So that's what we do
state.df$values <- log(state.df$values)
plot_usmap(data = state.df) + 
  scale_fill_continuous(
    low = "white", high = "red", name = "Log SESYNC US Participants", label = scales::comma
  ) + theme(legend.position = "right")
###### End of Plotting the US Map showing participant densities ######

# Subsetting the Non-Academic participants only and cleaning some of their "Institutional Status" responses
non.aca <- unique.sesync[which(unique.sesync$`Academic or Non-Academic?` == "non academic"),]
non.aca$`Institutional Status`[which(non.aca$`Institutional Status` == "COLLEGE_STAFF,NONPROFIT")] <- "nonprofit"
non.aca$`Institutional Status`[which(non.aca$`Institutional Status` == "GRAD,NONPROFIT")] <- "nonprofit"
non.aca$`Institutional Status`[which(non.aca$`Institutional Status` == "POSTDOC")] <- NA
non.aca$`Institutional Status` <- clean_strings(non.aca$`Institutional Status`)

# % of different non-academic industries
prop.table(table(non.aca$`Institutional Status`))

############ Making a Pie chart for % of non-academic industries ############
nonaca.pie <- data.frame(Type = names(prop.table(table(non.aca$`Institutional Status`)))
, percent = prop.table(table(non.aca$`Institutional Status`)))


nonaca.pie$Type <- c("Government", "Industry",   "Nonprofit",  "Other"  )
# Compute label positions
nonaca.pie <- nonaca.pie %>%
  mutate(
    label = paste0(Type, "\n", percent(percent.Freq, accuracy = 0.1))  # Category + % label
  )
 
# Create pie chart with adjusted label positions
ggplot(nonaca.pie, aes(x = "", y = percent.Freq, fill = Type)) +
  geom_bar(stat = "identity", width = 1, color = "black") +  
  coord_polar("y", start = 0) +  
  
  # Add labels inside slices with better positioning
  geom_text(aes(label = label),  position = position_stack(vjust = 0.5), color = "white", size = 6, fontface = "bold") +
  
  # Theme adjustments
  theme_void() +  
  theme(
    legend.position = "none"
  )

############ End of Making a Pie chart for % of non-academic industries ############

# Creating a variable that is "academic" if the participant specified that or their industry if "non-academic"
unique.sesync$institutty <- ifelse(unique.sesync$`Academic or Non-Academic?` == "academic", unique.sesync$`Academic or Non-Academic?`, 
                                     unique.sesync$`Institutional Status`)

# Cleaning up this new variable
unique.sesync$institutty[which(unique.sesync$institutty == "COLLEGE_STAFF,NONPROFIT")] <- "nonprofit"
unique.sesync$institutty[which(unique.sesync$institutty == "Industry")] <- "industry"

# An (ugly) barplot of this new variable
barplot(sort(prop.table(table(unique.sesync$institutty))), ylim = c(0,1))


## Now specifically looking at Academic/Non-Academic breakdown of SESYNC Pursuits
pursuits <- subset(sesync, `Project Type` == "Pursuit")
prop.table(table(pursuits$`Academic or Non-Academic?`)) # Non-academic: 27.2%



########### SESYNC Publication Analyses 
# Reading in Publication Data (Pulled from Zotero database)
pubs <- read.csv("~/Dropbox/SESYNC/SESYNC Publications_citations.csv")

list.of.tags <- str_split(pubs[, "Manual.Tags"], ";")

# Function to filter strings
filter_strings <- function(x) {
  sapply(x, function(str) {
    # Remove leading whitespace and check if the first non-whitespace character is "2"
    first_char <- gsub("^\\s*", "", str)  # Remove leading whitespace
    substr(first_char, 1, 1) == "2"        # Check if first non-whitespace character is "2"
  })
}

list.of.tags[[21]] # example of publication with no project code

# Pulling SESYNC Project Codes associated with publications (some pubs are associated with more than 1)
project.codes <- list()
for(i in 1:nrow(pubs)){
  x <- list.of.tags[[i]][grep("-",list.of.tags[[i]])]
  if(is_empty(x)){
    project.codes[[i]] <- NA
    next}
   project.codes[[i]] <- gsub("^\\s*", "",x[filter_strings(x)])
}

# Pull project codes with at least publication
pcs.with.pubs <- gsub("^\\s*", "", unique(unlist(project.codes))) # 218 unique project codes with at least 1 publication (one in the vector is NA)

length(unique(sesync$`Project Code`)) # 352 unique project codes in SEADMIN, i.e. ~ 134 non-publishing projects (38.1%)
pcs <- unique(sesync$`Project Code`) # All SESYNC project codes


# Create a data frame where each project code in SESYNC has its own row with just the number of pubs associated with it in Zotero.

pubs.frame <- data.frame(Projects = pcs, Count = rep(0, length(pcs)))
for(i in 1:length(pcs)){
  print(i)
  for(j in 1:nrow(pubs)){
   if(pubs.frame$Projects[i] %in% project.codes[[j]]){pubs.frame$Count[i] <- pubs.frame$Count[i] + 1}
  }
}

# Next, we add a column to this dataframe for the Project Type
project.types <- read_xlsx("~/Dropbox/SESYNC/project_types.xlsx")
### GPT function to pull the Project Type from the Project Code
extract_first_contiguous_substring <- function(string) {
  # Use regular expression to find the first contiguous substring of characters
  if(is.na(string)){return(NA)}
  match <- regexpr("[A-Za-z]+", string)
  
  if (match == -1) {
    # If no match is found, return an empty string
    return("")
  } else {
    # Extract the matched substring
    return(substr(string, match, match + attr(match, "match.length") - 1))
  }
}

p.types <- as.vector(sapply(pubs.frame$Projects, extract_first_contiguous_substring))

# This fills in the Project Type from the Project Code
pubs.frame$`Project Type` <- NA
for(i in 1:nrow(pubs.frame)){
  if(!(p.types[i] %in% project.types$`Letters in Code`)){next}
  p.type <- project.types$`Project Type`[which(project.types$`Letters in Code` == p.types[i] )]
  pubs.frame$`Project Type`[i] <- p.type
}

# Looking at the counts of each project type in this new publications dataframe
table(pubs.frame$`Project Type`)

# How many publications are associated with each Project type
pubs.frame %>% group_by(`Project Type`) %>%
  summarize(sum(Count))
# Postdoc Fellowships and Pursuits produced the most publications (by far)

# Publication Question #1:  "Were nonacademics included on pubs and if so, what nonacademic sectors were best represented? i.e., for ALL pubs, what % have nonacademic partners as authors and what sector were they in? Ignore authors that are not on teams (i.e., listed on spreadsheet)". 

# For each publication in the database, I create a list of last names and first names. 

# Pulling the authors in each publication (each publication is an list item)
authors <- str_split(pubs$Author,";")

# 2 GPT functions for pulling last and first names, respectively
pull_last_or_first <- function(lst) {
  result <- lapply(lst, function(vec) {
    last_or_first <- sapply(vec, function(str) {
      if (grepl(",", str)) {
        first_word <- trimws(strsplit(str, ",")[[1]][1])
        return(first_word)
      } else {
        last_word <- tail(trimws(strsplit(str, "\\s+")[[1]]), 1)
        return(last_word)
      }
    })
    return(last_or_first)
  })
  return(result)
}

pull_first_or_last <- function(lst) {
  result <- lapply(lst, function(vec) {
    first_or_last <- sapply(vec, function(str) {
      if (grepl(",", str)) {
        last_word <- tail(trimws(strsplit(str, ",")[[1]]), 1)
        return(last_word)
      } else {
        first_word <- trimws(strsplit(str, "\\s+")[[1]][1])
        if(is.na(first_word)){return(NA)}
        if(first_word == ""){first_word <- trimws(strsplit(str, "\\s+")[[1]][2])}
        return(first_word)
      }
    })
    return(first_or_last)
  })
  return(result)
}

# Lists of last names and first names (sometimes with middle initials)
authors.lasts <- pull_last_or_first(authors) 
authors.firsts <- pull_first_or_last(authors)

# Starting a counter for non-academic publications
non.aca.pubs <- 0
# Making vectors of last and first names of the non-academic SESYNC participants
last.names<- non.aca$`Last Name`
first.names<- non.aca$`First Name`

# Starting a placeholder for indices
pull.these <- NA


for(i in 1:nrow(pubs)){
  authors.lasts[[i]] <- clean_strings(authors.lasts[[i]]) #Cleaning
  authors.firsts[[i]] <- clean_strings(authors.firsts[[i]]) #Cleaning
  already.there <- 0 #Start indicator
  for(j in 1:length(authors.lasts[[i]])){ # within a particular publication, goes through all associated last names
    last.indices <- -1 # just setting these two to non-matching index values
    first.indices <- -2
    if(authors.lasts[[i]][j] %in% last.names){ # for each of the j last names associated with the ith publication, check if it matches one of the non-acdemic last names
      last.indices <- which(last.names == authors.lasts[[i]][j]) # if it does, return all matching indices
      if(authors.firsts[[i]][j] %in% first.names){ # Do the same thing for the first names
        first.indices <- which(first.names == authors.firsts[[i]][j])
      }
    }
    if(!is_empty(intersect(last.indices, first.indices))){ # if no last/first name matches move on, if they do match, change the "already.there" indictor to show that we know the publication is associated with a non-acdemic author, add one to the count of non-academic publications. If the indicator is already "switched" just pull the matching indices.
      if(!already.there){already.there <- TRUE; non.aca.pubs <- non.aca.pubs + 1;pull.these <- c(pull.these, intersect(last.indices, first.indices) )}else{
        pull.these <- c(pull.these, intersect(last.indices, first.indices) )
      }}
    
  }
}

pull.these <- pull.these[-1] # First is NA by design
pull.these <- unique(pull.these) # We only need to pull individuals once

non.aca.pubs <- non.aca[pull.these,] # These are the non-academic individuals associated with non-academic publications

prop.table(table(non.aca.pubs$`Institutional Status`)) # Percentage of industries involved in non-academic publications

# 194 non-academic publications

# Publication Question #2: What fraction of our teams were interdisciplinary? For Pursuits (not Grad Pursuits) only: A)  disciplinary diversity of authors on publications i.e., per publication, how many of the authors are from different disciplines (=ACADEMIC AREA); B) Do the same thing but now how many authors were from different NSF directorates (e.g., BIO, GEO, etc). 

# Go through list of pubs and pull matched authors' Academic Area
# The current issue with this is that it is not limited to pursuits, but includes publications from all sources
# If I don't already have a dataset that has the associated project codes pulled, I can pull the project codes and filter on those that are pursuits

# Pulling the academic areas associated with each publication
list.of.academic.areas <- vector("list", nrow(pubs))
for(i in 1:nrow(pubs)){
  for(j in 1:length(authors.lasts[[i]])){
  if(authors.lasts[[i]][j] %in% sesync$`Last Name`){
    last.indices <- which(sesync$`Last Name` == authors.lasts[[i]][j])
    if(authors.firsts[[i]][j] %in% sesync$`First Name`){
      first.indices <- which(sesync$`First Name`== authors.firsts[[i]][j])
    }
    author.indices <- intersect(last.indices, first.indices)
    list.of.academic.areas[[i]]<- c(list.of.academic.areas[[i]],unique(sesync$`Academic Areas – Assigned JAN 2024`[author.indices]))
  }
  }
}

# Just keeping one of each unique academic area
for(i in 1:nrow(pubs)){
  if(is_empty(list.of.academic.areas[[i]])){ list.of.academic.areas[[i]] <-NA}
  list.of.academic.areas[[i]] <- unique(list.of.academic.areas[[i]])
}


# Pulling the Project Types associated with each publication
long.project.codes <- lapply(project.codes, function(x) sapply(x,extract_first_contiguous_substring))
long.pubs.codes <- rep(FALSE, length(long.project.codes))

# Identify "Pursuits" via the Project Codes "C", "T", or "PI"
for(i in 1:length(long.project.codes)){
  p.type <- FALSE
  for(j in 1:length(long.project.codes[[i]])){
  if(p.type){next}
  if(is_empty(long.project.codes[[i]][j])){next}
  if(is.na(long.project.codes[[i]][j])){next}
  if(!(long.project.codes[[i]][j] %in% project.types$`Letters in Code`)){next}
  p.type <- long.project.codes[[i]][j] %in% c("C", "T", "PI")
  long.pubs.codes[i] <- p.type
  }
}

pursuit.index <- which(long.pubs.codes) # Indices for pursuits
length(pursuit.index) # 345 of the publications associated with a "Pursuit" project code

########## Calculating the number of publications associated with more than one academic area (Pursuits)
count.of.areas <- lapply(list.of.academic.areas, function(x){sum(!is.na(x))})
sum(unlist(count.of.areas)[pursuit.index] > 1)

# at least 187 out of 345 Pursuit publications were associated with more than one academic discipline (> 0%)

################## The same, but for NSF Directories
list.of.academic.areas <- vector("list", nrow(pubs))
for(i in 1:nrow(pubs)){
  for(j in 1:length(authors.lasts[[i]])){
    if(authors.lasts[[i]][j] %in% sesync$`Last Name`){
      last.indices <- which(sesync$`Last Name` == authors.lasts[[i]][j])
      if(authors.firsts[[i]][j] %in% sesync$`First Name`){
        first.indices <- which(sesync$`First Name`== authors.firsts[[i]][j])
      }
      author.indices <- intersect(last.indices, first.indices)
      #if(is_empty())
      list.of.academic.areas[[i]]<- c(list.of.academic.areas[[i]],unique(sesync$`New NSF Directorate`[author.indices]))
    }
    
  }
}

for(i in 1:nrow(pubs)){
  if(is.null( list.of.academic.areas[[i]])){ list.of.academic.areas[[i]] <-NA}
  list.of.academic.areas[[i]] <- unique(list.of.academic.areas[[i]])
}
list.of.academic.areas

count.of.areas <- lapply(list.of.academic.areas, function(x){sum(!is.na(x))})

sum(unlist(count.of.areas)[pursuit.index] > 1)
# at least 157 out of 345 (45.5%)

# Next, we count the number of individuals associated with each project code.
project.size <- data.frame(Projects = names(table(sesync$`Project Code`)), Size = table(sesync$`Project Code`))
project.size <- project.size[,c(1,3)] # Duplicated Column
names(project.size)[2] <- "Size"
# Put this info into the publication data frame
pubs.count.size <- merge(project.size, pubs.frame)

##### Start of Regression Models #########
# First looking the raw association of group size with publications of a project (not accounting for important factors like time since funding, etc.)
nb.model <- glm.nb(Count ~ Size, data = pubs.count.size)
summary(nb.model) # Raw association shows larger size associated with fewer publications

qp.model <- glm(Count ~ Size, data = pubs.count.size, family = quasipoisson())
summary(qp.model) # Also run the same analysis with quasi-poisson model to see if association is sensitive to the model, looks the same

# Breaking down gender makeup of projects
project.gender <- data.frame(Projects = names(prop.table(table(sesync$Gender, sesync$`Project Code`), margin = 2)[1,]), female = prop.table(table(sesync$Gender, sesync$`Project Code`), margin = 2)[1,])

# Adding this information to the publication data frame
pubs.count.size.gender <- merge(pubs.count.size, project.gender)

# Some Career Stage Variable Cleaning
sesync$`Career Stage`[which(sesync$`Career Stage` == "midcareer")] <- "mid career"
sesync$`Career Stage`[which(sesync$`Career Stage` == "Senior")] <- "senior career"
unique.sesync$`Career Stage`[which(unique.sesync$`Career Stage` == "Senior")] <- "senior career"
unique.sesync$`Career Stage`[which(unique.sesync$`Career Stage` == "midcareer")] <- "mid career"

# Career Stage Breakdown by Project Code
prop.table(table(sesync$`Career Stage`, sesync$`Project Code`), margin = 2)
project.career.props <- data.frame(Projects = names(prop.table(table(sesync$`Career Stage`, sesync$`Project Code`), margin = 2)[1,]), 
                               early.career = prop.table(table(sesync$`Career Stage`, sesync$`Project Code`), margin = 2)[1,],
                               mid.career =   prop.table(table(sesync$`Career Stage`, sesync$`Project Code`), margin = 2)[2,],
                               postdoc =  prop.table(table(sesync$`Career Stage`, sesync$`Project Code`), margin = 2)[3,],
                               senior.career = prop.table(table(sesync$`Career Stage`, sesync$`Project Code`), margin = 2)[4,],
                               student = prop.table(table(sesync$`Career Stage`, sesync$`Project Code`), margin = 2)[5,]
                               )
# Add this data to the publication dataframe
pubs.count.size.gender.career <- merge(pubs.count.size.gender, project.career.props)

# Now adding career stage information to the publication count model
nb.model.2 <- glm.nb(Count ~ Size + female + early.career + mid.career + postdoc + senior.career, data = pubs.count.size.gender.career)
summary(nb.model.2) # better AIC value; residual deviance 359.92 (9 observations removed due to missingness)
summary(nb.model) # residual deviance 365.09

# The final NB models indicate that projects with postdocs published significantly more often and that senior career were also associated with more more publications, although the effect size was moderate in comparison. 

############ Adding academic diversity to the analysis
# For academic diversity, I'll calculate the entropy measure referenced in the document Margaret sent to me

# Proportions of academic areas/NSF directories per Project Code
academic.areas.table <- prop.table(table(sesync$`Academic Areas – Assigned JAN 2024`, sesync$`Project Code`), margin = 2)
nsf.areas.table <- prop.table(table(sesync$`New NSF Directorate`, sesync$`Project Code`), margin = 2)

# Just Pursuits from the large SESYNC dataset
sesync.pursuit <- sesync[which(sesync$`Project Type` == "Pursuit"),]

# In the Pursuits, what was the breakdown of academic areas
academic.areas.table.pursuits <- prop.table(table(sesync.pursuit$`Academic Areas – Assigned JAN 2024`, sesync.pursuit$`Project Code`), margin = 2)

# Indices for "Natural Sciences" and "Humanities", respectively
aca.nat.sci.index <- c(6,3,14,16,15,4,1,11,5,9)
aca.hum.index <- c(12,18,19,7,22,23,24,2,20,10,21,13)


length(unique(sesync.pursuit$`Project Code`)) # 99 SESYNC Pursuit Project Codes

nat.sci.index <- c(1,2,4,7)

# The entropy measure referenced
entropy <- function(x){
  y <- x[-which(x == 0)]
  -sum(y*log(y))
}

anynat.sci <- (apply(academic.areas.table.pursuits[aca.nat.sci.index,], 2, sum) != 0) # An indicator of whether a pursuit had a natural science associated with it
anyhum <- (apply(academic.areas.table.pursuits[aca.hum.index,], 2, sum) != 0) # An indicator of whether a pursuit had a humanity associated with it

sum(anynat.sci== TRUE & anyhum == TRUE) # 92 of the 99 Pursuits had both a natural science and humanity associated with them
# Calculating the entropies for academic area and NSF Directorate
academic.entropies <- apply(academic.areas.table, 2, entropy) 
nsf.entropies <- apply(nsf.areas.table, 2, entropy)


nat.sci <- apply(nsf.areas.table[nat.sci.index,], 2, sum) # Percentage of Natural Science participation in each of the 352 Projects

aca.entropies <- data.frame(Projects = names(academic.entropies), academic = academic.entropies) # Adding Academic entropies to Project Codes
nsf.entropies <- data.frame(Projects = names(nsf.entropies), nsf = nsf.entropies, sbe = nsf.areas.table[9,],
                            nat.sci = nat.sci) 

# Adding NSF entropies, natural science % and SBE % to publication dataframe
pubs.count.size.gender.career.entropies <- merge(pubs.count.size.gender.career, aca.entropies)
pubs.count.size.gender.career.entropies <- merge(pubs.count.size.gender.career.entropies, nsf.entropies)

# Calculating the # of years since funding
pubs.count.size.gender.career.entropies$years <- 2024 - as.numeric(str_extract(pubs.count.size.gender.career.entropies$Projects, "\\d+"))

# Looking at the association between publications and academic diversity (as measured by entropy), accounting for time since funding, among Pursuits
aca.entropy.model.nb <- glm.nb(Count ~ academic + offset(log(years)), data =  subset(pubs.count.size.gender.career.entropies, `Project Type` == "Pursuit"))
summary(aca.entropy.model.nb)

# Checking a Zero-inflated model
library(pscl)
aca.entropy.model.zero <- zeroinfl(Count ~ academic + offset(log(years)) | academic + years, data =  subset(pubs.count.size.gender.career.entropies, `Project Type` == "Pursuit"))
summary(aca.entropy.model.zero) # academic diversity is associated with higher publication in the zero-inflated model
AIC(aca.entropy.model.zero) # but the AIC is higher than the negative-binomial model indicating a worse overall fit

# Vuong Test prefers the negative-binomial model over zero-inflated model
vuong(aca.entropy.model.nb, aca.entropy.model.zero)

# Looking at the association between publications and NSF Directorate diversity (as measured by entropy), accounting for time since funding,
nsf.entropy.model.nb <- glm.nb(Count ~ nsf + offset(log(years)), data = pubs.count.size.gender.career.entropies)
summary(nsf.entropy.model.nb)


# Answering Publication Question #4: What factors influenced productivity? For all publications, # pubs by team (project code) size, gender, and career stage of participants.

# First looking at the correlations between variables in our dataset (projects with at least 4 years since funding and non-postdoc fellowships)
library(corrplot)
corrplot(cor(subset(pubs.count.size.gender.career.entropies,  years > 4 & `Project Type` != "Postdoctoral Fellowship")[,-c(1,4)],
    use = "pairwise.complete.obs"),
    method = "color", type = "upper", 
    tl.col = "black", tl.srt = 45, addCoef.col = "black")


# Just Projects classified as "Pursuits"
pursuits <- subset(pubs.count.size.gender.career.entropies,  `Project Type` == "Pursuit") # Here "Postdoc Immersion" is added to "Pursuits" adding some 6 Project codes for a total of 105 Pursuits

# Just Pursuits with 5+ years of funding
pursuits.5plus <- subset(pursuits, years > 4)
# Just Projects with 5+ years of funding
fiveplus <- subset(pubs.count.size.gender.career.entropies, years > 4)

# Looking at association of publication rate with % of natural science project involvement in projects with 5+ years since funding
nb.model.nat <- glm.nb(Count ~  nat.sci + offset(log(years)), data = fiveplus)
summary(nb.model.nat)

##### These are the "Full" publication count/rate models and can be adjusted with offset terms to account for time
# Full Negative Binomial Model (w/o adjusting for funding years) only on Pursuits funded for at least 5 years
nb.model.3 <- glm.nb(Count ~ Size + female + early.career + mid.career + postdoc + senior.career, data = pursuits.5plus)
summary(nb.model.3) 

# Full Negative Binomial Model (w/o adjusting for funding years) only on non-Postdoc Fellowship projects funded for at least 5 years
nb.model.4 <- glm.nb(Count ~ Size + female + early.career + mid.career+postdoc+ senior.career , data = subset(fiveplus,  `Project Type` != "Postdoctoral Fellowship"))
summary(nb.model.4) 

# Combining mid-and senior.career into "Advanced Career" variable
pubs.count.size.gender.career.entropies$advanced.career <- pubs.count.size.gender.career.entropies$mid.career + pubs.count.size.gender.career.entropies$senior.career

# Full Negative Binomial Model (w/o adjusting for funding years) only on non-Postdoc Fellowship projects funded for at least 5 years (w/ advanced career variable)
nb.model.5 <- glm.nb(Count ~ Size + female + early.career + advanced.career + nat.sci + postdoc + offset(log(years)), data = subset(pubs.count.size.gender.career.entropies, years > 4 & `Project Type` != "Postdoctoral Fellowship"))

summary(nb.model.5) 

# Full Negative Binomial Model (adjusting for funding years) only on non-Postdoc Fellowship projects funded for at least 5 years (w/ advanced career variable)
nb.model.6 <- glm.nb(Count ~ Size + female + early.career + advanced.career + postdoc  + offset(log(years)), data = fiveplus)
summary(nb.model.6)

# Now allowing for non-linear relationships of the variables with the outcome via Generalized Additive Models
library(mgcv)
gam.model.1 <- gam(Count ~ s(Size) +s(female) + s(early.career) + s(advanced.career) + s(postdoc) + s(academic) + s(nsf) + offset(log(years)), family = quasipoisson, data = pursuits.5plus, method = "REML")

summary(gam.model.1)
plot(gam.model.1)


########## The following models will look at the outcome of whether or not a project had any publications associated with it
### Publication Yes/No Analysis ###
pubs.count.size.gender.career.entropies$pubyes <- pubs.count.size.gender.career.entropies$Count != 0 # Defining outcome of interest

# Looking at simple associations of publication yes/no with % natural science and SBE, respectively (among 5+ years funded projects)
model.nat.1 <- glm(pubyes ~  nat.sci + years, data = subset(pubs.count.size.gender.career.entropies, years > 4 ), family = binomial())
model.sbe.1 <- glm(pubyes ~  sbe + years, data = subset(pubs.count.size.gender.career.entropies, years > 4 ), family = binomial())

summary(model.nat.1) # non-significant
summary(model.sbe.1) # non-significant

# More complete publication yes/no model (5+ years funding and no postdocs)
bin.model.1 <- glm(pubyes ~ Size + female + early.career + advanced.career + postdoc + nat.sci + years, data = subset(pubs.count.size.gender.career.entropies, years > 4 & `Project Type`!= "Postdoctoral Fellowship"), family = binomial())

summary(bin.model.1)

gam.model.2 <- gam(pubyes ~ Size +s(female) + early.career + s(advanced.career) + s(postdoc) + academic + nsf + years, family = binomial(), data =subset(pubs.count.size.gender.career.entropies, years > 4 & `Project Type`!= "Postdoctoral Fellowship"), method = "REML")

summary(gam.model.2)
plot(gam.model.2)

###################

# Publication Question #5: Did team disciplinary diversity influence productivity and time to publish? For Pursuits only, A) # pubs vs team diversity (ACADEMIC AREA) … need some measure of diversity maybe besides just # of areas; B) do the same thing but use NSF directorates. 


nrow(subset(fiveplus, `Project Type` == "Pursuit")) # 102 of 105 (~97%) Pursuits (99 + 6 Postdoc Immersions) had 5+ years funding

# Scatterplot relating publication rate to academic diversity
plot(log(Count/years)~academic,
     xlab = "Academic Diversity (Entropy)", 
     ylab = "log(Publication Rate)",
     data = subset(fiveplus, `Project Type` == "Pursuit"))

# Regression model assessing the effect of academic diversity on publication rate (5+ funding years) among Pursuits
summary(glm.nb(Count~academic + offset(log(years)), data = subset(fiveplus, `Project Type` == "Pursuit")))

# Scatterplot relating publication rate to NSF Directorate diversity
plot(log(Count/years)  ~ nsf, 
     xlab = "NSF Directorate Diversity (Entropy)", 
     ylab = "log(Publication Rate)",
     data = subset(fiveplus, `Project Type` == "Pursuit"))

summary(glm.nb(Count~ nsf + offset(log(years)), data = subset(fiveplus, `Project Type` == "Pursuit")))

##### Citation Analysis (Measure of Research Impact) #####
# Function to extract citations
extract_number_before_citations <- function(text) {
  # Define the regex pattern
  pattern <- "(\\d+)\\s*citations"
  
  # Search for the pattern in the text
  match <- str_match(text, regex(pattern, ignore_case = TRUE))
  
  if (!is.na(match[1,2])) {
    # Extract the number from the match and convert it to an integer
    number <- as.integer(match[1,2])
    return(number)
  } else {
    return(NULL)
  }
}

# Addressing Publication Question #6: Did team disciplinary diversity, team size, or % of senior career stage folks influence the quality of journals a teams’ pubs were in?  Here, measure journal “quality” by ave (?) ISI Impact Factor (2023).

unique(pubs$Publication.Title) # around 332 unique journals

# Addressing Publication Question #7: Really what interests me more than the last question is: what was the impact of the publications and was it related to team diversity and size?  Many people do believe, maybe rightly so, that impact is higher in journals like Science and Nature which have high ISI factors.  But it would be great to analyze the citation rate of teams’ papers which would need to be standardized/normalized by year of publication. I am truly interested in this as is NSF and the external community. 

# Read in Zotero Citation data
citations <- read_csv("~/Dropbox/SESYNC/SESYNC Publications_citations.csv")
head(citations$Extra) # This is where the citation data is found 


#pubs <- read.csv("~/Dropbox/SESYNC/SESYNC Publications_citations.csv") # Read in pubs again (if starting from this point) 
list.of.tags <- str_split(pubs[, "Manual.Tags"], ";")

# Function to filter strings
filter_strings <- function(x) {
  sapply(x, function(str) {
    # Remove leading whitespace and check if the first non-whitespace character is "2"
    first_char <- gsub("^\\s*", "", str)  # Remove leading whitespace
    substr(first_char, 1, 1) == "2"        # Check if first non-whitespace character is "2"
  })
}

# Pulling project codes associated with publications again
project.codes <- list()
for(i in 1:nrow(pubs)){
  x <- list.of.tags[[i]][grep("-",list.of.tags[[i]])]
  if(is_empty(x)){
    project.codes[[i]] <- NA
    next}
  project.codes[[i]] <- gsub("^\\s*", "",x[filter_strings(x)])
}

extract_number_before_citations <- function(string) {
  # Find the positions of the pattern "number citations" in the string
  match <- gregexpr("\\b\\d+\\b(?=\\s*citations)", string, perl = TRUE)
  # Extract the first match
  first_number <- regmatches(string, match)[[1]]
  if (length(first_number) == 0) {
    return(NA)
  } else {
    return(as.numeric(first_number[1]))
  }
}

# Pulling the number of citation associated with each publication
citations <- sapply(pubs$Extra, extract_number_before_citations, USE.NAMES = FALSE) 

# Variable for years since publication
years.since.pub <- 2025 - pubs$Publication.Year

# Make a list with a space for each of the 352 Projects
citation_count_list <- vector("list", length(pubs.count.size.gender.career.entropies$Projects))

# This loop goes through each of the 352 SESYNC Projects and loops through each of the 929 publications to see if that publication was associated with the Project. If it was, the number of citations associated with that publication divided by the number of years since funding (citation rate) is added to the citation_count_list item associated with that Project. 
for(i in 1:length(pubs.count.size.gender.career.entropies$Projects)){
  current.project <- pubs.count.size.gender.career.entropies$Projects[i]
  for(j in 1:length(project.codes)){
    if(current.project %in% project.codes[[j]]){
      print(j)
      citation_count_list[[i]] <- c(citation_count_list[[i]], citations[j]/years.since.pub[j])
    }
  }
}

# Cleaning up the citation rate list
for (i in seq_along(citation_count_list)) {
  if (is.null(citation_count_list[[i]])) {
    citation_count_list[[i]] <- NA
  }
  for(j in 1:length(citation_count_list[[i]])){
    if(is.nan(citation_count_list[[i]][j])){
      citation_count_list[[i]][j] <- NA
    }
  }
}



sum_citations <- unlist(lapply(citation_count_list, sum, na.rm = TRUE)) # This will give the number of citations per year of each Project
mean_citation.rate <- unlist(lapply(citation_count_list, mean, na.rm = TRUE)) # This will give the average citation rate of papers associated with each Project

# Adding this information to the publication data frame
pubs.count.size.gender.career.entropies$sum.citations <- sum_citations
pubs.count.size.gender.career.entropies$mean.citation.rate<- mean_citation.rate

# Updating correlation plot to include citation information (just Pursuits)
corr.citations <- cor(subset(pubs.count.size.gender.career.entropies,  `Project Type` == "Pursuit")[,-c(1,4)],
    use = "pairwise.complete.obs")
corrplot(corr.citations[, c(ncol(corr.citations) -1, ncol(corr.citations))],
         method = "color", type = "lower", 
         tl.col = "black", tl.srt = 45, addCoef.col = "black")

# I decided to not focus on an "average" citation rate because it seems to me that a 1000 papers with a single citation each has just as much impact as 100 papers with 10 citations each over the same time period. But this can be changed if desired.

# Scatterplot of log(sum.citations) by Project Group Size (Pursuits)
plot(log(sum.citations)~Size,
     xlab = "Group Size", 
     ylab = "log(Sum of citation rates)",
     data = pubs.count.size.gender.career.entropies)

plot(log(sum.citations)~Size,
     xlab = "Group Size", 
     ylab = "log(Sum of citation rates)",
     subset(pubs.count.size.gender.career.entropies, `Project Type` == "Pursuit"))


# Scatterplot of log(sum.citations) by academic diversity measured by entropy (Pursuits)
plot(log(sum.citations)~academic,
     xlab = "Academic Diversity (Entropy)", 
     ylab = "log(Sum of citation rates)",
     data = pubs.count.size.gender.career.entropies)

plot(log(sum.citations)~academic,
     xlab = "Academic Diversity (Entropy)", 
     ylab = "log(Sum of citation rates)",
     subset(pubs.count.size.gender.career.entropies, `Project Type` == "Pursuit"))




# Univariate Regressions looking at raw associations of sum citation rates with various covariates (among Pursuits)
# Observations 4 and 67 are outliers
summary(lm(sum.citations ~ female, data = subset(pubs.count.size.gender.career.entropies, `Project Type` == "Pursuit")[-c(4,67),]))
summary(lm(sum.citations ~ early.career, data = subset(pubs.count.size.gender.career.entropies, `Project Type` == "Pursuit")[-c(4,67),]))
summary(lm(sum.citations ~ mid.career, data = subset(pubs.count.size.gender.career.entropies, `Project Type` == "Pursuit")[-c(4,67),]))
summary(lm(sum.citations ~ postdoc, data = subset(pubs.count.size.gender.career.entropies, `Project Type` == "Pursuit")[-c(4,67),]))
summary(lm(sum.citations ~ senior.career, data = subset(pubs.count.size.gender.career.entropies, `Project Type` == "Pursuit")[-c(4,67),]))
summary(lm(sum.citations ~ student, data = subset(pubs.count.size.gender.career.entropies, `Project Type` == "Pursuit")[-c(4,67),]))
summary(lm(sum.citations ~ academic, data = subset(pubs.count.size.gender.career.entropies, `Project Type` == "Pursuit")[-c(4,67),]))
summary(lm(sum.citations ~ nsf, data = subset(pubs.count.size.gender.career.entropies, `Project Type` == "Pursuit")[-c(4,67),]))
summary(lm(sum.citations ~ Size, data = subset(pubs.count.size.gender.career.entropies, `Project Type` == "Pursuit")[-c(4,67),]))
# Only female and Size have significant univariate associations

# Using a multivariate model without years since the years data is already in the outcome (citation rates)
summary(lm(sum.citations ~ Size + academic + postdoc + advanced.career + early.career + female, data = subset(pubs.count.size.gender.career.entropies, `Project Type` == "Pursuit")[-c(4,67),]))
# Group Size has a significant positive association

# Univariate Regressions looking at raw associations of citation measures with various covariates (among Pursuits)
# Observation 67 is an outlier
summary(lm(mean.citation.rate ~ female, data = subset(pubs.count.size.gender.career.entropies, `Project Type` == "Pursuit")[-c(67),]))
summary(lm(mean.citation.rate ~ early.career, data = subset(pubs.count.size.gender.career.entropies, `Project Type` == "Pursuit")[-c(67),]))
summary(lm(mean.citation.rate ~ mid.career, data = subset(pubs.count.size.gender.career.entropies, `Project Type` == "Pursuit")[-c(67),]))
summary(lm(mean.citation.rate ~ postdoc, data = subset(pubs.count.size.gender.career.entropies, `Project Type` == "Pursuit")[-c(67),]))
summary(lm(mean.citation.rate ~ senior.career, data = subset(pubs.count.size.gender.career.entropies, `Project Type` == "Pursuit")[-c(67),]))
summary(lm(mean.citation.rate ~ student, data = subset(pubs.count.size.gender.career.entropies, `Project Type` == "Pursuit")[-c(67),]))
summary(lm(mean.citation.rate ~ academic, data = subset(pubs.count.size.gender.career.entropies, `Project Type` == "Pursuit")[-c(67),]))
summary(lm(mean.citation.rate ~ nsf, data = subset(pubs.count.size.gender.career.entropies, `Project Type` == "Pursuit")[-c(67),]))
summary(lm(mean.citation.rate ~ Size, data = subset(pubs.count.size.gender.career.entropies, `Project Type` == "Pursuit")[-c(67),]))
# Only Group Size has a significant (positive) univariate association

# Using a multivariate model without years since the years data is already in the outcome (citation rates)
summary(lm(mean.citation.rate ~ Size + academic + advanced.career + postdoc + early.career + female, data = subset(pubs.count.size.gender.career.entropies, `Project Type` == "Pursuit")[-c(67),]))
# Size has a significant positive association and academic diversity has a negative association

# Visualizing the association of mean citation rate with academic diversity (entropy)
plot(mean.citation.rate ~ academic, data = subset(pubs.count.size.gender.career.entropies, `Project Type` == "Pursuit")[-c(67),])


# Time to publish analysis - the outcome for each Project is the # of years until the first publish

# Making a list with the years of each publication associated with the Projects
publication_year_list <- vector("list", length(pubs.count.size.gender.career.entropies$Projects))
for(i in 1:length(pubs.count.size.gender.career.entropies$Projects)){
  current.project <- pubs.count.size.gender.career.entropies$Projects[i]
  for(j in 1:length(project.codes)){
    if(current.project %in% project.codes[[j]]){
      print(j)
      publication_year_list[[i]] <- c(publication_year_list[[i]], pubs$Publication.Year[j])
    }
  }
}

min.function <- function(x, ...){
  if(any(is.null(x))){return(NA)}
  return(min(x))
}
first.pub.year <- unlist(lapply(publication_year_list, min.function, na.rm = TRUE))
sum(!is.na(first.pub.year)) # 212 projects with first publication years available

pubs.count.size.gender.career.entropies$years.to.pub <- first.pub.year - as.numeric(str_extract(pubs.count.size.gender.career.entropies$Projects, "\\d+"))  

# Cleaning up impossible entries
pubs.count.size.gender.career.entropies$years.to.pub[which(pubs.count.size.gender.career.entropies$years.to.pub < 0)] <- NA

# Negative Binomial Regression Model for Years until First Publication (Pursuits)
nb.model.pubtime <- glm.nb(years.to.pub ~ Size + female + early.career + advanced.career + postdoc + nsf + academic, data = subset(pubs.count.size.gender.career.entropies, `Project Type` == "Pursuit"))
  
summary(nb.model.pubtime) 

# Now allowing for non-linear relationships of the variables with the outcome via Generalized Additive Models
gam.model.2 <- gam(years.to.pub ~ s(Size) +s(female) + s(early.career) + s(advanced.career) + s(postdoc) + s(academic) + s(nsf) + offset(log(years)), family = quasipoisson, data = subset(pubs.count.size.gender.career.entropies, `Project Type` == "Pursuit"), method = "REML")

summary(gam.model.2)
plot(gam.model.2)

#### Extra plots #####
# Creating a more streamlined "Project Type" Variable
sesync$ptype2 <- sesync$`Project Type`
{
  sesync$ptype2[which(sesync$ptype2 %in% c("Foundation", "Pursuit", "Venture"))] <- "Team"
  sesync$ptype2[which(sesync$ptype2 %in% c("Postdoc Immersion", "Postdoctoral Fellowship"))] <- "Postdoctoral"
  
  sesync$ptype2[which(sesync$ptype2 %in% c("Graduate Student Pursuit"))] <- "Graduate Students"
  sesync$ptype2[which(sesync$ptype2 %in% c("Workshop", "Short Courses"))] <- "Other"
  
}

unique.sesync$ptype2 <- unique.sesync$`Project Type`
{
  unique.sesync$ptype2[which(unique.sesync$ptype2 %in% c("Foundation", "Pursuit", "Venture"))] <- "Team"
  unique.sesync$ptype2[which(unique.sesync$ptype2 %in% c("Postdoc Immersion", "Postdoctoral Fellowship"))] <- "Postdoctoral"
  
  unique.sesync$ptype2[which(unique.sesync$ptype2 %in% c("Graduate Student Pursuit"))] <- "Graduate Students"
  unique.sesync$ptype2[which(unique.sesync$ptype2 %in% c("Workshop", "Short Courses"))] <- "Other"
  
}

table(unique.sesync$ptype2, unique.sesync$Gender)

# Create data frame from above table
nsf_data <- data.frame(
  Gender = c("Female", "Male"),
  `Graduate Students` = c(86, 60),
  `Other` = c(924, 817),
  `Postdoctoral` = c(40, 30),
  `Team` = c(914, 1186)
)

# Convert from wide to long format
nsf_long <- nsf_data %>%
  pivot_longer(-Gender, names_to = "Category", values_to = "Count") %>%
  group_by(Category) %>%
  mutate(Percent = Count / sum(Count))  # Compute percentage within each category

# Create pie charts for each category of the new Project Type Variable
ggplot(nsf_long, aes(x = "", y = Percent, fill = Gender)) +
  geom_bar(stat = "identity", width = 1, color = "black") +
  coord_polar("y", start = 0) +
  facet_wrap(~Category, ncol = 4) +  # Arrange pie charts in a grid
  geom_text(aes(label = scales::percent(Percent, accuracy = 0.1)), 
            position = position_stack(vjust = 0.5), color = "white", size = 5, fontface = "bold") +
  theme_void() +  
  theme(legend.position = "bottom")  # Moves legend below the charts


