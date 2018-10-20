library(tidyverse)
library(ggmap)
library(lubridate)
library(stringr)
library(RCurl)
library(viridis)
library(dplyr)
library(rgdal)
library(sp)

# How can I offer the option to download from the Github Repo? 
#test = 
#  read.csv(text=getURL("https://github.com/ThomasPepperz/CrimeAnalysisinR/blob/master/Incidents.csv", header = TRUE))
#  test2 = download.file("https://github.com/ThomasPepperz/CrimeAnalysisinR/blob/master/Incidents.csv", test2)
#  test2 = "~/Downloads/name1.csv"
# SHOULD I APPEND MILEPOINT NUMBER TO ROADWAY


Offenses =
  read.csv("~/DataScience/DataSets/PaducahCrimes/Offenses.csv")

Incidents =
  read.csv("~/DataScience/DataSets/PaducahCrimes/Incidents.csv")

Locations =
  read.csv("~/DataScience/DataSets/PaducahCrimes/Locations.csv")

# List the variable names for each data frame
colnames(Offenses)
colnames(Incidents)
colnames(Locations)
Incidents
# Subset relevant variables for each data frame
Offenses = Offenses[, c(
  "incidentid",
  "OffenseID",
  "ViolationCde",
  "ViolationDesc"
)]

# Should I add "Time Received?" it differes from dispatched. Is this Report Time?
# Should I include LastModifiedDte? EntryDte, LocalCodeCaseNumber. 
# Point out IncidentTime can be derived from IncidentDte Incident Time and Incident Date contains time and end time
# DO I need ENTRYDT, SUPPLEMENTDTE, REVIEWDTE?, LASTMODIFIEDDTE
# ASK ZIDAR ABOUT SUPPLEMENT REVIEW LAST MODIFIED AND WHAT IT MEANS


#SHOULD I ADD TIME RECEIVED?/
Incidents = Incidents[, c(
  "incidentid",
  "IncidentDte",
  "IncidentEndDte",
  "TimeDispatched",
  "TimeArrived",
  "TimeCleared",
  "HowReportedDesc",
  "ReportedDte",
  "CreatedByBadgeNumber"
)]

Locations = Locations[, c(
  "incidentid",
  "LocationID",
  "LocStreetNumber",
  "LocStreetDir",
  "LocStreetName",
  "LocStreetSfx",
  "LocAptNumber",
  "LocalRdwyNumber",
  "MilePointNumber",
  "LocZip",
  "Sector",
  "InCityLimits",
  "LatDecimal",
  "LongDecimal"
)]

Locations
# Rename variables for each data frame
names(Offenses) =  c(
  "IncidentID",
  "OffenseID",
  "ViolationCode",
  "Offense"
)

names(Incidents) = c(
  "IncidentID",
  "IncidentDate",
  "EndDate",
  "DispatchTime",
  "ArrivalTime",
  "ClearTime",
  "ReportMode",
  "ReportDate",
  "BadgeNumber"
)

names(Locations) = c(
  "IncidentID",
  "LocationID",
  "StreetNumber",
  "StreetDirection",
  "StreetName",
  "StreetSuffix",
  "AptNumber",
  "RoadwayNumber",
  "MilePoint",
  "ZIPCode",
  "Sector",
  "InCityLimits",
  "LatDecimal",
  "LongDecimal"
)
# Combine Address Columns
# Here, I wonder if I ought to do something about those addresses without a street name
# Separate out the NA street name, remove the direction column for 1 DF, then combine addresses for both
# # then merge the data frames

# Filter records with a street number and store in "stNumbers" 
stNumbers = filter(Crimes, is.na(StreetNumber) == FALSE)
# 3,914 records with a street number
nrow(stNumbers)
# Filter records without a street number and store in "noStNumbers" 
noStNumbers = filter(Crimes, is.na(StreetNumber) == TRUE)
# 1,012 records without a street number (significance?)
nrow(noStNumbers)
# Paste address components together and store as a new variable `Address` (INCLUDE `StreetNumber`)
stNumbers$Address =
  paste(
    stNumbers$StreetNumber,
    stNumbers$StreetDirection,
    stNumbers$StreetName,
    stNumbers$StreetSuffix,
    stNumbers$AptNumber,
    sep = " ",
    collapse = NULL
  )

# Paste address components together and store as a new variable `Address` (EXCLUDE `StreetNumber`)
noStNumbers$Address =
  paste(
    noStNumbers$StreetDirection,
    noStNumbers$StreetName,
    noStNumbers$StreetSuffix,
    noStNumbers$AptNumber,
    sep = " ",
    collapse = NULL
  )

# Reassemble data frames "stNumber" and "noStNumber" into "Crimes"
Crimes = rbind(noStNumbers, stNumbers)
# Ensure record count is same as original # how come some records have no address info at all no street name?
nrow(Crimes)
# Assign unnecessary, address-related variable names to "removedVars"
removedVars = 
  c(
    "StreetDirection", 
    "StreetName", 
    "AptNumber", 
    "StreetSuffix", 
    "StreetNumber"
    )
# Remove unnecessary, address-related columns
Crimes = Crimes[, !(names(Crimes) %in% removedVars)]
# Trim blank spaces at the end of Address
Crimes$Address = gsub("[[:space:]]+$", "", Crimes$Address)
# Trim blank spaces at the beginning of Address
Crimes$Address = gsub("^[[:space:]]+", "", Crimes$Address)
# Reduce double-spaces to a single space character
Crimes$Address = gsub("\\s+"," ", Crimes$Address)
# Examine addresses of the last and first five records of the data frame
head(Crimes$Address); tail(Crimes$Address)

# Reorder data frame columns (SHOULD I APPEND MILEPOINT TO ROADWAY?)
# Derived Columns computing difference between dispatch time and arrival time

Crimes = 
  Crimes[c(
    "IncidentID", 
    "OffenseID", 
    "ViolationCode",
    "Offense",
    "IncidentDate",
    "EndDate",
    "ReportDate",
    "DispatchTime", 
    "ArrivalTime", 
    "ClearTime",
    "ReportMode",
    "BadgeNumber",
    "LocationID",
    "Address", 
    "RoadwayNumber",
    "MilePoint",
    "ZIPCode", 
    "Sector",
    "InCityLimits",
    "LatDecimal", 
    "LongDecimal"
    )]

# (what should the lat decimal be num?) what about address?

# Inspect variable type and formats
str(Crimes)
# Assign variables to be factored to data frame "colsToFactor"
colsToFactor = c(
  "ViolationCode", 
  "BadgeNumber", 
  "RoadwayNumber", 
  "ZIPCode", 
  "Sector", 
  "InCityLimits", 
  "Address"
  )
# Factor and assign variables
Crimes[colsToFactor] = lapply(Crimes[colsToFactor], factor)
# Check for successful coercion of variables into factors
str(Crimes)

# Reformat report date, do times match from dates? delete unnecessary columns
# Why do some not have end dates? What time zone are EndDate recorded in? It says UTC, do I need to change?

# Print the structure and variable type of `EndDate`
str(Crimes$EndDate)
# Use lubridate package to reformat `EndDate``
Crimes$EndDate = mdy_hm(as.character(Crimes$EndDate))
# Inspect structure and verify `EndDate` is of variable type POSIXct
str(Crimes$EndDate)

Crimes


# Write Master Data Set "Crimes" to a csv file (Send copy to Zidar once finished)
write.csv(Crimes, "~/DataScience/R/CrimeAnalysis/PaducahCrimeAnalysis/Crimes.csv")



# End of Data Wrangling
###############################################
# Begin Statistical Analysis

psych::describe(Crimes)

# Examine Data Set (can I dummy up factors to allow this or do I need to subset?)
cor(Crimes)

# Description of Crime (How can I cluster these and simplify into categories?)
str(Crimes$Offense)
# Frequencies by Crime Violation, Change "Description" to "Offense" and add a "OffenseType"
summary(Crimes$Offense)
# Show all unique levels of a factor
unique(Crimes$Offense)

# Which crimes appear greater than 100 times? Without crime-clustering.
plot(table(Crimes$Offense))
a = table(Crimes$Offense)
a > 100
subset(a, a > 100)
subset(a, a < 100 & a > 50)
table(Crimes$ViolationDesc)

#Begin carving up the data frame and also begin categorizing these as "OffenseType"
# Shoplifting
shoplifting = filter(Crimes, Description == "TBUT OR DISP SHOPLIFTING U/$500")

# pot
pot = filter(Crimes, Description == "POSS OF MARIJUANA")

a = filter(Crimes, HowReportedDesc == "BY INVESTIGATION")
View(a)

summary(a)





################# HEAT MAP ANALYSIS #######################

# Scatter plot
ggplot() +
  geom_point(data = Crimes,
             aes(x = LatDecimal, y = LongDecimal),
             alpha = .05)

# Basic heat map
ggplot() +
  stat_density2d(
    data = Crimes,
    aes(x = LatDecimal, y = LatDecimal, fill = ..density..),
    geom = 'tile',
    contour = F
  )

## Fetch Google Maps of Paducah, KY (Can you do zoom = X.5 so halves?) 

#
quadStateMap = get_map('Paducah', zoom = 8)
#
Paducah1 = get_map('Paducah', zoom = 12)
#
Paducah2 = get_map('Paducah', zoom = 13)
# How can you shift a map over with the same zoom?
Paducah3 = get_map('Paducah', zoom = 14) 

# Plot basic Paducah map
ggmap(Paducah3)

# MAP WITH HEATMAP OVERLAY
ggmap(Paducah3) +
  stat_density2d(
    data = Crimes,
    aes(x = LongDecimal, y = LatDecimal),
    geom = 'tile',
    contour = F,
    alpha = .5
  )

# SIMPLE HEATMAP WITH VIRIDIS COLORING
ggplot() +
  stat_density2d(
    data = Crimes,
    aes(x = LongDecimal, y = LatDecimal, fill = ..density..),
    geom = 'tile',
    contour = F,
    alpha = .5
  ) +
  scale_fill_viridis()


# VIRIDIS (inferno), alpha = .5
ggmap(Paducah3) +
  stat_density2d(
    data = Crimes,
    aes(x = LongDecimal, y = LatDecimal, fill = ..density..),
    geom = 'tile',
    contour = F,
    alpha = .5
  ) +
  scale_fill_viridis(option = 'inferno')

# CREATE "POLISHED" Crimes HEATMAP IN R

ggmap(Paducah3) +
  stat_density2d(
    data = pot,
    aes(x = LongDecimal, y = LatDecimal, fill = ..density..),
    geom = 'tile',
    contour = F,
    alpha = .7
  ) +
  scale_fill_viridis(option = 'inferno') +
  labs(
    title = str_c(
      'Marijuana Possession\n'
      ,
      ' '
    )
    ,
    subtitle = ' '
    ,
    fill = str_c('Intensity of offense frequency')
  ) +
  theme(
    text = element_text(color = "#444444")
    ,
    plot.title = element_text(size = 22, face = 'bold')
    ,
    plot.subtitle = element_text(size = 12)
    ,
    axis.text = element_blank()
    ,
    axis.title = element_blank()
    ,
    axis.ticks = element_blank()
  ) +
  guides(fill = guide_legend(override.aes = list(alpha = 1)))

# Is the legend numbers automated or something? 


####################### Hot spot Analysis if Different #################





############# NOTES ################

# Who had the most arrests? Percentage of arrests by badge number?
# change columns to appropriate variable types
# DO I want to cretae some derived tables? For intance, do some math on how long it took officers to resond and such?
#  some basic tables psych::describe, but I need to do something 
# about the 267 factor levels for crime type. Shoudl I do a cluster or should I cluster them myself? 

# Why do some EndDate have no entry?

# IncidentID is this auto-incremented, state-wide, national, county? seems as though other numbers have been outside 
# Do I need to use decimal or should i use degrees and minutes? is there an advantage of one over the other? should I keep both?
# Can I use the street direction occasionally to figure out if a direction is prevalent in certain areas/streets for crime. leaving a bar or maybe drug dealer
# What is the nature of Location ID, Location Type? Should I keep violation code?
# How should I deal with the crime descriptions? Can I simplify it somehow for purposes?
# What if we removed all of the Walmart-related crimes?
# Create several statistics describing relationships between times dispatched and so and so.
# Calculate gaps in dispatch time arrival time. which crimes more likely to be reported by phone versus in person in view by investigation,
# 298 Crimes types. anyway I can combine those or prune them down?
# Can I subset data for each of those hot spots?
# Heatmap versus Hotspot map, both show higher density/cluster, both use color gradient to represent higher densities
# Hot spot analysis uses statistical analysis in order to define areas of high occurrence versus areas of low occurrence.  
# Since hot spot areas are statistically significant, the end visualization is less subjective.  
# The designation of an area as being a hot spot is therefore expressed in terms of statistical confidence.
# Can I remove degree/mins/secs and just stick with the Long decimal notation?
# What is the probabiliy that a traffic violation will turn into a drug investigation
# Given presence of marijuana possession, what is probability of paraphenelia?
# For those cases where paraphenelia wasn't pressed, is there a uniue badge number?
# Note the data entry mistake on address for the first record, re: "PALM ST 2-Jan"
# I should use violation code late to filter and group crimes together
# What does MilepointNumber refer to? Should I have included that with roadway? if no st name add the milepoint?
# What about non roadway-only records with no st. name? should i paste the mile point anyway?



#### https://www.r-bloggers.com/san-francisco-crime-data-analysis-part-1/
library(data.table)
library(ggplot2)
library(lubridate)
library(plotly)

trn = Crimes

trn[, .N, by = Crimes$LocStreetName][order(N, decreasing = T)][1:10]

d = Crimes[, .N, by = Category][order(N, decreasing = T)]
ggplot(d, aes(x = reorder(Crimes$IncidentDescription, -N), y = N, fill = N)) + 
  geom_bar(stat = 'identity') + 
  coord_flip() + 
  scale_fill_continuous(guide=FALSE) + 
  labs(x = '', y = 'Total Number of Crimes', title = 'Total Count of Crimes per Category') 


