# CrimeAnalysisInR
An analysis of 2017 crime data from Paducah Police Department, Paducah, KY

## The Data
A Freedom of Information request to the crime analyst for Paducah Police Department of Paducah, KY asking for "all the data points you have on any crimes in 2017" yielded three data sets: Incidents.csv, Offenses.csv, and Locations.csv.

library(tidyverse)
library(ggmap)
library(lubridate)
library(stringr)
library(RCurl)
library(viridis)
library(dplyr)
library(rgdal)
library(sp)

# Load data
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
# Print the structure and variable type of `EndDate`
str(Crimes$EndDate)
# Use lubridate package to reformat `EndDate``
Crimes$EndDate = mdy_hm(as.character(Crimes$EndDate))
# Inspect structure and verify `EndDate` is of variable type POSIXct
str(Crimes$EndDate)
# Write Master Data Set "Crimes" to a csv file (Send copy to Zidar once finished)
write.csv(Crimes, "~/DataScience/R/CrimeAnalysis/PaducahCrimeAnalysis/Crimes.csv")

# End of Data Wrangling
###############################################
# Begin Statistical Analysis

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

################# HEAT MAP ANALYSIS #######################
# Scatter plot
ggplot() +
  geom_point(data = Crimes,
             aes(x = LatDecimal, y = LongDecimal),
             alpha = .05)
# Fetch Google Maps of Paducah, KY
quadStateMap = get_map('Paducah', zoom = 8)
Paducah1 = get_map('Paducah', zoom = 12)
Paducah2 = get_map('Paducah', zoom = 13)
# How can you shift a map over with the same zoom?
Paducah3 = get_map('Paducah', zoom = 14) 
# Plot basic Paducah map
ggmap(Paducah3)
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
      'Crime\n'
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
