'Author: Christopher Sampah'
'Date: 03/03/2020'

folder_path <- paste0(getwd(),'/')
library(reshape2)
library(caret)
library(randomForest)
library(party)
library(dplyr)

'Step 0: Read in the file'
current.location <- dirname(rstudioapi::getSourceEditorContext()$path)
setwd(current.location)
war1 <- read.csv("intrastate.csv")

'Step 1: Impute for unusable values'
'Change negative month values to month 6, close to the average of all months (6.5)'
war1$start_month1[war1$start_month1==(-9) |war1$start_month1==(-8) ] <- 6
war1$end_month1[war1$end_month1==(-8)| war1$end_month1==(-9)] <- 6


'Step 1.2: Drop unused time fields'
'time will be calculated exclusively in months s.t. days will be removed'
'pauses in conflict will be ignored for now, time from official start to final conclusion only'

drops <- c("start_day1","start_year2", "end_year2", "start_month2",
           "start_day2","end_day1", "end_month2","end_day2","previous_war")
war1 <- war1[ , !(names(war1) %in% drops)]

'Date:Create a new field for the full date'
war1$start_date <- as.Date(paste(war1$start_month1,"1", war1$start_year1, sep="/"), "%m/%d/%Y")
war1$end_date <- as.Date(paste(war1$end_month1,"1", war1$end_year1, sep="/"),"%m/%d/%Y")

'Combat fatalities: drop rows with missing battle deaths'
war1 <-war1[!(war1$side1_fatalities==(-9)),]
war1 <-war1[!(war1$side2_fatalities==(-9)),]

'change blank fatality counts to zero'
war1$side1_fatalities[war1$side1_fatalities ==(-8)] <- 0
war1$side2_fatalities[war1$side2_fatalities ==(-8)] <- 0

war.key <- data.frame( "War"= 4:7 , "Type" = c("Central Control", "Local Issues", "Regional Internal", 
                                               "Intercommunal"))

'Step 2: Change war outcome to binary'
war1$outcome[war1$outcome ==2|war1$outcome ==3 ] <- 1
war1$outcome[war1$outcome ==4 |war1$outcome ==5 |war1$outcome ==6 | war1$outcome ==7 ] <- 0

outcome.key <- data.frame( "first_outcome" = 1:7, "new_outcome" = c(rep(1,3), rep(0,4)),
                           "Type" = c("Win-Loss", "Win-Loss", "Compromise", "Another War",
                                      "Ongoing Conflict", "Stalemate", "Grudging Peace"))
#outcome.key

'Step 3: Create and insert new fields: fatalities and duration of conflict(years) with revised date fields'
war1$fatalities <- war1$side1_fatalities+war1$side2_fatalities
war1$days <- as.numeric(difftime(war1$end_date,war1$start_date, units = "days"))
war1$duration <- as.numeric(war1$days/365)
war1 <-war1[!(is.na(war1$duration)),] # blank values for war duration
war1 <-war1[!(war1$duration == 0),]
#the dataset currently has duplicate wars, e.g. Greek Independence


'Step 3.5: Start creating dataset for the model'
cols <- c("outcome","war_id","international_war","start_year1","duration","combat_location", "fatalities","war_type")
war1 <- war1[ , which(names(war1) %in% cols)] #subset the original dataset only picking those values
war1 <- war1[,cols] # re-order columns according to the list above
#cols2 <- c("war_id", "outcome", "international_war")
#war2 <- war1[ , !(names(war1) %in% cols2)]
cols2 <- c("outcome", "international_war")
war2 <- war1[ , !(names(war1) %in% cols2)]
war2 <- aggregate(fatalities ~ war_id  + combat_location + war_type + duration, data = war2, sum)
war.summary <- as.data.frame(table(war2$war_id))
war2
#war.summary[order(war.summary$Freq),]



'I STOPPED HERE!!!'


'Step 3.9: Scatter-plot Matrix'
pairs(war2)

'change duration back to days for modelling'
war1$duration <- as.numeric(war1$duration*365)
