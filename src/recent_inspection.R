# Example R script to identify most recent letter grade by restaurant

#Install packages: reader, tidyverse
library(readr)
library(tidyverse)

# load the cleaned dataset
df <- readRDS(file='data/cleaned_DOHMH_New_York_City_Restaurant_Inspection_Results.Rda')

# shorten the column names so they don't overlap in the plot
names(df) <- abbreviate(names(df), minlength=5)

#Filter on inspection type, score, grade
Inspections <- df %>%
  filter((insp.t %in% 
            c('Cycle Inspection / Re-inspection'
              ,'Pre-permit (Operational) / Re-inspection')
          |(insp.t %in%
              c('Cycle Inspection / Initial Inspection'
                ,'Pre-permit (Operational) / Initial Inspection')) 
          & score <= 13)
         |(insp.t %in%  
              c('Pre-permit (Operational) / Reopening Inspection'
                ,'Cycle Inspection / Reopening Inspection'))
         & grade %in% c('A', 'B', 'C', 'P', 'Z')) %>%
         select(camis,insp.d)

#Select distinct inspections
Inspections_Distinct <- distinct(Inspections)

#Select most recent inspection date
MostRecentInsp <- Inspections_Distinct %>%
  group_by(camis) %>%
  slice(which.max(as.Date(insp.d,'%m/%d/%Y')))

#Join most recent inspection with original dataset
inner_join(df,MostRecentInsp, by = "camis","insp.d")

#Select restaurant inspection data based on most recent inspection date
Final <- df %>% inner_join(MostRecentInsp) %>%
  filter((insp.d %in% 
            c('Cycle Inspection / Re-inspection'
              ,'Pre-permit (Operational) / Re-inspection'
              , 'Pre-permit (Operational) / Reopening Inspection' 
              ,'Cycle Inspection / Reopening Inspection')
          |(insp.t %in%
              c('Cycle Inspection / Initial Inspection'
                ,'Pre-permit (Operational) / Initial Inspection')) 
          & score <= 13)) %>%
    select(camis,dba,boro,zip,cuisn,insp.d,grade,insp.t,score)

#Select distinct restaurant inspection data
Final <- distinct(Final)
View(Final)
