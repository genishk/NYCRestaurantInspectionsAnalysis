library(tidyverse)
library(ggalluvial)

rm(list = ls())

# load the cleaned dataset
df <- readRDS(file='data/cleaned_DOHMH_New_York_City_Restaurant_Inspection_Results.Rda')

# shorten the column names so they don't overlap in the plot
# names(df) <- abbreviate(names(df), minlength=5)

# df <- na.omit(df)
df$grade <- fct_relevel(df$grade, "A", "B", "C", "G", "N", "P", "Z")
df$score <- as.numeric(df$score)
df <- df[row.names(unique(df[c('camis','insp.date','score','grade')])),]
df$score <- ifelse(df$action == 'No violations were recorded at the time of this inspection.', 0, df$score)
df <- df %>% drop_na(score)

df <- subset(df,!(boro %in% c('0','210')))
df <- subset(df,!(flag %in% c('Not Applicable')))



###########################################



#Filter on inspection type, score, grade
Inspections <- df %>%
  select(camis,insp.date)

#Select distinct inspections
Inspections_Distinct <- distinct(Inspections)

#Select most recent inspection date
MostRecentInsp <- Inspections_Distinct %>%
  group_by(camis) %>%
  slice(which.max(as.Date(insp.date,'%m/%d/%Y')))

#Join most recent inspection with original dataset
inner_join(df,MostRecentInsp, by = "camis","insp.date")

#Select restaurant inspection data based on most recent inspection date
Final <- df %>% inner_join(MostRecentInsp)

#Select distinct restaurant inspection data
df_recent <- distinct(Final)


#############################################
