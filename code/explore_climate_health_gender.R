# Setup ####
list.of.packages <- c("data.table", "rstudioapi", "ggplot2", "Hmisc", "tidyverse", "stringr", "scales")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only=T)

wd <- dirname(getActiveDocumentContext()$path) 
setwd(wd)
setwd("../")

crs = fread("~/git/gender-nlp-exploration/large_data/crs_for_gender_climate_disability_predictions.csv")

# Set blanks to false and 0
blanks = c("", "-")
blank_indices = which(crs$project_title %in% blanks & crs$short_description %in% blanks & crs$long_description %in% blanks)
crs$`Gender equality - significant objective confidence`[blank_indices] = 0
crs$`Gender equality - significant objective predicted`[blank_indices] = F
crs$`Gender equality - principal objective confidence`[blank_indices] = 0
crs$`Gender equality - principal objective predicted`[blank_indices] = F

crs$`Climate adaptation - significant objective confidence`[blank_indices] = 0
crs$`Climate adaptation - significant objective predicted`[blank_indices] = F
crs$`Climate adaptation - principal objective confidence`[blank_indices] = 0
crs$`Climate adaptation - principal objective predicted`[blank_indices] = F

crs$`Climate mitigation - significant objective confidence`[blank_indices] = 0
crs$`Climate mitigation - significant objective predicted`[blank_indices] = F
crs$`Climate mitigation - principal objective confidence`[blank_indices] = 0
crs$`Climate mitigation - principal objective predicted`[blank_indices] = F

crs$`Principal gender equality` = F
crs$`Principal gender equality`[which(crs$gender==2)] = T
crs$`Principal gender equality`[which(
  crs$`Gender equality - principal objective predicted` &
    crs$`Gender keyword match`
)] = T

crs$`Principal climate adaptation` = F
crs$`Principal climate adaptation`[which(crs$climate_adaptation==2)] = T
crs$`Principal climate adaptation`[which(
  crs$`Climate adaptation - principal objective predicted` &
    crs$`Climate keyword match`
)] = T

crs$`Significant climate adaptation` = F
crs$`Significant climate adaptation`[which(crs$climate_adaptation==1)] = T
crs$`Significant climate adaptation`[which(
  crs$`Climate adaptation - significant objective predicted` &
    crs$`Climate keyword match`
)] = T

crs$`Principal climate mitigation` = F
crs$`Principal climate mitigation`[which(crs$climate_mitigation==2)] = T
crs$`Principal climate mitigation`[which(
  crs$`Climate mitigation - principal objective predicted` &
    crs$`Climate keyword match`
)] = T

crs$`Significant climate mitigation` = F
crs$`Significant climate mitigation`[which(crs$climate_mitigation==1)] = T
crs$`Significant climate mitigation`[which(
  crs$`Climate mitigation - significant objective predicted` &
    crs$`Climate keyword match`
)] = T

crs$health = crs$sector_code %in% c(
  120, # Health
  121, # Health, general
  122, # Basic health
  123, # NCDs
  130 # Population & Reproductive health
)

describe(crs$health)
describe(crs$sector_code)
describe(crs$`Principal gender equality`)
describe(crs$`Principal climate adaptation`)
describe(crs$`Principal climate mitigation`)

adaptation_sector_table = data.table(table(subset(crs, `Principal climate adaptation`)$sector_name))
adaptation_sector_table = adaptation_sector_table[order(-adaptation_sector_table$N),]

mitigation_sector_table = data.table(table(subset(crs, `Principal climate mitigation`)$sector_name))
mitigation_sector_table = mitigation_sector_table[order(-mitigation_sector_table$N),]

health_sector_table = data.table(table(subset(crs, health)$sector_name))
health_sector_table = health_sector_table[order(-health_sector_table$N),]

gender_sector_table = data.table(table(subset(crs, `Principal gender equality`)$sector_name))
gender_sector_table = gender_sector_table[order(-gender_sector_table$N),]

crs$climate_health = (
  crs$health & (crs$`Principal climate adaptation` | crs$`Principal climate mitigation`)
)

climate_health = subset(crs, climate_health)
climate_health_sums = climate_health[,.(sum=sum(usd_disbursement_deflated, na.rm=T)), by=.(sector_name)]
climate_health_sums = climate_health_sums[order(-climate_health_sums$sum),]
climate_purpose_health_sums = climate_health[,.(sum=sum(usd_disbursement_deflated, na.rm=T)), by=.(purpose_name)]
climate_purpose_health_sums = climate_purpose_health_sums[order(-climate_purpose_health_sums$sum),]

climate_health_gender = subset(climate_health, `Principal gender equality`)
climate_purpose_health_gender_sums = climate_health_gender[,.(sum=sum(usd_disbursement_deflated, na.rm=T)), by=.(purpose_name)]
climate_purpose_health_gender_sums = climate_purpose_health_gender_sums[order(-climate_purpose_health_gender_sums$sum),]