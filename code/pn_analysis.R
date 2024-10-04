lapply(c("data.table", "rstudioapi", "scales", "zoo"), require, character.only = T)
setwd(dirname(getActiveDocumentContext()$path))
setwd("../")

# Download data and load
source("code/download_crs.R")
download_au_crs()
load("large_input/crs_2002_2022.RData")

# Remove without PN and subset year
crs = subset(crs, CrsID!="" & !is.na(CrsID) & Year >= 2012)

# Calculate unique donor/cid code
crs$dccid = paste(crs$DonorCode, crs$CrsID, sep="|")

# Aggregate to donor, cid, year
cid_agg = crs[,.(
  USD_Disbursement = sum(USD_Disbursement, na.rm=T),
  USD_Commitment = sum(USD_Commitment, na.rm=T)
), by=.(dccid, Year)]

# Find projects with both commitments and disbursements
cid_agg_agg = cid_agg[,.(
  USD_Disbursement_Total = sum(USD_Disbursement, na.rm=T),
  USD_Commitment_Total = sum(USD_Commitment, na.rm=T)
), by=.(dccid)]
cid_agg_agg = subset(cid_agg_agg, USD_Disbursement_Total > 0 & USD_Commitment_Total > 0)

# Percent where commit and disburse are equal
mean(cid_agg_agg$USD_Disbursement_Total == cid_agg_agg$USD_Commitment_Total)

# Percent where commit is greater than disburse
mean(cid_agg_agg$USD_Disbursement_Total < cid_agg_agg$USD_Commitment_Total)

# Percent where commit is less than disburse
mean(cid_agg_agg$USD_Disbursement_Total > cid_agg_agg$USD_Commitment_Total)

# Remove incomplete projects (commitments not yet fulfilled)
# cid_agg_agg = subset(cid_agg_agg, USD_Disbursement_Total >= USD_Commitment_Total)

# Set total commit to total disburse when it's less
cid_agg_agg$USD_Commitment_Total[which(
  cid_agg_agg$USD_Disbursement_Total > cid_agg_agg$USD_Commitment_Total
)] = cid_agg_agg$USD_Disbursement_Total[which(
  cid_agg_agg$USD_Disbursement_Total > cid_agg_agg$USD_Commitment_Total
)]

cid_agg_agg[,c("USD_Disbursement_Total")] = NULL

cid_agg = merge(cid_agg, cid_agg_agg, by=c("dccid"))
cid_agg[,c("USD_Commitment")] = NULL

# Calculate years in terms of time since first transaction
cid_agg[,"normalized_year":=(Year - min(Year)) + 1, by=.(dccid)]
# Grid out to all years
all_years = expand.grid(
  dccid=unique(cid_agg$dccid),
  normalized_year=unique(cid_agg$normalized_year)
)
cid_agg = merge(cid_agg, all_years, by=c("dccid", "normalized_year"), all=T)
cid_agg = cid_agg[order(cid_agg$dccid, cid_agg$normalized_year),]

# Normalize (Set disbursements to % of total commitments)
cid_agg[,"USD_Commitment_Total":=na.locf(USD_Commitment_Total), by=.(dccid)]
cid_agg$USD_Disbursement[which(is.na(cid_agg$USD_Disbursement))] = 0
cid_agg[,"cumulative_USD_Disbursement":=cumsum(USD_Disbursement), by=.(dccid)]
cid_agg$cumulative_USD_Disbursement_Percentage = cid_agg$cumulative_USD_Disbursement / cid_agg$USD_Commitment_Total

# Calculate mean percentage completed past each year
pg_mean_pct = cid_agg[,.(
  mean_cumulative_percentage_disbursed=mean(cumulative_USD_Disbursement_Percentage)
), by=.(normalized_year)]
fwrite(pg_mean_pct, "output/pn_cum_disb.csv")

cid_max_years = cid_agg[,.(Year=max(Year, na.rm=T)), by=.(dccid)]
cid_agg_max = merge(cid_agg, cid_max_years, by=c("dccid", "Year"))
cid_agg_max[,c("USD_Disbursement")] = NULL
# Set same year to 0
cid_agg_max$normalized_year = cid_agg_max$normalized_year - 1
setnames(
  cid_agg_max,
  c("cumulative_USD_Disbursement", "cumulative_USD_Disbursement_Percentage", "normalized_year"),
  c("USD_Disbursement_Total", "percent_disbursed", "lag")
)

weighted.mean(cid_agg_max$lag, cid_agg_max$USD_Disbursement_Total)
weighted.mean(cid_agg_max$percent_disbursed, cid_agg_max$USD_Disbursement_Total)
fwrite(cid_agg_max, "output/pn_metadata.csv")
