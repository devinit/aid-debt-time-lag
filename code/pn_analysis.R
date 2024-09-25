lapply(c("data.table", "rstudioapi", "scales"), require, character.only = T)
setwd(dirname(getActiveDocumentContext()$path))
setwd("../")

# From ratio_analysis.R
load("large_input/crs.RData")

# Remove without PN and subset year
crs = subset(crs, CrsID!="" & !is.na(CrsID) & Year >= 2013)

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

# Normalize (Set disbursements to % of total commitments)
cid_agg$USD_Disbursement = cid_agg$USD_Disbursement / cid_agg$USD_Commitment_Total
cid_agg[,c("USD_Commitment_Total", "USD_Commitment")] = NULL
# Calculate years in terms of time since first transaction
cid_agg[,"years":=(Year - min(Year)) + 1, by=.(dccid)]
cid_agg[,c("Year")] = NULL
# Grid out to all years
all_years = expand.grid(
  dccid=unique(cid_agg$dccid),
  years=unique(cid_agg$years)
)
cid_agg = merge(cid_agg, all_years, by=c("dccid", "years"), all=T)
cid_agg$USD_Disbursement[which(is.na(cid_agg$USD_Disbursement))] = 0
cid_agg = cid_agg[order(cid_agg$dccid, cid_agg$years),]
cid_agg[,"cum_USD_Disbursement":=cumsum(USD_Disbursement), by=.(dccid)]

# Calculate mean percentage completed past each year
pg_mean_pct = cid_agg[,.(
  mean_cumulative_percentage_disbursed=mean(cum_USD_Disbursement)
), by=.(years)]
fwrite(pg_mean_pct, "output/project_number_cumulative_disb_by_year.csv")

# Calculate mean years to complete
completed = subset(cid_agg, cum_USD_Disbursement == 1)
completed[,"min_years":=min(years), by=.(dccid)]
completed = subset(completed, years == min_years)
completed_table = data.table(table(completed$years))
setnames(completed_table, "V1", "years_to_fulfill_commitment")
completed_table$pct = completed_table$N / sum(completed_table$N)
completed_table$cum_pct = cumsum(completed_table$pct)
fwrite(completed_table, "output/completed_project_number_counts.csv")
completed_table$cum_pct_label = label_percent(accuracy=0.1)(completed_table$cum_pct)
mean(completed$years)
