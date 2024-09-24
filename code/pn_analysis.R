lapply(c("data.table", "rstudioapi", "scales"), require, character.only = T)
setwd(dirname(getActiveDocumentContext()$path))
setwd("../")

# From ratio_analysis.R
load("large_input/crs.RData")

# Remove without PN and subset year
crs = subset(crs, ProjectNumber!="" & !is.na(ProjectNumber) & Year >= 2013)

# Calculate unique donor/pn code
crs$dcpn = paste(crs$DonorCode, crs$ProjectNumber, sep="|")

# Aggregate to donor, pn, year
pn_agg = crs[,.(
  USD_Disbursement = sum(USD_Disbursement, na.rm=T),
  USD_Commitment = sum(USD_Commitment, na.rm=T)
), by=.(dcpn, Year)]

# Find projects with both commitments and disbursements
pn_agg_agg = pn_agg[,.(
  USD_Disbursement_Total = sum(USD_Disbursement, na.rm=T),
  USD_Commitment_Total = sum(USD_Commitment, na.rm=T)
), by=.(dcpn)]
pn_agg_agg = subset(pn_agg_agg, USD_Disbursement_Total > 0 & USD_Commitment_Total > 0)

# Percent where commit and disburse are equal
mean(pn_agg_agg$USD_Disbursement_Total == pn_agg_agg$USD_Commitment_Total)

# Percent where commit is greater than disburse
mean(pn_agg_agg$USD_Disbursement_Total < pn_agg_agg$USD_Commitment_Total)

# Percent where commit is less than disburse
mean(pn_agg_agg$USD_Disbursement_Total > pn_agg_agg$USD_Commitment_Total)

# Remove incomplete projects (commitments not yet fulfilled)
pn_agg_agg = subset(pn_agg_agg, USD_Disbursement_Total >= USD_Commitment_Total)

# Set total commit to total disburse when it's less
pn_agg_agg$USD_Commitment_Total[which(
  pn_agg_agg$USD_Disbursement_Total > pn_agg_agg$USD_Commitment_Total
)] = pn_agg_agg$USD_Disbursement_Total[which(
  pn_agg_agg$USD_Disbursement_Total > pn_agg_agg$USD_Commitment_Total
)]

pn_agg_agg[,c("USD_Disbursement_Total")] = NULL

pn_agg = merge(pn_agg, pn_agg_agg, by=c("dcpn"))

# Normalize (Set disbursements to % of total commitments)
pn_agg$USD_Disbursement = pn_agg$USD_Disbursement / pn_agg$USD_Commitment_Total
pn_agg[,c("USD_Commitment_Total", "USD_Commitment")] = NULL
# Calculate years in terms of time since first transaction
pn_agg[,"years":=(Year - min(Year)) + 1, by=.(dcpn)]
pn_agg[,c("Year")] = NULL
# Grid out to all years
all_years = expand.grid(
  dcpn=unique(pn_agg$dcpn),
  years=unique(pn_agg$years)
)
pn_agg = merge(pn_agg, all_years, by=c("dcpn", "years"), all=T)
pn_agg$USD_Disbursement[which(is.na(pn_agg$USD_Disbursement))] = 0
pn_agg = pn_agg[order(pn_agg$dcpn, pn_agg$years),]
pn_agg[,"cum_USD_Disbursement":=cumsum(USD_Disbursement), by=.(dcpn)]

# Calculate mean percentage completed past each year
pg_mean_pct = pn_agg[,.(
  mean_cumulative_percentage_disbursed=mean(cum_USD_Disbursement)
), by=.(years)]
fwrite(pg_mean_pct, "output/project_number_cumulative_disb_by_year.csv")

# Calculate mean years to complete
completed = subset(pn_agg, cum_USD_Disbursement == 1)
completed[,"min_years":=min(years), by=.(dcpn)]
completed = subset(completed, years == min_years)
completed_table = data.table(table(completed$years))
setnames(completed_table, "V1", "years_to_fulfill_commitment")
completed_table$pct = completed_table$N / sum(completed_table$N)
fwrite(completed_table, "output/completed_project_number_counts.csv")
completed_table$pct_label = label_percent(accuracy=0.01)(completed_table$pct)
mean(completed$years)
