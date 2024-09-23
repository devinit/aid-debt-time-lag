lapply(c("data.table", "rstudioapi", "XML", "httr"), require, character.only = T)
setwd(dirname(getActiveDocumentContext()$path))
setwd("../")

# From lag_analysis.R
load("large_input/crs.RData")

# Calculate commitment year
pre_subset_sums = crs[,.(USD_Disbursement_Defl_original=sum(USD_Disbursement_Defl, na.rm=T)), by=.(FlowName)]
crs$CommitmentYear = as.numeric(substr(crs$CommitmentDate, 1, 4))
crs = subset(crs, !is.na(CommitmentYear))
post_subset_sums = crs[,.(USD_Disbursement_Defl_subset=sum(USD_Disbursement_Defl, na.rm=T)), by=.(FlowName)]
subset_analysis_df = merge(pre_subset_sums, post_subset_sums, by="FlowName", all=T)
subset_analysis_df$USD_Disbursement_Defl_subset[which(is.na(subset_analysis_df$USD_Disbursement_Defl_subset))] = 0
subset_analysis_df$USD_Disbursement_Defl_original[which(is.na(subset_analysis_df$USD_Disbursement_Defl_original))] = 0
subset_analysis_df$percentage = subset_analysis_df$USD_Disbursement_Defl_subset / subset_analysis_df$USD_Disbursement_Defl_original
fwrite(subset_analysis_df, "output/flow_without_commitment.csv")

# Calculate commitment sum table
commitment_sum_table = crs[,.(
  USD_Commitment_Defl=sum(USD_Commitment_Defl, na.rm=T)
), by=.(Year, FlowCode)]
setnames(commitment_sum_table, "Year", "CommitmentYear")

# For each year, calculate the expected disbursement
# as a ratio of the committed year's disbursements
analysis_years = 2012:2022
expected_value_list = list()
expected_value_index = 1
for(year in analysis_years){
  all_committed_this_year = subset(crs, CommitmentYear == year)
  total_disbursed_by_commitments_this_year = all_committed_this_year[,.(USD_Disbursement_Defl=sum(USD_Disbursement_Defl, na.rm=T)), by=.(FlowCode)]
  total_commitment_in_this_year = commitment_sum_table[which(commitment_sum_table$CommitmentYear == year),]
  expected_df = merge(
    total_disbursed_by_commitments_this_year,
    total_commitment_in_this_year,
    by=c("FlowCode"),
    all=T
  )
  expected_value_list[[expected_value_index]] = expected_df
  expected_value_index = expected_value_index + 1
  message(year)
}

flow_codelist = unique(crs[,c("FlowCode", "FlowName")])

aid_debt_df = rbindlist(expected_value_list)
aid_debt_df$USD_Disbursement_Defl[which(is.na(aid_debt_df$USD_Disbursement_Defl))] = 0
aid_debt_df$USD_Commitment_Defl[which(is.na(aid_debt_df$USD_Commitment_Defl))] = 0
aid_debt_df$aid_debt_ratio = aid_debt_df$USD_Disbursement_Defl / aid_debt_df$USD_Commitment_Defl
aid_debt_df = merge(aid_debt_df, flow_codelist)

aid_debt_agg = aid_debt_df[,.(USD_Disbursement_Defl=sum(USD_Disbursement_Defl), USD_Commitment_Defl=sum(USD_Commitment_Defl)), by=.(FlowCode)]
aid_debt_agg$aid_debt_ratio = aid_debt_agg$USD_Disbursement_Defl / aid_debt_agg$USD_Commitment_Defl

aid_debt_agg = merge(aid_debt_agg, flow_codelist)
fwrite(aid_debt_agg, "output/aid_debt_by_flow.csv")
