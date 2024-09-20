lapply(c("data.table", "rstudioapi", "XML", "httr"), require, character.only = T)
setwd(dirname(getActiveDocumentContext()$path))
setwd("../")

# Set up years of analysis
# years = 2002:2022
# 
# # Set up columns to keep from large dataset
# keep = c(
#   "Year", "CommitmentDate", "ExpectedStartDate", "CompletionDate", "DonorName",
#   "RecipientName", "DERecipientcode", "SectorName", "FlowName",
#   "USD_Disbursement", "USD_Disbursement_Defl",
#   "USD_Commitment", "USD_Commitment_Defl"
# )
# 
# # Iterate through and load yearly files
# crs_list = list()
# for(i in 1:length(years)){
#   year = years[[i]]
#   dataset = fread(paste0("https://github.com/devinit/gha_automation/raw/main/IHA/datasets/crs_", year, ".gz"), showProgress = F)
#   dataset = dataset[,keep,with=F]
#   crs_list[[i]] = dataset
#   rm(dataset)
#   message(years[[i]])
# }
# crs = rbindlist(crs_list)
# rm(crs_list)
# # Run this once
# save(crs, file="large_input/crs.RData")
# And then you can start from here
load("large_input/crs.RData")

# Calculate commitment year
crs$CommitmentYear = as.numeric(substr(crs$CommitmentDate, 1, 4))

# Calculate commitment sum table
commitment_sum_table = crs[,.(
  USD_Commitment=sum(USD_Commitment, na.rm=T)
), by=.(Year)]
setnames(commitment_sum_table, "Year", "CommitmentYear")

# For each year, calculate the expected disbursement
# as a ratio of the committed year's disbursements
analysis_years = 2012:2022
expected_value_list = list()
expected_value_index = 1
for(year in analysis_years){
  commitment_range = year - 10
  year_subset = subset(crs, Year==year & CommitmentYear >= commitment_range)
  year_disbursements_by_comitted_year = year_subset[,.(
    USD_Disbursement=sum(USD_Disbursement, na.rm=T),
    USD_Commitment_actual=sum(USD_Commitment, na.rm=T)
  ), by=.(CommitmentYear)]
  year_disbursement_sum = sum(year_disbursements_by_comitted_year$USD_Disbursement, na.rm=T)
  year_disbursements_by_comitted_year$percentage = 
    year_disbursements_by_comitted_year$USD_Disbursement / 
    year_disbursement_sum
  year_disbursements_by_comitted_year = merge(
    year_disbursements_by_comitted_year,
    commitment_sum_table,
    by="CommitmentYear",
    all.x=T
  )
  year_disbursements_by_comitted_year$USD_Commitment_expected_contribution = 
    year_disbursements_by_comitted_year$USD_Commitment * 
    year_disbursements_by_comitted_year$percentage
  expected_disbursement = sum(
    year_disbursements_by_comitted_year$USD_Commitment_expected_contribution,
    na.rm=T
  )
  current_year = year_disbursements_by_comitted_year[which(CommitmentYear==year),]
  error_rate = abs(current_year$USD_Commitment_expected_contribution - current_year$USD_Commitment_actual) / current_year$USD_Commitment_actual
  expected_df = data.frame(
    Year=year,
    USD_Disbursement=year_disbursement_sum,
    USD_Expected_Disbursement=expected_disbursement,
    error_rate=error_rate
  )
  expected_value_list[[expected_value_index]] = expected_df
  expected_value_index = expected_value_index + 1
  message(year)
}

aid_debt_df = rbindlist(expected_value_list)
aid_debt_df$aid_debt_ratio = aid_debt_df$USD_Disbursement / aid_debt_df$USD_Expected_Disbursement

