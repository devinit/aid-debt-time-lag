lapply(c("data.table", "rstudioapi", "XML", "httr", "scales"), require, character.only = T)
setwd(dirname(getActiveDocumentContext()$path))
setwd("../")

# Set up years of analysis
years = 2002:2022

# Set up columns to keep from large dataset
keep = c(
  "Year", "CommitmentDate", "ExpectedStartDate", "CompletionDate", "DonorName", "DonorCode",
  "RecipientName", "DERecipientcode", "SectorName", "SectorCode", "FlowName", "FlowCode",
  "ProjectNumber", "CrsID",
  "ClimateAdaptation", "ClimateMitigation",
  "USD_Disbursement", "USD_Disbursement_Defl",
  "USD_Commitment", "USD_Commitment_Defl"
)

# Iterate through and load yearly files
crs_list = list()
for(i in 1:length(years)){
  year = years[[i]]
  dataset = fread(paste0("https://github.com/devinit/gha_automation/raw/main/IHA/datasets/crs_", year, ".gz"), showProgress = F)
  dataset = dataset[,keep,with=F]
  crs_list[[i]] = dataset
  rm(dataset)
  message(years[[i]])
}
crs = rbindlist(crs_list)
rm(crs_list)
# Run this once
save(crs, file="large_input/crs.RData")
# And then you can start from here
load("large_input/crs.RData")

# Calculate commitment year
pre_subset_sum = sum(crs$USD_Disbursement_Defl, na.rm=T)
crs$CommitmentYear = as.numeric(substr(crs$CommitmentDate, 1, 4))
crs = subset(crs, !is.na(CommitmentYear))
post_subset_sum = sum(crs$USD_Disbursement_Defl, na.rm=T)
percent(post_subset_sum/pre_subset_sum)

# Calculate commitment sum table
commitment_sum_table = crs[,.(
  USD_Commitment_Defl=sum(USD_Commitment_Defl, na.rm=T)
), by=.(Year)]
setnames(commitment_sum_table, "Year", "CommitmentYear")

# For each year, calculate the expected disbursement
# as a ratio of the committed year's disbursements
analysis_years = 2012:2022
expected_value_list = list()
expected_value_index = 1
for(year in analysis_years){
  all_committed_this_year = subset(crs, CommitmentYear == year)
  total_disbursed_by_commitments_this_year = sum(all_committed_this_year$USD_Disbursement_Defl, na.rm=T)
  total_commitment_in_this_year = commitment_sum_table[which(commitment_sum_table$CommitmentYear == year),"USD_Commitment_Defl"][[1]]
  expected_df = data.frame(
    CommitmentYear=year,
    USD_Disbursement_Defl=total_disbursed_by_commitments_this_year,
    USD_Commitment_Defl=total_commitment_in_this_year
  )
  expected_value_list[[expected_value_index]] = expected_df
  expected_value_index = expected_value_index + 1
  message(year)
}


aid_debt_df = rbindlist(expected_value_list)
aid_debt_df$USD_Disbursement_Defl[which(is.na(aid_debt_df$USD_Disbursement_Defl))] = 0
aid_debt_df$USD_Commitment_Defl[which(is.na(aid_debt_df$USD_Commitment_Defl))] = 0
aid_debt_df$aid_debt_ratio = aid_debt_df$USD_Disbursement_Defl / aid_debt_df$USD_Commitment_Defl

fwrite(aid_debt_df, "output/aid_debt_by_year.csv")
