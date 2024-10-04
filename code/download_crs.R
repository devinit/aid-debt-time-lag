download_au_crs = function(year_start=2002, year_end=2022){
  lapply(c("data.table"), require, character.only = T)
  years = year_start:year_end
  outfile_name = paste0(
    "large_input/crs_",
    year_start,
    "_",
    year_end,
    ".RData"
  )
  if(!file.exists(outfile_name)){
    # Set up columns to keep from large dataset
    keep = c(
      "Year", "CommitmentDate", "ExpectedStartDate", "CompletionDate", "DonorName", "DonorCode",
      "RecipientName", "DERecipientcode", "SectorName", "SectorCode", "FlowName", "FlowCode",
      "ProjectNumber", "CrsID", "IncomegroupName", "IncomegroupCode", "Bi_Multi",
      "ClimateAdaptation", "ClimateMitigation",
      "USD_Disbursement", "USD_Disbursement_Defl",
      "USD_Commitment", "USD_Commitment_Defl"
    )
    
    # Define AU country codes
    au_ccs = c(
      'DZA','AGO','BEN','BWA','BFA','BDI','CMR',
      'CPV','CAF','TCD','COM','COD','COG','DJI',
      'EGY','GNQ','ERI','SWZ','ETH','GAB','GMB',
      'GHA','GIN','GNB','CIV','KEN','LSO','LBR',
      'LBY','MDG','MWI','MLI','MRT','MUS','MAR',
      'MOZ','NAM','NER','NGA','RWA','ESH','STP',
      'SEN','SYC','SLE','SOM','ZAF','SSD','SDN',
      'TZA','TGO','TUN','UGA','ZMB','ZWE'
    )
    
    # Iterate through and load yearly files
    crs_list = list()
    for(i in 1:length(years)){
      year = years[[i]]
      dataset = fread(paste0("https://github.com/devinit/gha_automation/raw/main/IHA/datasets/crs_", year, ".gz"), showProgress = F)
      dataset = subset(dataset, DERecipientcode %in% au_ccs, select=keep)
      crs_list[[i]] = dataset
      rm(dataset)
      message(years[[i]])
    }
    crs = rbindlist(crs_list)
    rm(crs_list)
    # Fix missing income groups
    income_groups = unique(crs[,c("DERecipientcode", "IncomegroupCode", "IncomegroupName")])
    income_groups = subset(income_groups, !is.na(IncomegroupCode))
    crs[,c("IncomegroupCode", "IncomegroupName")] = NULL
    crs = merge(crs, income_groups, by="DERecipientcode")
    
    # Add bi_multi mapping
    bi_multi_names = c(
      "1"="Bilateral",
      "3"="Bilateral",
      "4"="Multilateral",
      "6"="Private",
      "7"="Bilateral",
      "8"="Bilateral"
    )
    bi_multi = crs[,.(Bi_Multi=min(Bi_Multi, na.rm=T)), by=.(DonorCode)]
    bi_multi$Bi_MultiName = bi_multi_names[
      as.character(bi_multi$Bi_Multi)
    ]
    crs$Bi_Multi = NULL
    crs = merge(crs, bi_multi, by="DonorCode")
    # save file
    save(crs, file=outfile_name)
  }
}