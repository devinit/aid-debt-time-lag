lapply(c("data.table", "rstudioapi", "XML", "httr", "scales","ggplot2","scales","Hmisc"), require, character.only = T)
setwd(dirname(getActiveDocumentContext()$path))
setwd("../")

#### Chart setup ####

reds = c(
  "#e84439", "#f8c1b2", "#f0826d", "#bc2629", "#8f1b13", "#fce3dc", "#fbd7cb", "#f6b0a0", "#ec6250", "#dc372d", "#cd2b2a", "#a21e25", "#6b120a"
)
oranges = c(
  "#eb642b", "#f6bb9d", "#f18e5e", "#d85b31", "#973915", "#fde5d4", "#fcdbbf", "#facbad", "#f3a47c", "#ee7644", "#cb5730", "#ac4622", "#7a2e05"
)
yellows = c(
  "#f49b21", "#fccc8e", "#f9b865", "#e48a00", "#a85d00", "#feedd4", "#fee7c1", "#fedcab", "#fac47e", "#f7a838", "#df8000", "#ba6b15", "#7d4712"
)
pinks = c(
  "#c2135b", "#e4819b", "#d64278", "#ad1257", "#7e1850", "#f9cdd0", "#f6b8c1", "#f3a5b6", "#e05c86", "#d12568", "#9f1459", "#8d0e56", "#65093d"
)
purples = c(
  "#893f90", "#c189bb", "#a45ea1", "#7b3b89", "#551f65", "#ebcfe5", "#deb5d6", "#cb98c4", "#af73ae", "#994d98", "#732c85", "#632572", "#42184c"
)
blues = c(
  "#0089cc", "#88bae5", "#5da3d9", "#0071b1", "#0c457b", "#d3e0f4", "#bcd4f0", "#a3c7eb", "#77adde", "#4397d3", "#105fa3", "#00538e", "#0a3a64"
)
greens = c(
  "#109e68", "#92cba9", "#5ab88a", "#1e8259", "#16513a", "#c5e1cb", "#b1d8bb", "#a2d1b0", "#74bf93", "#3b8c61", "#00694a", "#005b3e", "#07482e"
)
greys = c(
  "#6a6569", "#a9a6aa", "#847e84", "#555053", "#443e42", "#d9d4da", "#cac5cb", "#b3b0b7", "#b9b5bb", "#5a545a", "#736e73", "#4e484c", "#302b2e"
)

di_style = theme_bw() +
  theme(
    panel.border = element_blank()
    ,panel.grid.major.x = element_blank()
    ,panel.grid.minor.x = element_blank()
    ,panel.grid.major.y = element_line(colour = greys[2])
    ,panel.grid.minor.y = element_blank()
    ,panel.background = element_blank()
    ,axis.line.x = element_line(colour = "black")
    ,axis.line.y = element_blank()
    ,axis.ticks = element_blank()
    ,legend.position = "bottom"
  )

donut_style = di_style +
  theme(
    panel.grid.major.y = element_blank()
    ,axis.line.x = element_blank()
    ,axis.text = element_blank()
  )

rotate_x_text_45 = theme(
  axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)
)
rotate_x_text_90 = theme(
  axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)
)
#### End chart setup ####

# Download data and load
source("code/download_crs.R")
download_au_crs()
load("large_input/crs_2002_2022.RData")

# Remove without PN and subset year
crs = subset(crs, CrsID!="" & !is.na(CrsID) & Year >= 2012)

# Calculate unique donor/cid code
crs$dccid = paste(crs$DonorCode, crs$CrsID, sep="|")

# Load previous analysis
pn_metadata = fread("output/pn_metadata.csv")

# Percent without match
1 - (length(unique(pn_metadata$dccid))/length(unique(crs$dccid)))

# Merge
crs = merge(crs, pn_metadata, by=c("dccid", "Year"))

# By sector
short_names = c(
  "Administrative Costs of Donors" = "Admin. Costs of Donors",
  "I. Social Infrastructure & Services" = "Social Infrastructure",
  "I.1. Education" = "Education",
  "I.1.a. Education, Level Unspecified" = "Education, Unspecified",
  "I.1.b. Basic Education" = "Basic Education",
  "I.1.c. Secondary Education" = "Secondary Education",
  "I.1.d. Post-Secondary Education" = "Post-sec Education",
  "I.2. Health" = "Health",
  "I.2.a. Health, General" = "General Health",
  "I.2.b. Basic Health" = "Basic Health",
  "I.2.c. Non-communicable diseases (NCDs)" = "NCDs",
  "I.3. Population Policies/Programmes & Reproductive Health" = "Population, Repro. Health",
  "I.4. Water Supply & Sanitation" = "WASH",
  "I.5. Government & Civil Society" = "Gov’t, CSOs",
  "I.5.a. Government & Civil Society-general" = "Gov’t, CSOs, General",
  "I.5.b. Conflict, Peace & Security" = "Conflict, Peace, Security",
  "I.6. Other Social Infrastructure & Services" = "Social Services",
  "II. Economic Infrastructure & Services" = "Economic Infrastructure",
  "II.1. Transport & Storage" = "Transportation",
  "II.2. Communications" = "Communications",
  "II.3. Energy" = "Energy",
  "II.3.a. Energy Policy" = "Energy Policy",
  "II.3.b. Energy generation, renewable sources" = "Renewable Energy",
  "II.3.c. Energy generation, non-renewable sources" = "Non-renewable Energy",
  "II.3.d. Hybrid energy plants" = "Hybrid Energy",
  "II.3.e. Nuclear energy plants" = "Nuclear Energy",
  "II.3.f. Energy distribution" = "Energy Distribution",
  "II.4. Banking & Financial Services" = "Banking, Finance",
  "II.5. Business & Other Services" = "Business",
  "III. Production Sectors" = "Production",
  "III.1. Agriculture, Forestry, Fishing" = "Agriculture, Forestry, Fishing",
  "III.1.a. Agriculture" = "Agriculture",
  "III.1.b. Forestry" = "Forestry",
  "III.1.c. Fishing" = "Fishing",
  "III.2. Industry, Mining, Construction" = "Industry, Mining, Construction",
  "III.2.a. Industry" = "Industry",
  "III.2.b. Mineral Resources & Mining" = "Mineral Resources",
  "III.2.c. Construction" = "Construction",
  "III.3.a. Trade Policies & Regulations" = "Trade Policies",
  "III.3.b. Tourism" = "Tourism",
  "IV. Multi-Sector / Cross-Cutting" = "Multisector, Cross-cutting",
  "IV.1. General Environment Protection" = "Environment",
  "IV.2. Other Multisector" = "Multisector",
  "IX. Unallocated / Unspecified" = "Unspecified",
  "Refugees in Donor Countries" = "In-donor Refugees",
  "Sectors not specified" = "Not Specified",
  "VI.1. General Budget Support" = "Budget Support",
  "VI.2. Development Food Assistance" = "Dev. Food Assistance",
  "VI.3. Other Commodity Assistance" = "Commodity Assistance",
  "VII. Action Relating to Debt" = "Debt",
  "VIII. Humanitarian Aid" = "Humanitarian Aid",
  "VIII.1. Emergency Response" = "Emergency Response",
  "VIII.2. Reconstruction Relief & Rehabilitation" = "Reconstruction",
  "VIII.3. Disaster Prevention & Preparedness" = "Disaster Prevention"
)
percent_disbursed_by_sector = crs[,.(percent_disbursed=weighted.mean(percent_disbursed, USD_Commitment_Total, na.rm=T)), by=.(SectorName)]
percent_disbursed_by_sector = subset(percent_disbursed_by_sector, !is.nan(percent_disbursed))
percent_disbursed_by_sector = percent_disbursed_by_sector[order(-percent_disbursed_by_sector$percent_disbursed),]
fwrite(percent_disbursed_by_sector, "output/percent_disbursed_by_sector.csv")
percent_disbursed_by_sector$short_name = short_names[percent_disbursed_by_sector$SectorName]
percent_disbursed_by_sector$short_name = factor(
  percent_disbursed_by_sector$short_name,
  levels=percent_disbursed_by_sector$short_name
)
ggplot(percent_disbursed_by_sector, aes(x=short_name, y=percent_disbursed)) +
  geom_bar(stat="identity",fill=reds[1]) +
  scale_y_continuous(expand = c(0, 0), label=percent) +
  expand_limits(y=c(0, max(percent_disbursed_by_sector$percent_disbursed*1.1))) +
  di_style +
  labs(
    y="Percent of commitments\ndisbursed to date",
    x="",
    color=""
  ) +
  rotate_x_text_45
ggsave(
  filename="output/percent_disbursed_by_sector.png",
  height=5,
  width=8
)

# By donor
percent_disbursed_by_donor = crs[,.(percent_disbursed=weighted.mean(percent_disbursed, USD_Commitment_Total, na.rm=T)), by=.(DonorName)]
percent_disbursed_by_donor = subset(percent_disbursed_by_donor, !is.nan(percent_disbursed))
percent_disbursed_by_donor = percent_disbursed_by_donor[order(percent_disbursed_by_donor$percent_disbursed),]
fwrite(percent_disbursed_by_donor, "output/percent_disbursed_by_donor.csv")
percent_disbursed_by_donor$DonorName = factor(
  percent_disbursed_by_donor$DonorName,
  levels=percent_disbursed_by_donor$DonorName
)
ggplot(percent_disbursed_by_donor[1:10], aes(x=DonorName, y=percent_disbursed)) +
  geom_bar(stat="identity",fill=reds[1]) +
  scale_y_continuous(expand = c(0, 0), label=percent) +
  expand_limits(y=c(0, max(percent_disbursed_by_donor$percent_disbursed*1.1))) +
  di_style +
  labs(
    y="Percent of commitments\ndisbursed to date",
    x="",
    color=""
  ) +
  rotate_x_text_45
ggsave(
  filename="output/percent_disbursed_by_donor.png",
  height=5,
  width=8
)

# By donor bi_multi
percent_disbursed_by_bi_multi = crs[,.(percent_disbursed=weighted.mean(percent_disbursed, USD_Commitment_Total, na.rm=T)), by=.(Bi_MultiName)]
percent_disbursed_by_bi_multi = subset(percent_disbursed_by_bi_multi, !is.nan(percent_disbursed))
percent_disbursed_by_bi_multi = percent_disbursed_by_bi_multi[order(-percent_disbursed_by_bi_multi$percent_disbursed),]
fwrite(percent_disbursed_by_bi_multi, "output/percent_disbursed_by_bi_multi.csv")
percent_disbursed_by_bi_multi$Bi_MultiName = factor(
  percent_disbursed_by_bi_multi$Bi_MultiName,
  levels=percent_disbursed_by_bi_multi$Bi_MultiName
)
ggplot(percent_disbursed_by_bi_multi, aes(x=Bi_MultiName, y=percent_disbursed)) +
  geom_bar(stat="identity",fill=reds[1]) +
  scale_y_continuous(expand = c(0, 0), label=percent) +
  expand_limits(y=c(0, max(percent_disbursed_by_bi_multi$percent_disbursed*1.1))) +
  di_style +
  labs(
    y="Percent of commitments\ndisbursed to date",
    x="",
    color=""
  ) +
  rotate_x_text_45
ggsave(
  filename="output/percent_disbursed_by_bi_multi.png",
  height=5,
  width=8
)

# By flow
percent_disbursed_by_flow = crs[,.(percent_disbursed=weighted.mean(percent_disbursed, USD_Commitment_Total, na.rm=T)), by=.(FlowName)]
percent_disbursed_by_flow = subset(percent_disbursed_by_flow, !is.nan(percent_disbursed))
percent_disbursed_by_flow = percent_disbursed_by_flow[order(-percent_disbursed_by_flow$percent_disbursed),]
fwrite(percent_disbursed_by_flow, "output/percent_disbursed_by_flow.csv")
percent_disbursed_by_flow$FlowName = factor(
  percent_disbursed_by_flow$FlowName,
  levels=percent_disbursed_by_flow$FlowName
)
ggplot(percent_disbursed_by_flow, aes(x=FlowName, y=percent_disbursed)) +
  geom_bar(stat="identity",fill=reds[1]) +
  scale_y_continuous(expand = c(0, 0), label=percent) +
  expand_limits(y=c(0, max(percent_disbursed_by_flow$percent_disbursed*1.1))) +
  di_style +
  labs(
    y="Percent of commitments\ndisbursed to date",
    x="",
    color=""
  ) +
  rotate_x_text_45
ggsave(
  filename="output/percent_disbursed_by_flow.png",
  height=5,
  width=8
)

# Climate adaptation
climate_adapt_recent_crs_disb = subset(
  crs, 
  ClimateAdaptation %in% c(0, 1, 2))
adapt_map = c(
  "0" = "No adaptation",
  "1" = "Significant adaptation",
  "2" = "Principal adaptation"
)
climate_adapt_recent_crs_disb$label = adapt_map[
  as.character(climate_adapt_recent_crs_disb$ClimateAdaptation)
]
percent_disbursed_by_adaptation = climate_adapt_recent_crs_disb[,.(percent_disbursed=weighted.mean(percent_disbursed, USD_Commitment_Total, na.rm=T)), by=.(label, FlowName)]
percent_disbursed_by_adaptation = subset(
  percent_disbursed_by_adaptation,
  !is.nan(percent_disbursed) &
    FlowName %in% c("ODA Grants", "ODA Loans")
)
percent_disbursed_by_adaptation = percent_disbursed_by_adaptation[order(-percent_disbursed_by_adaptation$percent_disbursed),]
fwrite(percent_disbursed_by_adaptation, "output/percent_disbursed_by_adaptation.csv")
percent_disbursed_by_adaptation$label = factor(
  percent_disbursed_by_adaptation$label,
  levels=adapt_map
)
ggplot(percent_disbursed_by_adaptation, aes(x=label, y=percent_disbursed, group=FlowName, fill=FlowName)) +
  geom_bar(stat="identity", position="dodge") +
  scale_y_continuous(expand = c(0, 0), label=percent) +
  scale_fill_manual(values=greens) +
  expand_limits(y=c(0, max(percent_disbursed_by_adaptation$percent_disbursed*1.1))) +
  di_style +
  labs(
    y="Percent of commitments\ndisbursed to date",
    x="",
    fill=""
  ) +
  rotate_x_text_45
ggsave(
  filename="output/percent_disbursed_by_adaptation.png",
  height=5,
  width=8
)

climate_mitig_recent_crs_disb = subset(
  crs, 
  ClimateMitigation %in% c(0, 1, 2))
mitig_map = c(
  "0" = "No mitigation",
  "1" = "Significant mitigation",
  "2" = "Principal mitigation"
)
climate_mitig_recent_crs_disb$label = mitig_map[
  as.character(climate_mitig_recent_crs_disb$ClimateMitigation)
]
percent_disbursed_by_mitigation = climate_mitig_recent_crs_disb[,.(percent_disbursed=weighted.mean(percent_disbursed, USD_Commitment_Total, na.rm=T)), by=.(label, FlowName)]
percent_disbursed_by_mitigation = subset(percent_disbursed_by_mitigation, !is.nan(percent_disbursed)  &
                             FlowName %in% c("ODA Grants", "ODA Loans"))
percent_disbursed_by_mitigation = percent_disbursed_by_mitigation[order(-percent_disbursed_by_mitigation$percent_disbursed),]
fwrite(percent_disbursed_by_mitigation, "output/percent_disbursed_by_mitigation.csv")
percent_disbursed_by_mitigation$label = factor(
  percent_disbursed_by_mitigation$label,
  levels=mitig_map
)
ggplot(percent_disbursed_by_mitigation, aes(x=label, y=percent_disbursed, group=FlowName, fill=FlowName)) +
  geom_bar(stat="identity",position="dodge") +
  scale_y_continuous(expand = c(0, 0), label=percent) +
  scale_fill_manual(values=blues) +
  expand_limits(y=c(0, max(percent_disbursed_by_mitigation$percent_disbursed*1.1))) +
  di_style +
  labs(
    y="Percent of commitments\ndisbursed to date",
    x="",
    fill=""
  ) +
  rotate_x_text_45
ggsave(
  filename="output/percent_disbursed_by_mitigation.png",
  height=5,
  width=8
)

# By income
percent_disbursed_by_income = crs[,.(percent_disbursed=weighted.mean(percent_disbursed, USD_Commitment_Total, na.rm=T)), by=.(IncomegroupName)]
percent_disbursed_by_income = subset(percent_disbursed_by_income, !is.nan(percent_disbursed))
percent_disbursed_by_income = percent_disbursed_by_income[order(-percent_disbursed_by_income$percent_disbursed),]
fwrite(percent_disbursed_by_income, "output/percent_disbursed_by_income.csv")
percent_disbursed_by_income$IncomegroupName = factor(
  percent_disbursed_by_income$IncomegroupName,
  levels=percent_disbursed_by_income$IncomegroupName
)
ggplot(percent_disbursed_by_income, aes(x=IncomegroupName, y=percent_disbursed)) +
  geom_bar(stat="identity",fill=reds[1]) +
  scale_y_continuous(expand = c(0, 0), label=percent) +
  expand_limits(y=c(0, max(percent_disbursed_by_income$percent_disbursed*1.1))) +
  di_style +
  labs(
    y="Percent of commitments\ndisbursed to date",
    x="",
    color=""
  ) +
  rotate_x_text_45
ggsave(
  filename="output/percent_disbursed_by_income.png",
  height=5,
  width=8
)
