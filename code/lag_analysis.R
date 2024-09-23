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
    ,plot.background = element_blank()
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

# From ratio_analysis.R
load("large_input/crs.RData")

# Calculate commitment year
crs$CommitmentYear = as.numeric(substr(crs$CommitmentDate, 1, 4))
crs$CommitmentYear[which(crs$CommitmentYear==1900)] = NA
crs$CommitmentYear[which(crs$CommitmentYear > crs$Year)] = 
  crs$Year[which(crs$CommitmentYear > crs$Year)]
crs$lag = crs$Year - crs$CommitmentYear
describe(crs$lag)

# By disbursement year
lag_by_year = subset(crs, USD_Disbursement > 0)[,.(mean_lag=mean(lag, na.rm=T)), by=.(Year)]

ggplot(lag_by_year, aes(x=Year, y=mean_lag)) +
  geom_line(color=reds[1]) +
  geom_point(color=reds[1]) +
  scale_y_continuous(expand = c(0, 0), n.breaks=6) +
  scale_x_continuous(n.breaks = 15) +
  expand_limits(y=c(0, max(lag_by_year$mean_lag*1.1))) +
  di_style +
  labs(
    y="Mean years between\ncommitment and disbursement",
    x="",
    color=""
  ) +
  rotate_x_text_45
fwrite(lag_by_year, "output/lag_by_year.csv")

y2009 = subset(crs, Year==2009)
View(table(y2009$CommitmentYear))
hist(y2009$CommitmentYear)
y2008 = subset(crs, Year==2008)
View(table(y2008$CommitmentYear))
hist(y2008$CommitmentYear)

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
recent_crs_disb = subset(crs, Year >= 2012 & USD_Disbursement > 0)
lag_by_sector = recent_crs_disb[,.(mean_lag=mean(lag, na.rm=T)), by=.(SectorName)]
lag_by_sector = subset(lag_by_sector, !is.nan(mean_lag))
lag_by_sector = lag_by_sector[order(-lag_by_sector$mean_lag),]
fwrite(lag_by_sector, "output/lag_by_sector.csv")
lag_by_sector$short_name = short_names[lag_by_sector$SectorName]
lag_by_sector$short_name = factor(
  lag_by_sector$short_name,
  levels=lag_by_sector$short_name
)
ggplot(lag_by_sector, aes(x=short_name, y=mean_lag)) +
  geom_bar(stat="identity",fill=reds[1]) +
  scale_y_continuous(expand = c(0, 0)) +
  expand_limits(y=c(0, max(lag_by_year$mean_lag*1.1))) +
  di_style +
  labs(
    y="Mean years between\ncommitment and disbursement",
    x="",
    color=""
  ) +
  rotate_x_text_45

# By donor
lag_by_donor = recent_crs_disb[,.(mean_lag=mean(lag, na.rm=T)), by=.(DonorName)]
lag_by_donor = subset(lag_by_donor, !is.nan(mean_lag))
lag_by_donor = lag_by_donor[order(-lag_by_donor$mean_lag),]
fwrite(lag_by_donor, "output/lag_by_donor.csv")
lag_by_donor$DonorName = factor(
  lag_by_donor$DonorName,
  levels=lag_by_donor$DonorName
)
ggplot(lag_by_donor[1:10], aes(x=DonorName, y=mean_lag)) +
  geom_bar(stat="identity",fill=reds[1]) +
  scale_y_continuous(expand = c(0, 0)) +
  expand_limits(y=c(0, max(lag_by_donor$mean_lag*1.1))) +
  di_style +
  labs(
    y="Mean years between\ncommitment and disbursement",
    x="",
    color=""
  ) +
  rotate_x_text_45

# By flow
lag_by_flow = recent_crs_disb[,.(mean_lag=mean(lag, na.rm=T)), by=.(FlowName)]
lag_by_flow = subset(lag_by_flow, !is.nan(mean_lag))
lag_by_flow = lag_by_flow[order(-lag_by_flow$mean_lag),]
fwrite(lag_by_flow, "output/lag_by_flow.csv")
lag_by_flow$FlowName = factor(
  lag_by_flow$FlowName,
  levels=lag_by_flow$FlowName
)
ggplot(lag_by_flow, aes(x=FlowName, y=mean_lag)) +
  geom_bar(stat="identity",fill=reds[1]) +
  scale_y_continuous(expand = c(0, 0)) +
  expand_limits(y=c(0, max(lag_by_flow$mean_lag*1.1))) +
  di_style +
  labs(
    y="Mean years between\ncommitment and disbursement",
    x="",
    color=""
  ) +
  rotate_x_text_45
