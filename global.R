library(shiny)
require(ggplot2) || install.packages("ggplot2")
require(markdown) || install.packages("markdown")
require(lubridate) || install.packages("lubridate")
require(qcc) || install.packages("qcc")
require(RCurl) || install.packages("RCurl")
require(scales) || install.packages("scales")

#------------------------------------------------------------------------------#
# Data loading

# Selected time series datasets
test_datasets <- c(
  "JMP In Situ Water Temp" = "http://internal-data.dpaw.wa.gov.au/dataset/82992b4c-18df-4282-a290-d5fac9a53171/resource/ac8c3854-bc81-4141-a0a9-acfdfb6fdbed/download/aimsnambungbaywatertemperature11may2012to16feb2015.csv",
  "WNIMP Black Bream avg weight" = "http://internal-data.dpaw.wa.gov.au/dataset/cd754eda-0998-49e0-852a-2ee5d8e3b075/resource/b3a36efe-039b-4a90-96e0-0b90441c14b8/download/wnimpfishingcompetitions.csv",
  "EMBMP Rainfall" = "http://internal-data.dpaw.wa.gov.au/dataset/0f86add2-eb6d-4ba9-bd9d-2f5590c005cd/resource/4ed57506-12c7-4702-a11f-fc0c90bf0d16/download/IDCJAC0001004068Data1.csv",
  "JBMP ASL pup counts" = "http://internal-data.dpaw.wa.gov.au/dataset/be25828b-e528-462e-a8a2-514723f7fe1e/resource/09556e7e-2cd2-4d1c-95bf-c299d41c7142/download/jbmpaslpupcounts.csv"
)

# Possible date formats and timezone for lubridate::parse_date_time
ldo <- c("YmdHMSz", "YmdHMS","Ymd", "Y")
ldz <- "Australia/Perth"


#------------------------------------------------------------------------------#
# The GGplot2 theme for MPA graphs
mpa_theme <- theme(axis.text.x = element_text(size=14),
                   axis.text.y = element_text(size=14),
                   axis.title.x=element_text(size=14), # or element_blank(),
                   axis.title.y=element_text(size=14),
                   axis.line=element_line(colour="black"),
                   panel.grid.minor = element_blank(),
                   panel.grid.major = element_blank(),
                   panel.border=element_blank(),
                   panel.background=element_blank(),
                   legend.justification=c(1,10), legend.position=c(1,10), # Position legend in top right
                   legend.title = element_blank(),
                   legend.key = element_blank())

mpa_theme_text <- paste(
  "  theme(",
  "    axis.text.x = element_text(size=14),",
  "    axis.text.y = element_text(size=14),",
  "    axis.title.x=element_text(size=14), # or element_blank(),",
  "    axis.title.y=element_text(size=14),",
  "    axis.line=element_line(colour='black'),",
  "    panel.grid.minor = element_blank(),",
  "    panel.grid.major = element_blank(),",
  "    panel.border=element_blank(),",
  "    panel.background=element_blank(),",
  "    legend.justification=c(1,10),",
  "    legend.position=c(1,10), # Position legend in top right",
  "    legend.title = element_blank(),",
  "    legend.key = element_blank()",
  "  )",
  sep="\n"
)
