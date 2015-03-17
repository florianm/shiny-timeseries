#' Test script for timeseries analysis and visualisation
#'
library(ggplot2)
library(plyr)
library(lubridate)

#' EMBMP Rainfall
#' http://internal-data.dpaw.wa.gov.au/dataset/rainfall-relevant-to-eighty-mile-beach-marine-park
csv_url <- "http://internal-data.dpaw.wa.gov.au/dataset/0f86add2-eb6d-4ba9-bd9d-2f5590c005cd/resource/4ed57506-12c7-4702-a11f-fc0c90bf0d16/download/IDCJAC0001004068Data1.csv"
d <- read.table(csv_url, sep=",", header=T, stringsAsFactors=T)
d


#' WNIMP Black Bream catch during annual competitions
#' http://internal-data.dpaw.wa.gov.au/dataset/cd754eda-0998-49e0-852a-2ee5d8e3b075/resource/b3a36efe-039b-4a90-96e0-0b90441c14b8/download/wnimpfishingcompetitions.csv
Y=read.table("http://internal-data.dpaw.wa.gov.au/dataset/cd754eda-0998-49e0-852a-2ee5d8e3b075/resource/b3a36efe-039b-4a90-96e0-0b90441c14b8/download/wnimpfishingcompetitions.csv",sep=",",header=TRUE)

# pdf("wnimp-fishing_competitions.pdf", width=7, height=5)

pd=position_dodge(0.25)
colnames(Y)
ggplot(Y, aes(x=year, y=weight)) +
  geom_line(position=pd) +
  geom_point(position=pd, size=3) +
  xlab("Year") +
  ylab("Average weight (g)") +
  scale_x_continuous(limits=c(min(Y$year-0.15), max(Y$year+0.15)), breaks=min(Y$year):max(Y$year)) +
  theme_bw() +
  theme(
    axis.text.x = element_text(size=14),
    axis.text.y = element_text(size=14),
    axis.title.x=element_blank(),
    axis.title.y=element_text(size=14),
    axis.line=element_line(colour="black"),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    panel.border=element_blank(),
    panel.background=element_blank(),
    legend.justification=c(1,10), legend.position=c(1,10), # Position legend in top right
    legend.title = element_blank(),
    legend.key = element_blank())
# dev.off()
