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


#' QCC plots
library(qcc)
data=read.csv("http://internal-data.dpaw.wa.gov.au/dataset/be25828b-e528-462e-a8a2-514723f7fe1e/resource/09556e7e-2cd2-4d1c-95bf-c299d41c7142/download/jbmpaslpupcounts.csv")

x_col <- "abundance"
y_col <- "label"
filter_data <- TRUE
filter_col <- "location"
filter_by <- "Beagles"
x_label <- "Season"
y_label <- "Abundance"
title_text <- "Abundance of Australian Sea Lion pups at Beagles Bay"
cal0 = 1
cal1 = 5
new0 = x_cal_max + 1
new1 = length(d)
y_min = 0
y_max = 100
n_sigma = 2
add_stats = TRUE
group_statistics_type = "xbar.one"
# "xbar"	mean	means of a continuous process variable
# "R"	range	ranges of a continuous process variable
# "S"	standard deviation	standard deviations of a continuous variable
# "xbar.one"	mean	one-at-time data of a continuous process variable
# "p"	proportion	proportion of nonconforming units
# "np"	count	number of nonconforming units
# "c"	count	nonconformities per unit
# "u"	count	average nonconformities per unit
# "g"	count	number of non-events between events


if (filter_data == TRUE) {
  df = subset(data, data[[filter_col]] == filter_by)
} else {df = data}

df.groups = qcc.groups(df[[x_col]], df[[y_col]])
df.t=t(df.groups)
d=na.omit(df.t)
qcc(d[cal0:cal1,],
    newdata=d[new0:new1,],
    type=group_statistics_type,
    add.stats=add_stats,
    xlab=x_label,
    ylab=y_label,
    title = title_text,
    ylim=c(y_min, y_max),
    nsigmas=n_sigma
)

