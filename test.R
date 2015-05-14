#' Test script for developing timeseries analysis and visualisation
#'
library(ggplot2)
library(plyr)
library(lubridate)


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

# Data cleaning
# Asset in situ seawater temperature
source("global.R")
url <- "http://internal-data.dpaw.wa.gov.au/dataset/258537e3-7ea3-40e6-ae73-db0fd02b19a3/resource/054e1c99-0b26-4af7-837c-7a053bdd1e81/download/rsmpinsitutemp.csv"
filename <- "datarsmpinsitutemp.csv"
d <- get_data(url)
summary(d)
lapply(d, class)
write.table(d, file=filename, sep=",", row.names=F, col.names=T, quote=T)

dplyr::rename(d, Date=date)

d2 <- tidyr::gather(d[1:4], "site", "temperature", 2:4)
summary(d2)
lapply(d2, class)
write.table(d2, file=filename, sep=",", row.names=F, col.names=T, quote=T)

