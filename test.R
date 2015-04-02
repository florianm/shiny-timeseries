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




# In situ seawater temp
lubridate::parse_date_time(d$date.time, orders=date_formats)
x = "2014-12-31T23:55:59+0800"
lubridate::parse_date_time(x, orders=c("YmdHMSz"), tz="Australia/Perth")

csv_url <- "http://internal-data.dpaw.wa.gov.au/dataset/82992b4c-18df-4282-a290-d5fac9a53171/resource/ac8c3854-bc81-4141-a0a9-acfdfb6fdbed/download/aimsnambungbaywatertemperature11may2012to16feb2015.csv"

cc <- c(
  "date.time" = "POSIXct",
  "datetime" = "POSIXct",
  "date" = "POSIXct",
  "time" = "POSIXt",
  "year" = "POSIXct"
  )

header = read.csv(csv_url, sep=",", header=T, nrows=1)
ccl = names(header)
d <- read.csv(csv_url, sep=",", header=T, colClasses=)

source("global.R")

df <- as.data.frame(
  lapply(read.table('http://internal-data.dpaw.wa.gov.au/dataset/c3802293-be1a-4060-be1e-02e881cd7b19/resource/b44b811c-d46d-40e9-99d5-c198a264dbd2/download/jbmpinvertebrate2013.csv', sep=',', header=T, stringsAsFactors=T),
         function(x) {if(is.factor(x)){x <- lubridate::parse_date_time(x, orders=c('YmdHMSz', 'YmdHMS','Ymd','dmY'), tz='Australia/Perth')};x}))

s <- summary(df)
cl <- lapply(df, class)

d = ckan_json(api_call="tag_show", oid="format_csv_ts")
items <- setNames(lapply(d$packages, function(x){x$id}), lapply(d$packages, function(x){x$title}))
