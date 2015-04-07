#' Test script for developing timeseries analysis and visualisation
#'
library(ggplot2)
library(plyr)
library(lubridate)

# Data cleaning
# Asset in situ seawater temperature
source("global.R")

d <- get_data(url)
summary(d); str(d)
d2 <- tidyr::gather(d[1:maxcol], "site", "temperature", mincol:maxcol)
# d2 <- d2[!is.na(d2$date),]
summary(d2); str(d2)
write.table(d2, file=filename, sep=",", row.names=F, col.names=T, quote=T)

# JBMP
url <- "http://internal-data.dpaw.wa.gov.au/dataset/82992b4c-18df-4282-a290-d5fac9a53171/resource/6e8107b1-b4c0-43e3-8ce7-b3f8abe0be09/download/jbmpinsitutemp.csv"
filename <- "datajbmpinsitutemp.csv"
mincol<- 2; maxcol <- 4

# MMP
url <- "http://internal-data.dpaw.wa.gov.au/dataset/60dc9e19-9531-4afa-8c9e-e62ed5da78f0/resource/5697e3d3-36e0-4fcb-9057-d278a425647e/download/mmpinsitutemp.csv"
filename <- "datammpinsitutemp.csv"
mincol<- 2; maxcol <- 4

# NCMP
url <- "http://internal-data.dpaw.wa.gov.au/dataset/e157ccfb-45db-447f-bd3e-2084287bb12e/resource/e099bffb-e822-4863-8cdb-857e51c114f1/download/ncmpinsitutemp.csv"
filename <- "datancmpinsitutemp.csv"
mincol<- 2; maxcol <- 6

# DAMPA
url <- "http://internal-data.dpaw.wa.gov.au/dataset/67527605-0064-4f39-8a95-009c2de55986/resource/f39d3a7d-7c76-41e3-a77b-2d3a2b663a0f/download/dampainsitutemp.csv"
filename <- "datadampainsitutemp.csv"
mincol<- 2; maxcol <- 6

# MBIMPA
url <- "http://internal-data.dpaw.wa.gov.au/dataset/b12b673a-ee02-40ac-90e0-d3e95d418be7/resource/19de9e1c-dbbf-42ce-8b0e-e0cfcbb5ba79/download/mbimpainsitutemp.csv"
filename <- "datambimpainsitutemp.csv"
mincol<- 2; maxcol <- 13

# NMPA
url <- "http://internal-data.dpaw.wa.gov.au/dataset/089efeb3-d73d-4123-9222-03103c283583/resource/90220625-40ae-47f0-a364-689c0469c528/download/nmpainsitutemp.csv"
filename <- "datanmpainsitutemp.csv"
mincol<- 2; maxcol <- 6

# RSMP
url <- "http://internal-data.dpaw.wa.gov.au/dataset/258537e3-7ea3-40e6-ae73-db0fd02b19a3/resource/054e1c99-0b26-4af7-837c-7a053bdd1e81/download/rsmpinsitutemp.csv"
filename <- "datarsmpinsitutemp.csv"
mincol<- 2; maxcol <- 4

# SIMP
url <- "http://internal-data.dpaw.wa.gov.au/dataset/bda97642-6377-40b7-89dd-85a599204466/resource/f7d03d06-78a1-4597-a3e5-164caa5554d3/download/simpinsitutemp.csv"
filename <- "datasimpinsitutemp.csv"
mincol<- 2; maxcol <- 4

# SBMP
url <- "http://internal-data.dpaw.wa.gov.au/dataset/6db69777-4b5e-45f2-82e7-22934b87cb5c/resource/1a1fce01-2c09-4dee-971a-487fca3d166f/download/sbmpainsitutemp.csv"
filename <- "datasbmpinsitutemp.csv"
mincol<- 2; maxcol <- 6
