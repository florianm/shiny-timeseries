library(shiny)
require(ggplot2) || install.packages("ggplot2")
require(markdown) || install.packages("markdown")
require(lubridate) || install.packages("lubridate")
require(qcc) || install.packages("qcc")
require(RCurl) || install.packages("RCurl")
require(scales) || install.packages("scales")
require(rjson) || install.packages("rjson")

#------------------------------------------------------------------------------#
# Data loading

# Selected time series datasets
test_datasets <- c(
  "SIMP In Situ Water Temp" = "http://internal-data.dpaw.wa.gov.au/dataset/bda97642-6377-40b7-89dd-85a599204466/resource/f7d03d06-78a1-4597-a3e5-164caa5554d3/download/simpinsitutemp.csv",
  "JMP In Situ Water Temp" = "http://internal-data.dpaw.wa.gov.au/dataset/82992b4c-18df-4282-a290-d5fac9a53171/resource/ac8c3854-bc81-4141-a0a9-acfdfb6fdbed/download/aimsnambungbaywatertemperature11may2012to16feb2015.csv",
  "WNIMP Black Bream avg weight" = "http://internal-data.dpaw.wa.gov.au/dataset/cd754eda-0998-49e0-852a-2ee5d8e3b075/resource/b3a36efe-039b-4a90-96e0-0b90441c14b8/download/wnimpfishingcompetitions.csv",
  "EMBMP Rainfall" = "http://internal-data.dpaw.wa.gov.au/dataset/0f86add2-eb6d-4ba9-bd9d-2f5590c005cd/resource/4ed57506-12c7-4702-a11f-fc0c90bf0d16/download/IDCJAC0001004068Data1.csv"
)

# Possible date formats and timezone for lubridate::parse_date_time
ldo <- c("YmdHMSz", "YmdHMS","Ymd","dmY")
ldz <- "Australia/Perth"


#------------------------------------------------------------------------------#
# The GGplot2 theme for MPA graphs
mpa_theme <- theme(axis.text.x = element_text(size=14),
                   axis.text.y = element_text(size=14),
                   axis.title.x=element_text(size=14), # or element_blank(),
                   axis.title.y=element_text(size=14),
                   plot.title = element_text(lineheight=.8, face="bold"),
                   #axis.line=element_line(colour="black"),
                   #panel.grid.minor = element_blank(),
                   #panel.grid.major = element_blank(),
                   #panel.border=element_blank(),
                   #panel.background=element_blank(),
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

#------------------------------------------------------------------------------#
# CKAN API helpers

#' Return a CKAN API call as JSON dict
#'
#' Sends a request to a CKAN API, e.g.:
#' http://my.ckan.instance.com/api/3/action/package_show?id=MY-DATASET-ID-abcdef-12345
#'
#' @param oid the alphanumeric CKAN object (here: dataset) ID or slug, default: ""
#' @param base_url the base url of the CKAN instance,
#'   default: "http://internal-data.dpaw.wa.gov.au/"
#' @param api_call the path to the API function, default: "package_show"
#' @param debug a flag to toggle debug output to the console, default: FALSE
#' @return a JSON dict of dataset details (name, notes, tags etc.)
#' @import rjson
#' @export
ckan_json <- function(
  base_url="http://internal-data.dpaw.wa.gov.au/",
  api_call="package_show",
  oid='',
  debug=FALSE){

  url = paste(base_url, "api/3/action/", api_call, "?id=", oid, sep="")
  if (debug) {print(paste("CKAN API URL to be called:", url))}

  if (url.exists(base_url)) {
    out <- rjson::fromJSON(getURLContent(url), unexpected.escape="skip")$result
  } else {
    out <- NULL
  }

  out
}
