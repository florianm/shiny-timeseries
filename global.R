#' Library imports, helper functions, global variables

library(shiny)

# rendering
require(markdown) || install.packages("markdown")

# plotting
require(ggplot2) || install.packages("ggplot2")
require(qcc) || install.packages("qcc")
require(scales) || install.packages("scales")

# web
require(RCurl) || install.packages("RCurl")
require(rjson) || install.packages("rjson")

# utilities
require(lubridate) || install.packages("lubridate")
# require(tidyr) || install.packages("tidyr")
# require(dplyr) || install.packages("dplyr")


#------------------------------------------------------------------------------#
# Data loading

#' Load CSV data from a URL and guess variable classes
#'
#' Reads numbers as numeric
#' Reads strings as date if matching an ISO8601 date format, else as factor
#' Will result in either numeric, POSIXct, or string/factor classes
#'
#' Date is parsed with lubridate::parse_date_time
#'
#' @param url A valid URL to a CSV file
#' @param ldo Lubridate date orders, default: "YmdHMSz", "YmdHMS","Ymd","dmY"
#' @param ltz Lubridate time zone, default: "Australia/Perth"
get_data <- function(url,
                     ldo=c("YmdHMSz", "YmdHMS","Ymd","dmY"),
                     ltz="Australia/Perth"){
  as.data.frame(
    lapply(
      read.table(url, sep=',', header=T, stringsAsFactors=T),
      function(x) {
        if(is.factor(x)){
            # insert test whether factor x really is a date
            x <- lubridate::parse_date_time(x, orders=ldo, tz=ltz)
        }
        x
      }))
}

#' From CKAN package$resources loaded as R list, return items matching format_string
#'
#' @param res_list A CKAN package$resources JSON dict, loaded as R list
#' @param format_string The desired format, e.g. "CSV", "PDF", "TXT"
resources_format_filter <- function(res_list, format_string){
  Filter(
    function(res_list){
      length(res_list)>0 && res_list[["format"]] == format_string
    },
    res_list
  )
}

#------------------------------------------------------------------------------#
# The GGplot2 theme for MPA graphs
et14 <- element_text(size=14)
mpa_theme <- theme(axis.text.x = et14,
                   axis.text.y = et14,
                   axis.title.x= et14,
                   axis.title.y= et14,
                   plot.title = element_text(lineheight=.8, face="bold"),
                   #axis.line=element_line(colour="black"),
                   #panel.grid.minor = element_blank(),
                   #panel.grid.major = element_blank(),
                   #panel.border=element_blank(),
                   #panel.background=element_blank(),
                   legend.justification=c(1,10),
                   legend.position=c(1,10),
                   legend.title = element_blank(),
                   legend.key = element_blank())

mpa_theme_text <- paste(
  "  theme(",
  "    axis.text.x = element_text(size=14),",
  "    axis.text.y = element_text(size=14),",
  "    axis.title.x=element_text(size=14),",
  "    axis.title.y=element_text(size=14),",
  #   "    axis.line=element_line(colour='black'),",
  #   "    panel.grid.minor = element_blank(),",
  #   "    panel.grid.major = element_blank(),",
  #   "    panel.border=element_blank(),",
  #   "    panel.background=element_blank(),",
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
