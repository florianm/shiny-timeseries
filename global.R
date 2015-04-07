#' Library imports, helper functions, global variables

library(shiny)

# rendering
require(markdown) || install.packages("markdown")
require(whisker) || install.packages("whisker")

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
#' Reads strings as factor
#' Converts column with names indicating date format ("Date", "date") to POSIXct
#' Will result in either numeric, POSIXct, or string/factor classes
#'
#' Date is parsed with lubridate::parse_date_time
#'
#' @param url A valid URL to a CSV file
#' @param ldo Lubridate date orders, default: "YmdHMSz", "YmdHMS","Ymd","dmY"
#' @param ltz Lubridate time zone, default: "Australia/Perth"
#' @param dcn Date column names, default: "date", "Date"
get_data <- function(url,
                     ldo=c("YmdHMSz", "YmdHMS","Ymd","dmY"),
                     ltz="Australia/Perth",
                     dcn=c("date", "Date")
){
  df <- read.table(url, sep=',', header=T, stringsAsFactors=T)

  # Option 1
  #   df<- cbind(
  #     lapply(select(df, matches("[Dd]ate")),
  #            function(x){x<- lubridate::parse_date_time(x, orders=ldo, tz=ltz)}),
  #     select(df, -matches("[Dd]ate")))

  # Option 2
  cn <- names(df)
  df[cn %in% dcn] <- lapply(
    df[cn %in% dcn],
    function(x){x<- lubridate::parse_date_time(x, orders=ldo, tz=ltz)}
  )

  # Option 3
#   for (i in 1:length(cn)){
#     if (cn[i] %in% dcn){
#       df[cn[i]] <- lubridate::parse_date_time(df[cn[i]], orders=ldo, tz=ltz)}
#   }

  df
}

#' Filter CKAN package$resources loaded as R list by file type
#'
#' @param resource_dict A CKAN package$resources JSON dict, loaded as R list
#' @filetype_string The file type as string, e.g. "CSV", "PDF", "TXT"
#' @return The resource_dict with only those resources matching the file type
resources_format_filter <- function(resource_dict, filetype_string){
  Filter(
    function(resource_dict){
      length(resource_dict)>0 && resource_dict[["format"]] == filetype_string
    },
    resource_dict
  )
}

#' Make named list (name=url) from CKAN resource dict filtered by file type
#'
#' @param resource_dict A CKAN package$resources JSON dict, loaded as R list
#' @filetype_string The file type as string, e.g. "CSV", "PDF", "TXT"
#' @return A named list of CKAN resource names (as keys) and URLs (as values)
res2nl <- function(resource_dict, filetype_string){
  rr <- resources_format_filter(resource_dict, filetype_string)
  i <- setNames(lapply(rr, function(x){x$url}),
                lapply(rr, function(x){x$name}))
}

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
