#' Imports, config, global functions
library(shiny)
require(shinyIncubator)
require(markdown)
require(whisker)
require(scales)
require(Hmisc)
require(plyr)
require(dplyr)
require(tidyr)
require(lubridate)
require(ggplot2)
require(qcc)
require(RCurl)
require(ckanr)

ckanr::ckanr_setup(url = "http://internal-data.dpaw.wa.gov.au/")

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
                     ldo = c("YmdHMSz", "YmdHMS","Ymd","dmY"),
                     ltz = "Australia/Perth",
                     dcn = c("date", "Date", "date.start", "date.end")
){
  df <- read.table(url, sep = ',', header = T, stringsAsFactors = T)
  #   df<- cbind(
  #     lapply(select(df, matches("[Dd]ate")),
  #       function(x){x<- lubridate::parse_date_time(x, orders = ldo, tz = ltz)}),
  #     select(df, -matches("[Dd]ate")))
  cn <- names(df)
  df[cn %in% dcn] <- lapply(
    df[cn %in% dcn],
    function(x){x<- lubridate::parse_date_time(x, orders = ldo, tz = ltz)}
  )
  names(df) <- Hmisc::capitalize(names(df))
  df
}


#' Filter a list of lists by a key matching a given value
#'
#' @param lol An R list of lists, e.g. a JSON dict
#' @param key The key to filter by
#' @param val The value to match against
list_filter <- function(lol, key, val){
  Filter(function(lol){length(lol)>0 && lol[[key]] == val}, lol)
}

#' Make named list (name=url) from CKAN resource dict filtered by file type
#'
#' @param resource_dict A CKAN package$resources JSON dict, loaded as R list
#' @filetype_string The file type as string, e.g. "CSV", "PDF", "TXT"
#' @return A named list of CKAN resource names (as keys) and URLs (as values)
res2nl <- function(resource_dict, filetype_string){
  rr <- list_filter(resource_dict, "format", filetype_string)
  i <- setNames(lapply(rr, function(x){x$id}),
                lapply(rr, function(x){x$name}))
}
