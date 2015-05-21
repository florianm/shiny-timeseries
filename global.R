#' Library imports, helper functions, global variables

library(shiny)
# require(shinyIncubator) || install.packages("shinyIncubator")

# rendering
require(markdown) || install.packages("markdown")
require(whisker) || install.packages("whisker")
require(Hmisc) || install.packages("Hmisc")


# plotting
require(ggplot2) || install.packages("ggplot2")
require(qcc) || install.packages("qcc")
require(scales) || install.packages("scales")

# web
require(RCurl) || install.packages("RCurl")
require(rjson) || install.packages("rjson")

# utilities
require(lubridate) || install.packages("lubridate")
require(tidyr) || install.packages("tidyr")
require(dplyr) || install.packages("dplyr")


# CKAN API
require(devtools) || install.packages("devtools")
require(ckanr) || devtools::install_github("ropensci/ckanr")

#------------------------------------------------------------------------------#
# ckanR setup

# Constants
CKAN_URL = "http://data-demo.dpaw.wa.gov.au/"
set_ckanr_url(CKAN_URL)

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
                     dcn=c("date", "Date", "date.start", "date.end")
){
  df <- read.table(url, sep=',', header=T, stringsAsFactors=T)

  ## Alternative
  #   df<- cbind(
  #     lapply(select(df, matches("[Dd]ate")),
  #       function(x){x<- lubridate::parse_date_time(x, orders=ldo, tz=ltz)}),
  #     select(df, -matches("[Dd]ate")))

  cn <- names(df)
  df[cn %in% dcn] <- lapply(
    df[cn %in% dcn],
    function(x){x<- lubridate::parse_date_time(x, orders=ldo, tz=ltz)}
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
  base_url=CKAN_URL,
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


# Upload to CKAN
#   print("Uploading report PDFs to {0}dataset/mpa-reports".format(DC))
#   [resource_update(d, r["resid"], r["file"], api_key=CKAN) for r in REPORTS]
#
#   set_last_updated_fields(p, api_key=CKAN, lub=os.environ["LOGNAME"],
#                           luo=datetime.now().strftime("%Y-%m-%d %H:%M:%S"))

# res = resource_show(res_id)
# res["state"] = "active"
# res["last_modified"] = datetime.now().strftime("%Y-%m-%d %H:%M:%S")

# if os.path.isfile(os.path.join(filedir, filepath)):
#   r = requests.post("{0}resource_update".format(api_url),
#                     #data={"id": res_id},
#                     data=res,
#                     headers={"Authorization": api_key},
#                     files=[('upload', file(os.path.join(filedir, filepath)))])
# print("Uploaded {0}".format(filepath))
# else:
#   print("File {0} not found, skipping upload".format(filepath))


# r = requests.get("{0}resource_show?id={1}".format(api_url, resource_id))
# if r.status_code == 200:
#   return json.loads(r.content)["result"]
# else:
#   return None


