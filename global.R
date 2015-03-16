library(shiny)
require(ggplot2) || install.packages("ggplot2")
require(markdown) || install.packages("markdown")
require(lubridate) || install.packages("lubridate")

# Variables that can be put on the x and y axes
axis_vars <- c(
    "Tomato Meter" = "Meter",
    "Numeric Rating" = "Rating",
    "Number of reviews" = "Reviews",
    "Dollars at box office" = "BoxOffice",
    "Year" = "Year",
    "Length (minutes)" = "Runtime"
)

ckan_instances <-  c(
  "DPaW Internal" = "http://internal-data.dpaw.wa.gov.au/",
  "DPaW Sandbox" = "http://test-data.dpaw.wa.gov.au/",
  "DPaW Public" = "http://data-demo.dpaw.wa.gov.au/"
)

