library(shiny)
library(markdown)
shinyUI(
  navbarPage(
    "Time series explorer",
    tabPanel(
      "Load Data",
      fluidRow(
        column(3,
               wellPanel(
                 h3("Load Data"),
                 selectizeInput('ckan_url', 'Select catalogue', choices = ckan_instances),
                 textInput("api_key", "Paste your CKAN API key"),
                 textInput("csv_url", "Paste CSV data resource URL"),
                 textInput("pdf_url", "Paste PDF graph resource URL")
               ) # /wellPanel
        ), # /col3
        column(9,
               p("Inspect data selection")
        ) # /col9
      ) # /fluidRow
    ) # /tabPanel

  )) # /navbar, app

