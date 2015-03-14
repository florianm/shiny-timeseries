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
                 h4("Load Data"),
                 textInput("csv_url", "Paste CSV data resource URL"),
                 uiOutput("xcol"),
                 uiOutput("ycol"),
                 uiOutput("gcol")
               ), # /wellPanel
               wellPanel(
                 h4("Save Figure"),
                 textInput("api_key", "Paste your CKAN API key"),
                 textInput("pdf_url", "Paste PDF graph resource URL")
               ) # /wellPanel
        ), # /col3
        column(9,
               p("Inspect data selection"),
               dataTableOutput("table")
        ) # /col9
      ) # /fluidRow
    ) # /tabPanel

  )) # /navbar, app

