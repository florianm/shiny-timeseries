shinyUI(
  fluidPage(
    titlePanel("Time series explorer"),
    sidebarLayout(
      sidebarPanel(


        wellPanel(
          h4("Load Data"),
#           textInput("csv_url", "Paste CSV URL"),
          selectizeInput('csv_url', 'Select dataset', choices = test_datasets),
          uiOutput("xcol"),
          uiOutput("ycol"),
          uiOutput("has_groups"),
          uiOutput("gcol")
        ), # /wellPanel

        wellPanel(
          h4("Plot Data"),
          uiOutput("plot_pd")
        ), # /wellPanel

        wellPanel(
          h4("Save Figure"),
          textInput("api_key", "Paste CKAN API key"),
          textInput("pdf_url", "Paste PDF URL")
        ) # /wellPanel

      ), # /sidebarPanel
      mainPanel(

        tabsetPanel(type = "tabs",
                    tabPanel("Summary",
                             verbatimTextOutput("summary"),
                             dataTableOutput("table")),
                    tabPanel("Figure", plotOutput("plot"))
        ) # /tabsetPanel

      ) # /mainPanel
    ) # /sidebarLayout
  ) # /fluidPage
) # /shinyUI
