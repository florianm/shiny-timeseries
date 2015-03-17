shinyUI(
  fluidPage(
    titlePanel("Time series explorer"),
    sidebarLayout(
      sidebarPanel(

        wellPanel(
          h4("Load Data"),
          # textInput("csv_url", "Paste CSV URL"),
          selectizeInput('csv_url', 'Select dataset', choices = test_datasets),
          uiOutput("xcol"),
          uiOutput("ycol"),
          uiOutput("has_groups"),
          uiOutput("gcol")
        ), # /wellPanel

        wellPanel(
          h4("Plot Data"),
          uiOutput("plot_title"),
          uiOutput("plot_ylab"),
          uiOutput("plot_xlab"),
          uiOutput("plot_pd"),
          uiOutput("plot_x_extra")
        ), # /wellPanel

        wellPanel(
          h4("Save Figure"),
          textInput("api_key", "Paste CKAN API key"),
          textInput("pdf_url", "Paste PDF URL")
        ) # /wellPanel

      ), # /sidebarPanel
      mainPanel(

        tabsetPanel(type = "tabs",

                    tabPanel("Inspect Data",
                             h3("Summary"),
                             p("Which columns would you like to plot?"),
                             verbatimTextOutput("summary"),
                             h3("Preview"),
                             dataTableOutput("table")
                    ),

                    tabPanel("Figure",
                             h3("Simple Plot"),
                             plotOutput("plot_simple"),
                             h3("GGplot2 Figure"),
                             plotOutput("plot_ggplot")
                    )
        ) # /tabsetPanel

      ) # /mainPanel
    ) # /sidebarLayout
  ) # /fluidPage
) # /shinyUI
