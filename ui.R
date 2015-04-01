shinyUI(
  fluidPage(
    titlePanel("Time series explorer"),
    sidebarLayout(
      sidebarPanel(

        wellPanel(
          h4("Load Data"),
          uiOutput("ckan_package"),
          uiOutput("ckan_csv"),
          uiOutput("ycol"),
          uiOutput("xcol"),
          uiOutput("has_groups"),
          uiOutput("gcol")
        ), # /wellPanel

        wellPanel(
          h4("Plot Data"),
          uiOutput("plot_title"),
          uiOutput("plot_ylab"),
          uiOutput("plot_xlab"),
#           uiOutput("plot_x_breaks"),
#           uiOutput("plot_x_extra"),
          uiOutput("plot_pd")
        ), # /wellPanel

        wellPanel(
          h4("Save Figure"),
          textInput("api_key", "Paste CKAN API key"),
          uiOutput("ckan_pdf"),
          uiOutput("ckan_r"),
          textInput("output_filename", "File name", value="figure"),
          downloadButton("downloadPdf", "Download PDF"),
          downloadButton("downloadCode", "Download R Code")
        ) # /wellPanel

      ), # /sidebarPanel
      mainPanel(

        tabsetPanel(type = "tabs",

                    tabPanel("Inspect",
                             p("Which columns would you like to plot?"),
                             p(paste0("The X axis should be a valid datetime or",
                                      " date. Supported ISO 8601 date formats ",
                                      "are: Y-m-d (H:M:S (z)).")),
                             verbatimTextOutput("overview"),
                             verbatimTextOutput("summary")
                    ),

                    tabPanel("Preview",
                             p("Preview the actual data."),
                             dataTableOutput("table")
                    ),

                    tabPanel("Visualise",
                             plotOutput("plot_ggplot"),
                             verbatimTextOutput("rcode")

                    )
        ) # /tabsetPanel

      ) # /mainPanel
    ) # /sidebarLayout
  ) # /fluidPage
) # /shinyUI
