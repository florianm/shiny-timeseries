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
          uiOutput("plot_pd"),
          uiOutput("add_moving_average"),
          uiOutput("number_smooth_points"),
          uiOutput("legend_title"),
          uiOutput("legend_position"),
          uiOutput("label_font_size")

        ), # /wellPanel

        wellPanel(
          h4("Save Figure"),
          textInput("api_key", "Paste your own CKAN API key"),
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
                             h1("Select columns to plot"),
                             p("The Y axis must be a numeric variable."),
                             p(paste0("The X axis must be a valid datetime or",
                                      " date. Supported ISO 8601 date formats ",
                                      "are: Y-m-d (H:M:S (z)).")),
                             p("Have a look at the data below, and choose Y and X variables accordingly."),
                             verbatimTextOutput("overview"),
                             verbatimTextOutput("summary"),
                             dataTableOutput("table")
                    ),

                    tabPanel("Visualise",
                             plotOutput("plot_ggplot"),
                             verbatimTextOutput("rcode")

                    ),

                    tabPanel("Help",
                             includeMarkdown("instructions.md")
                    )

        ) # /tabsetPanel

      ) # /mainPanel
    ) # /sidebarLayout
  ) # /fluidPage
) # /shinyUI
