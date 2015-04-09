shinyUI(
  fluidPage(
    titlePanel("Time series explorer"),
    sidebarLayout(
      sidebarPanel(

        wellPanel(
          h4("Load Data"),
          uiOutput("ckan_package"),
          uiOutput("ckan_csv"),
          uiOutput("varmap")
        ), # /wellPanel

        wellPanel(
          h4("Plot Data"),
          uiOutput("plot_x_range"),
          uiOutput("plot_labels"),
          uiOutput("moving_average"),
          uiOutput("hline"),
          uiOutput("legend")
        ), # /wellPanel

        wellPanel(
          h4("Save Products"),
          uiOutput("save2disk"),
          uiOutput("push2ckan")
        ) # /wellPanel

      ), # /sidebarPanel
      mainPanel(
#         progressInit(),
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
