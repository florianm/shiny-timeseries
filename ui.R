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
          uiOutput("qcc"),
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
        tabsetPanel(type = "tabs",
                    tabPanel("Inspect",
                             verbatimTextOutput("overview"),
                             verbatimTextOutput("summary"),
                             dataTableOutput("table")
                    ),
                    tabPanel("Plot",
                             plotOutput("plot_ggplot"),
                             verbatimTextOutput("rcode")
                    ),
                    tabPanel("Learn more",
                             includeMarkdown("instructions.md")
                    )
        ) # /tabsetPanel
      ) # /mainPanel
    ) # /sidebarLayout
  ) # /fluidPage
) # /shinyUI
