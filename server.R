shinyServer(function(input, output) {

  # Data source: CSV URL e.g. from CKAN
  data <- reactive({
    infile <- input$csv_url
    if (is.null(infile)) { return(NULL) }
    read.table(input$csv_url, sep=",", header=T, stringsAsFactors=T)
  })

  # Define data column for x axis, y axis, groups
  output$xcol <- renderUI({
    df <-data()
    if (is.null(df)) return(NULL)
    items=names(df)
    names(items)=items
    selectInput("xcol", "Select X Variable", items)
  })

  output$ycol <- renderUI({
    df <-data()
    if (is.null(df)) return(NULL)
    items=names(df)
    names(items)=items
    selectInput("ycol", "Select Y Variable", items)
  })

  output$has_groups <- renderUI({
    checkboxInput(inputId = "has_groups",
                  label = strong("Group data by a factor"),
                  value = FALSE)
  })

  output$gcol <- renderUI({
    df <-data()
    if (is.null(df)) return(NULL)
    items=names(df)
    names(items)=items
    conditionalPanel(condition = "input.has_groups == true",

                     selectInput("gcol", "Select Group Variable", items)
    )
  })

  output$plot_pd <- renderUI({
    sliderInput(inputId = "plot_pd",
                label = "Position dodge",
                min = 0, max = 1, value = 0.25, step = 0.05)
  })

  # Reactive data, summary, table
  output$summary <- renderPrint({ summary(data()) })
  output$table <- renderDataTable({ data() })


  pd = reactive ({ ggplot2::position_dodge(input$pd) })

  # Plot
  output$plot <- renderPlot({
    df <-data()
#     if (is.null(df)) return(NULL)

# Option 1 - plot()
    plot(df)

# Option 2: ggplot2
#     ggplot(df, aes(x=input$xcol, y=input$ycol)) +
#       geom_line(position=pd()) +
#       geom_point(position=pd(), size=3) +
#       xlab("Year") +
#       ylab("Average weight (g)") +
#       scale_x_continuous(limits=c(min(df$year-0.15), max(df$year+0.15)),
#                          breaks=min(df$year):max(df$year)) +
#       mpa_theme

#     if (input$individual_obs) { }

# Option 3: qcc

  })

})
