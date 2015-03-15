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
  output$gcol <- renderUI({
    df <-data()
    if (is.null(df)) return(NULL)
    items=names(df)
    names(items)=items
    selectInput("gcol", "Select Group Variable", items)
  })

  # Reactive data, summary, table
  output$data <- data()
  output$summary <- renderPrint({ summary(data()) })
  output$table <- renderDataTable({ data() })



})
