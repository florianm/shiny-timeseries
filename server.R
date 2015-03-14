library(shiny)

shinyServer(function(input, output) {

  data <- reactive({
      infile <- input$csv_url
      if (is.null(infile)) { return(NULL) }
      read.table(input$csv_url, sep=",", header=T, stringsAsFactors=T)
    })

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

  output$data <- data()


  output$table <- renderDataTable({
    data()
    })
})
