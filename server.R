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
    selectInput("ycol", "Select Y Variable", items, selected=items[2])
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

  output$plot_title <- renderUI({ textInput("title", "Figure title") })
  output$plot_ylab <- renderUI({ textInput("y_label", "Y axis label") })
  output$plot_xlab <- renderUI({ textInput("x_label", "X axis label") })
  output$plot_pd <- renderUI({
    sliderInput(inputId = "pd", label = "Position dodge",
                min = 0, max = 1, value = 0.25, step = 0.05)
  })
  output$plot_x_extra <- renderUI({
    sliderInput(inputId = "x_extra", label = "X scale padding",
                min = 0, max = 1, value = 0.15, step = 0.05)
  })



  output$summary <- renderPrint({ summary(data()) })
  output$table <- renderDataTable({ data() })
  output$plot_simple <- renderPlot({
    df <-data()
    plot(df[[input$ycol]] ~ df[[input$xcol]],
         xlab=input$x_label, ylab=input$y_label)
  })

  # Step 1 - reactive plot object
  plot_ggplot <- reactive(function() {
    df <-data()

    x_col <- input$xcol
    y_col <- input$ycol
    x_min <- min(df[[input$xcol]])
    x_max <- max(df[[input$xcol]])
    x_limits <- c(x_min-input$x_extra, x_max+input$x_extra)
    x_breaks <- x_min:x_max
    point_size <- 3
    pd <- position_dodge(input$pd)
    p <- print(
      ggplot(df, aes_string(x=x_col, y=y_col)) +
        geom_line(position=pd) +
        geom_point(position=pd, size=point_size) +
        ylab(input$y_label) +
        xlab(input$x_label) +
        scale_x_continuous(limits=x_limits, breaks=x_breaks) +
        mpa_theme
    )
  })

  # Step 2 - output object: rendered plot
  output$plot_ggplot <- renderPlot({
    plot_ggplot()
  })

  # Step 3 - output object: download PDF
  output$downloadPdf <- downloadHandler(
    filename = function() { paste0(input$pdf_filename, '.pdf') },
    content = function(file) {
      pdf(file, height = 5, width = 7);
      print(plot_ggplot());
      dev.off()
    }
  )



})
