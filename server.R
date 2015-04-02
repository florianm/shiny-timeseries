source("global.R")
shinyServer(function(input, output) {

  #----------------------------------------------------------------------------#
  # Select data

  # Query CKAN for packages with tag "format_csv_ts"
  output$ckan_package <- renderUI({
    d <- ckan_json(api_call="tag_show", oid="format_csv_ts")
    if (is.null(d)) return(NULL)
    items <- setNames(
      lapply(d$packages, function(x){x$id}),
      lapply(d$packages, function(x){x$title}))
    selectInput("ckan_package", "Choose dataset", items)
  })

  # Get resources of CKAN once as dict
  resource_dict <- reactive ({
    d <- ckan_json(api_call="package_show", oid=input$ckan_package)
    if (is.null(d)) return(NULL)
    d$resources
  })

  # Let user select CSV from resources
  output$ckan_csv <- renderUI({
    rr <- resources_format_filter(resource_dict(), "CSV")
    i <- setNames(lapply(rr, function(x){x$url}), lapply(rr, function(x){x$name}))
    selectInput("ckan_csv", "Choose CSV data resource", i)
  })

  # Let user select PDF resource to overwrite with new figure
  output$ckan_pdf <- renderUI({
    rr <- resources_format_filter(resource_dict(), "PDF")
    i <- setNames(lapply(rr, function(x){x$url}), lapply(rr, function(x){x$name}))
    selectInput("ckan_pdf", "Choose PDF graph resource", i)
  })

  # Let user select R code resource to overwrite with R code for figure
  output$ckan_r <- renderUI({
    rr <- resources_format_filter(resource_dict(), "TXT")
    i <- setNames(lapply(rr, function(x){x$url}), lapply(rr, function(x){x$name}))
    selectInput("ckan_r", "Choose R script resource", i)
  })

  # Load data from selected CSV resource, detect date formats
  data <- reactive({
    if (is.null(input$ckan_csv)) return(NULL)
    get_data(input$ckan_csv)
    })
  # data is now loaded
  #----------------------------------------------------------------------------#

  #----------------------------------------------------------------------------#
  # Inspect and select data to plot
  #
  # Get data columns as named list, all or filtered by class
    datavars <- reactive({
      df <- data()
      if (is.null(df)) return(NULL)
      x <- names(df)
      items=setNames(x,x)
      items
    })

  datevars <- reactive({
    df <- data()
    if (is.null(df)) return(NULL)
    # urge to scrape out eyes with rusty spoon intensifies...
    x = names(Filter(function(x){length(x)==2 && x[[1]]=="POSIXct"},
                     setNames(sapply(df, class), names(df))))
    setNames(x,x)
  })

  numericvars <- reactive({
    df <- data()
    if (is.null(df)) return(NULL)
    x = names(Filter(function(x){length(x)==1 && x[[1]]=="numeric"},
                     setNames(sapply(df, class), names(df))))
    setNames(x,x)
  })

  factorvars <- reactive({
    df <- data()
    if (is.null(df)) return(NULL)
    x <- names(df[,sapply(df, is.factor)])
    setNames(x,x)
  })

  # Let user select responding variable for y axis
  output$ycol <- renderUI({
    selectInput("ycol", "Choose Y variable", numericvars())
  })

  # Let user select independent variable for x axis
  output$xcol <- renderUI({
    selectInput("xcol", "Choose X variable", datevars())
  })

  # Let user choose whether to draw multiple data series
  output$has_groups <- renderUI({
    checkboxInput(inputId = "has_groups", value = FALSE,
                  label = strong("Group data by a factor"))
  })

  # Let user select factor variable for multiple data series
  output$gcol <- renderUI({
    conditionalPanel(condition = "input.has_groups == true",
                     selectInput("gcol", "Choose grouping variable", factorvars())
    )
  })

  #----------------------------------------------------------------------------#
  # Plotting parameters
  #
  # User submitted parameters for plot
  output$plot_title <- renderUI({ textInput("title", "Figure title") })
  output$plot_ylab <- renderUI({ textInput("y_label", "Y axis label") })
  output$plot_xlab <- renderUI({ textInput("x_label", "X axis label") })
  output$plot_x_breaks <- renderUI({
    sliderInput(inputId = "max_x_breaks", label = "Number of X axis breaks",
                min = 0, max = 20, value = 10, step = 1)
  })
  output$plot_pd <- renderUI({
    sliderInput(inputId = "pd", label = "Position dodge",
                min = 0, max = 1, value = 0, step = 0.05)
  })
  output$plot_x_extra <- renderUI({
    sliderInput(inputId = "x_extra", label = "X scale padding",
                min = 0, max = 1, value = 0.15, step = 0.05)
  })
  output$add_moving_average <- renderUI({
    checkboxInput(inputId = "add_moving_average",
                  label = strong("Add moving average"),
                  value = FALSE)
  })
  # End UI elements------------------------------------------------------------#

  # output object: data summary
  output$overview <- renderPrint({str(data())}, width=120)
  output$summary <- renderPrint({summary(data())}, width=120)

  # output object: data table
  output$table <- renderDataTable({data()}, options=list(pageLength=10))

  # object: ggplot
  plot_ggplot <- reactive({
    df <-data()

    # Parameters
    positiondodge <- position_dodge(input$pd)
    pointsize <- 2
    dateformat <- "%Y-%m"
    datebreaks <- "1 year"
    dateminorbreaks <- "3 months"

    # Multiple or single data series
    if (!is.null(input$gcol) && input$has_groups == TRUE) {
      aesthetic <- aes_string(x=input$xcol, y=input$ycol,
                              group=input$gcol, shape=input$gcol)
    } else {
      aesthetic <- aes_string(x=input$xcol, y=input$ycol)
    }

    # Main plot object
    ggplt <- ggplot(df, aesthetic) +
      geom_line(position=positiondodge) +
      geom_point(position=positiondodge, size=pointsize) +
      labs(title=input$title, x=input$x_label, y=input$y_label) +
      scale_x_datetime(labels=date_format(dateformat),
                       breaks=date_breaks(datebreaks),
                       minor_breaks=dateminorbreaks) +
      mpa_theme

    # Optional moving average
    if (input$add_moving_average == T){
      ggplt <- ggplt + geom_smooth(size=pointsize)
    }

    ggplt

  })

  # output object: rendered plot
  output$plot_ggplot <- renderPlot({plot_ggplot()})

  # output object: download PDF
  output$downloadPdf <- downloadHandler(
    filename = function() {paste0(input$output_filename, '.pdf')},
    content = function(file) {
      pdf(file, height = 5, width = 7);
      print(plot_ggplot());
      dev.off()
    }
  )

  # Output object: instruction dependent on valid ckan_r url
  text_instruction <- reactive({
    if (is.null(input$ckan_r) ||
          input$ckan_r == "" ||
          !url.exists(input$ckan_r)) { return(NULL) }
    paste0("## Reproduce the figure:\n# source('", input$ckan_r, "')\n\n")
  })

  # R code spelled out
  plot_code <- reactive({
    if (input$add_moving_average == T){
      smooth_text <- "  geom_smooth(size=2) +\n"
    } else {smooth_text <- ""}

    paste0(
      text_instruction(),
      "require(ggplot2) || install.packages('ggplot2')\n",
      "require(lubridate) || install.packages('lubridate')\n",
      "require(scales) || install.packages('scales')\n",
      "df <- as.data.frame(lapply(\n  read.table('",
      input$ckan_csv, "', sep=',', header=T, stringsAsFactors=T),\n",
      "  function(x) {if(is.factor(x)){x <- lubridate::parse_date_time(",
      "x, c('YmdHMSz', 'YmdHMS','Ymd','dmY'), tz='Australia/Perth')};x}))\n\n",
      "pdf('", input$output_filename,".pdf', height = 5, width = 7);\n",
      "ggplot(df, aes_string(x='", input$xcol, "', y='", input$ycol, "')) +\n",
      "  geom_line(position=position_dodge(", input$pd,")) +\n",
      "  geom_point(position=position_dodge(", input$pd,"), size=2) +\n",
      "  labs(title='",input$title,"', x='",input$x_label,"', y='",input$y_label,"') +\n",
      "  scale_x_datetime(labels=date_format('%Y-%m'), breaks='1 year', minor_breaks='3 months') +\n",
      smooth_text,
      mpa_theme_text,
      "\ndev.off()\n"
    ) # /paste
  }) #/reactive

  # output object: rendered R code
  output$rcode <- renderText({plot_code()})

  # output object: download R code
  output$downloadCode <- downloadHandler(
    filename = function() {paste0(input$output_filename, '.txt')},
    content = function(file) {writeLines(plot_code(), file)}
  )

})
