source("global.R")
shinyServer(function(input, output) {

  #----------------------------------------------------------------------------#
  # Select data
  #
  # Query CKAN for packages with tag "format_csv_ts"
  output$ckan_package <- renderUI({

    d <- ckan_json(api_call="tag_show", oid="format_csv_ts")
    if (is.null(d)) return(NULL)
    items <- setNames(
      lapply(d$packages, function(x){x$id}),
      lapply(d$packages, function(x){x$title}))
    selectInput("ckan_package", "Choose dataset", items)
  })

  # Get resources of CKAN
  ckan_resources <- reactive ({
    d <- ckan_json(api_call="package_show", oid=input$ckan_package)
    if (is.null(d)) return(NULL)
    items <- setNames(
      lapply(d$resources, function(x){x$url}),
      lapply(d$resources, function(x){x$name}))
    items
  })

  # Let user select CSV resource to read data from
  output$ckan_csv <- renderUI({
    selectInput("ckan_csv", "Choose data CSV", ckan_resources())
  })

  # Let user select PDF resource to overwrite with new figure
  output$ckan_pdf <- renderUI({
    selectInput("ckan_pdf", "Choose PDF CSV", ckan_resources())
  })

  # Let user select R code resource to overwrite with R code for figure
  output$ckan_r <- renderUI({
    selectInput("ckan_r", "Choose R CSV", ckan_resources())
  })

  # Load data from selected CSV resource
  data <- reactive({
    if (is.null(input$ckan_csv)) return(NULL)
    d <- as.data.frame(lapply(
      read.csv(input$ckan_csv, sep=",", header=T, stringsAsFactors=T),
      function(x) {if(is.factor(x)){x <- lubridate::parse_date_time(x, ldo, tz=ldz)};x}
    ))
  })
  # data is now loaded
  #----------------------------------------------------------------------------#

  #----------------------------------------------------------------------------#
  # Inspect and select data to plot
  #
  # Get data columns as named list
  datavars <- reactive({
    df <-data()
    if (is.null(df)) return(NULL)
    items=setNames(names(df), names(df))
    items
  })

  # Let user select responding variable for y axis
  output$ycol <- renderUI({selectInput("ycol", "Choose Y variable", datavars())})

  # Let user select independent variable for x axis
  output$xcol <- renderUI({selectInput("xcol", "Choose X variable", datavars())})

  # Let user choose whether to draw multiple data series
  output$has_groups <- renderUI({
    checkboxInput(inputId = "has_groups",
                  label = strong("Group data by a factor"),
                  value = FALSE)
  })

  # Let user select factor variable for multiple data series
  output$gcol <- renderUI({
    conditionalPanel(condition = "input.has_groups == true",
                     selectInput("gcol", "Choose grouping variable", datavars())
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
                min = 0, max = 1, value = 0.25, step = 0.05)
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
  output$table <- renderDataTable({data()}, options=list(iDisplayLength=10))

  # object: ggplot
  plot_ggplot <- reactive({
    df <-data()
    pd <- position_dodge(input$pd)

    ggplt <- ggplot(df, aes_string(x=input$xcol, y=input$ycol)) +
      geom_line(position=pd) +
      geom_point(position=pd, size=2) +
      ggtitle(input$title) +
      ylab(input$y_label) +
      xlab(input$x_label) +
      scale_x_datetime(labels=date_format("%Y-%m"),
                       breaks=date_breaks("1 year"),
                       minor_breaks="3 months") +
      mpa_theme

    if (input$add_moving_average == T){ggplt <- ggplt + geom_smooth(size=2)}

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
      smooth_text <- "geom_smooth(size=2) +\n"
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
      "  ylab('", input$y_label,"') +\n",
      "  xlab('", input$x_label,"') +\n",
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
