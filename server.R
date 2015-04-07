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
    selectInput("ckan_package",
                "Choose dataset (select or type)",
                items)
  })

  package_dict <- reactive({
    ckan_json(api_call="package_show", oid=input$ckan_package)
  })

  # Get resources of CKAN once as dict
  resource_dict <- reactive ({
    d <- package_dict()
    if (is.null(d)) return(NULL)
    d$resources
  })

  # Let user select CSV from resources
  output$ckan_csv <- renderUI({
    r <- resource_dict()
    if (is.null(r)) return(NULL)
    selectInput("ckan_csv",
                "Choose CSV resource to load data from",
                res2nl(r, "CSV"))
  })

  # Let user select PDF resource to overwrite with new figure
  output$ckan_pdf <- renderUI({
    r <- resource_dict()
    if (is.null(r)) return(NULL)
    selectInput("ckan_pdf",
                "Choose PDF resource to overwrite with figure",
                res2nl(r, "PDF"))
  })

  # Let user select R code resource to overwrite with R code for figure
  output$ckan_r <- renderUI({
    r <- resource_dict()
    if (is.null(r)) return(NULL)
    selectInput("ckan_r",
                "Choose text resource to overwrite with R code",
                res2nl(r, "TXT"))
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
  #     datavars <- reactive({
  #       df <- data()
  #       if (is.null(df)) return(NULL)
  #       x <- names(df)
  #       items=setNames(x,x)
  #       items
  #     })

  # Date variables
  datevars <- reactive({
    df <- data()
    if (is.null(df)) return(NULL)
    # urge to scrape out eyes with rusty spoon intensifies...
    x = names(Filter(function(x){length(x)==2 && x[[1]]=="POSIXct"},
                     setNames(sapply(df, class), names(df))))
    setNames(x,x)
  })

  # Numeric variables
  numericvars <- reactive({
    df <- data()
    if (is.null(df)) return(NULL)
    x = names(Filter(function(x){length(x)==1 && x[[1]]=="numeric"},
                     setNames(sapply(df, class), names(df))))
    setNames(x,x)
  })

  # Factor variables
  factorvars <- reactive({
    df <- data()
    if (is.null(df)) return(NULL)
    x = names(Filter(function(x){length(x)==1 && x[[1]]=="factor"},
                     setNames(sapply(df, class), names(df))))
    setNames(x,x)
  })

  # Let user assign any numeric variable to Y axis
  output$ycol <- renderUI({
    selectInput("ycol", "Choose Y variable", numericvars())
  })

  # Let user assign any date variable to X axis
  output$xcol <- renderUI({
    selectInput("xcol", "Choose X variable", datevars())
  })

  # Let user choose whether to draw multiple data series
  output$has_groups <- renderUI({
      checkboxInput(inputId = "has_groups",
                    value = FALSE,
                    label = strong("Group data by a factor"))
  })

  # Let user select factor variable for multiple data series
  output$gcol <- renderUI({
    conditionalPanel(condition = "input.has_groups == true",
      selectInput("gcol", "Choose grouping variable", factorvars())
    )
  })
  #
  # Figure is ready to plot

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
  output$number_smooth_points <- renderUI({
    sliderInput(inputId = "number_smooth_points",
                label = "Smoothing window (number of data points for moving average)",
                min = 0, max = 200, value = 80, step = 5)
  })

  output$legend_title <- renderUI({ textInput("legend_title", "Legend Title") })
  output$legend_position <- renderUI({
    selectInput("legend_position",
                "Legend Position",
                c("Right"="right",
                  "Bottom"="bottom",
                  "Left"="left",
                  "Top"="top"))
  })
  output$label_font_size <- renderUI({
    sliderInput(inputId = "label_font_size", label = "Label font size",
                min = 6, max = 24, value = 14, step = 1)
  })

  # End UI elements for plotting parameters -----------------------------------#

  #----------------------------------------------------------------------------#
  # output objects: data summary and table
  output$overview <- renderPrint({str(data())}, width=120)
  output$summary <- renderPrint({summary(data())}, width=120)
  output$table <- renderDataTable({data()}, options=list(pageLength=10))

  # output object: ggplot -----------------------------------------------------#
  # The GGplot2 theme for MPA graphs
  text_pars <- reactive({element_text(size=input$label_font_size)})
  title_pars <- reactive({element_text(lineheight=.8, face="bold")})
  mpa_theme <- reactive({
    theme(axis.text.x = text_pars(),
          axis.text.y = text_pars(),
          axis.title.x= text_pars(),
          axis.title.y= text_pars(),
          plot.title = title_pars(),
          legend.position=input$legend_position
    )
  })

  mpa_theme_text <- reactive({
    paste0(
    "  theme(\n",
    "    axis.text.x = element_text(size=", input$label_font_size, "),\n",
    "    axis.text.y = element_text(size=", input$label_font_size, "),\n",
    "    axis.title.x = element_text(size=", input$label_font_size, "),\n",
    "    axis.title.y = element_text(size=", input$label_font_size, "),\n",
    "    legend.position = '",input$legend_position,"'\n",
    "  )\n"
  )
  })

  plot_ggplot <- reactive({
    df <-data()

    # Parameters
    positiondodge <- position_dodge(input$pd)
    pointsize <- 2
    dateformat <- "%Y-%m"
    datebreaks <- "1 year"
    dateminorbreaks <- "3 months"
    simple_aes <- aes_string(x=input$xcol, y=input$ycol)

    # Multiple or single data series
    if (!is.null(input$gcol) && input$has_groups == TRUE) {
      # Split data by factor
      geom_point_text <- geom_point(
        aes_string(x=input$xcol, y=input$ycol,
                   group=input$gcol, shape=input$gcol, col=input$gcol),
        position=positiondodge,
        size=pointsize
        )
      geom_line_text <- geom_line(
        aes_string(x=input$xcol, y=input$ycol,
                   group=input$gcol, shape=input$gcol, col=input$gcol),
        position=positiondodge
      )
    } else {
      # Plot all data
      geom_point_text <- geom_point(position=positiondodge, size=pointsize)
      geom_line_text <- geom_line(position=positiondodge)
    }

    # Main plot object
    ggplt <- ggplot(df, simple_aes) +
      geom_line_text +
      geom_point_text +
      labs(title=input$title, x=input$x_label, y=input$y_label) +
      scale_x_datetime(labels=date_format(dateformat),
                       breaks=date_breaks(datebreaks),
                       minor_breaks=dateminorbreaks) +
      mpa_theme()

    # Optional moving average
    if (input$add_moving_average == T){
      ggplt <- ggplt + geom_smooth(simple_aes, size=pointsize, n=input$number_smooth_points)
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

    # Reusable code fragment --------------------------------------------------#
    aesthetic <- paste0("aes(x=", input$xcol, ", y=", input$ycol, ")")
    pd <- paste0("position=position_dodge(", input$pd,")")

    # Optional moving average
    if (input$add_moving_average == T){
      geom_smooth_text <- paste0("  geom_smooth(", aesthetic ,", size=2, n=",
                                 input$number_smooth_points, ") +\n")
    } else {geom_smooth_text <- ""}

    # Multiple or single data series
    if (!is.null(input$gcol) && input$has_groups == TRUE) {
      aes_grp <- paste0("aes(x=", input$xcol, ", y=", input$ycol,
        ", group=", input$gcol, ", shape=", input$gcol, ", col=", input$gcol, ")")
      geom_point_text <- paste0("geom_point(", aes_grp, ", ", pd,", size=2) +\n")
      geom_line_text <- paste0("geom_line(", aes_grp, ",\n", pd, ") +\n")
    } else {
      geom_point_text <- paste0("geom_point(", pd,", size=2) +\n")
      geom_line_text <- paste0("geom_line(", pd,") +\n")
    }

    # Final code output -------------------------------------------------------#
    paste0(
      text_instruction(),
      "require(ggplot2) || install.packages('ggplot2')\n",
      "require(lubridate) || install.packages('lubridate')\n",
      "require(scales) || install.packages('scales')\n\n",
      "csv_url <- '", input$ckan_csv, "'\n",
      "df <-read.table(csv_url, sep=',', header=T, stringsAsFactors=T)\n",
      "# Convert only columns called 'date' or 'Date' into POSIXct dates\n",
      "cn <- names(df)\n",
      "dcn <- c('date', 'Date') # Date column names\n",
      "df[cn %in% dcn] <- lapply(\n",
      "  df[cn %in% dcn],\n",
      "  function(x){\n",
      "    x<- lubridate::parse_date_time(",
      "x, orders=c('YmdHMSz', 'YmdHMS','Ymd','dmY'), tz='Australia/Perth')\n",
      "  }\n",
      ")\n\n",

      "pdf('", input$output_filename,".pdf', height = 5, width = 7)\n\n",

      "ggplot(df, ", aesthetic, ") +\n",
      geom_point_text,
      geom_line_text,
      "  labs(title='",input$title,"', x='",input$x_label,"', y='",input$y_label,"') +\n",
      "  scale_x_datetime(labels=date_format('%Y-%m'), breaks='1 year', minor_breaks='3 months') +\n",
      geom_smooth_text,
      mpa_theme_text(),

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
