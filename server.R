source("global.R")
shinyServer(function(input, output) {

  #----------------------------------------------------------------------------#
  # Select data

  datasets <- reactive({
    # http://internal-data.dpaw.wa.gov.au/api/3/action/tag_show?id=format_csv_ts
    x <- ckan_json(api_call="tag_show", oid="format_csv_ts")
    if (is.null(x)) return(NULL)
    x$packages
  })

  # Query CKAN for packages with tag "format_csv_ts"
  output$ckan_package <- renderUI({
    withProgress(message = 'Loading datasets...', value = 0, {
      d <- datasets()
      if (is.null(d)) return(NULL)
      items <- setNames(lapply(d, function(x){x$id}),
                        lapply(d, function(x){x$title}))
      selectInput("ckan_package", "Choose dataset (select or type)", items)
    })
  })

  package_dict <- reactive({
    # ckan_json(api_call="package_show", oid=input$ckan_package)
    list_filter(datasets(), "id", input$ckan_package)[[1]]
  })

  # Let user select CSV from resources
  output$ckan_csv <- renderUI({
    withProgress(message = 'Shlorping data...', value = 0, {
      r <- package_dict()$resources
      if (is.null(r)) return(NULL)
      selectInput("ckan_csv", "Choose CSV resource to load data from",
                  res2nl(r, "CSV"))
    })
  })

  # Save and upload output
  output$push2ckan <- renderUI({
    r <- package_dict()$resources

    if (is.null(r)) return(NULL)
    wellPanel(
      h4("Upload to data catalogue"),
      textInput("api_key", "Paste your own CKAN API key"),
      conditionalPanel(
        condition = "input.api_key != ''",
        selectInput("ckan_pdf", "Choose PDF resource to overwrite with figure",
                    res2nl(r, "PDF")),
        selectInput("ckan_r", "Choose text resource to overwrite with R code",
                    res2nl(r, "TXT")),
        actionButton("goButton", "Upload")
      )
    )
  })

  # Load data from selected CSV resource, detect date formats
  all_data <- reactive({
    x <- input$ckan_csv
    if (is.null(x)) return(NULL)
    d <- get_data(x)
    d
  })

  #   res_dict <- reactive({
  #     r <- package_dict()$resources
  #     if (is.null(r)) return(NULL)
  #     list(
  #     pdf = list_filter(r, "id", input$ckan_pdf)[[1]]
  #     txt = list_filter(r, "id", input$ckan_r)[[1]]
  #     )
  #   })


  # data is now loaded
  #----------------------------------------------------------------------------#

  #----------------------------------------------------------------------------#
  # Inspect and select data to plot

  varlists <- reactive({
    df <- all_data()

    dv <- names(Filter(function(x){length(x)==2 && x[[1]]=="POSIXct"},
                       setNames(sapply(df, class), names(df))))

    nv <- names(Filter(function(x){length(x)==1 && x[[1]]=="numeric"},
                       setNames(sapply(df, class), names(df))))

    fv <- names(Filter(function(x){length(x)==1 && x[[1]]=="factor"},
                       setNames(sapply(df, class), names(df))))
    v <- list(
      "nv" = setNames(nv,nv),
      "dv" = setNames(dv,dv),
      "fv" = setNames(fv,fv)
    )
    v

  })

  output$varmap <- renderUI({
    withProgress(message = 'Fiddling with variables...', value = 0, {

      v <- varlists()
      if (is.null(v)) return(NULL)

      wellPanel(
        selectInput("ycol", "Choose Y variable", v$nv),
        selectInput("xcol", "Choose X variable", v$dv),
        checkboxInput(inputId = "has_groups",
                      value = FALSE,
                      label = strong("Group data by a factor")),
        conditionalPanel(
          condition = "input.has_groups == true",
          selectInput("gcol", "Choose grouping variable", v$fv)),
        checkboxInput(inputId = "subset_data",
                      value = FALSE,
                      label = strong("Subset data")),
        conditionalPanel(
          condition = "input.subset_data == true",
          selectInput("scol", "Choose filter variable", v$fv),
          selectInput("exclude_cases", "Exclude cases", multiple=TRUE,
                      levels(all_data()[[input$scol]])
          )
        )
      )

    })
  })

  data <- reactive({
    d <- all_data()
    if(is.null(d)) return(NULL)

    if(input$subset_data == TRUE) {
      d[which(!d[[input$scol]] %in% input$exclude_cases),]
    } else {
      d
    }

  })

  # Figure is ready to plot

  #----------------------------------------------------------------------------#
  # Plotting parameters
  #
  # User submitted parameters for plot
  output$plot_x_range <- renderUI({
    x_min <- min(data()[[input$xcol]])
    x_max <- max(data()[[input$xcol]])
    dateRangeInput("plot_x_range", "Date range for x axis",
                   start = x_min, end = x_max,
                   min = as.Date("1900-01-01"), max = as.Date("2015-06-30"),
                   format = "yyyy-mm-dd", startview = "month",
                   weekstart = 1, language = "en", separator = " to ")
    #     sliderInput(inputId = "x_extra", label = "X scale padding",
    #                 min = 0, max = 1, value = 0.15, step = 0.05)
  })

  output$plot_labels <- renderUI({
    wellPanel(
      textInput("title", "Figure title") ,
      textInput("y_label", "Y axis label"),
      textInput("x_label", "X axis label"),
      #sliderInput(inputId = "max_x_breaks", label = "Number of X axis breaks",
      #            min = 0, max = 20, value = 10, step = 1),
      #sliderInput(inputId = "pd", label = "Position dodge",
      #            min = 0, max = 1, value = 0, step = 0.05)
      sliderInput(inputId = "label_font_size", label = "Label font size",
                  min = 6, max = 24, value = 14, step = 1)
    )
  })

  output$moving_average <- renderUI({
    wellPanel(
      checkboxInput(inputId = "add_moving_average",
                    label = strong("Add moving average"),
                    value = FALSE),
      conditionalPanel(condition = "input.add_moving_average == true",
                       sliderInput(inputId = "number_smooth_points",
                                   label = "Smoothing window size",
                                   min = 0, max = 200, value = 80, step = 5)
      )
    )
  })

  output$qcc <- renderUI({
    wellPanel(

      checkboxInput(inputId = "plot_qcc",
                    value = FALSE,
                    label = strong("Add QCC limits"))
    )
  })



  output$hline <- renderUI({
    wellPanel(
      checkboxInput(inputId = "add_hline",
                    label = strong("Add horizontal line"),
                    value = FALSE),
      conditionalPanel(condition = "input.add_hline == true",
                       numericInput("hline_y", "Y offset", value=31))
    )
  })

  output$legend <- renderUI({
    wellPanel(
      textInput("legend_title", "Legend Title"),
      selectInput("legend_position", "Legend Position",
                  c("Right"="right", "Bottom"="bottom", "Left"="left", "Top"="top"))
    )
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
          legend.title=text_pars(),
          legend.text=text_pars(),
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
      "    legend.title = element_text(size=", input$label_font_size, "),\n",
      "    legend.text = element_text(size=", input$label_font_size, "),\n",
      "    legend.position = '",input$legend_position,"'\n",
      "  )\n"
    )
  })

  plot_ggplot <- reactive({
    withProgress(message = 'Drawing figure...', value = 0, {
      df <-data()

      # Parameters
      positiondodge <- position_dodge(input$pd)
      pointsize <- 2
      dateformat <- "%Y-%m"
      datebreaks <- "1 year"
      dateminorbreaks <- "3 months"
      date_range <- as.POSIXct(as.Date(input$plot_x_range))
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
      g <- ggplot(df, simple_aes) +
        geom_line_text +
        geom_point_text +
        labs(title=input$title, x=input$x_label, y=input$y_label) +
        scale_x_datetime(labels=date_format(dateformat),
                         limits=date_range,
                         breaks=date_breaks(datebreaks),
                         minor_breaks=dateminorbreaks) +
        mpa_theme()

      # Optional moving average
      if (input$add_moving_average == T){
        g <- g + geom_smooth(simple_aes, size=pointsize,
                             n=input$number_smooth_points)
      }

      # Optional hline
      if (input$add_hline == T){
        g <- g + geom_hline(aes_string(yintercept=input$hline_y))
      }

      if (input$plot_qcc == T){
        ym <- mean(df[[input$ycol]], na.rm=T)
        ysd <- sd(df[[input$ycol]], na.rm=T)
        xmax <- max(df[[input$xcol]])

        g <- g +
          geom_hline(aes_string(yintercept=ym), label=round(ym, digits=0)) +
          # geom_hline(aes_string(yintercept=ym+ysd), linetype="dashed") +
          # geom_hline(aes_string(yintercept=ym-ysd), linetype="dashed") +
          geom_hline(aes_string(yintercept=ym+2*ysd), linetype="dotted", col="orange") +
          geom_hline(aes_string(yintercept=ym-2*ysd), linetype="dotted", col="orange") +
          geom_hline(aes_string(yintercept=ym+3*ysd), linetype=4, col="red") +
          geom_hline(aes_string(yintercept=ym-3*ysd), linetype=4, col="red")
      }

      g
    })

  })

  # output object: rendered plot
  output$plot_ggplot <- renderPlot({plot_ggplot()})

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
    date_range <- as.POSIXct(as.Date(input$plot_x_range))

    # Optional moving average
    if (input$add_moving_average == T){
      geom_smooth_text <- paste0("  geom_smooth(", aesthetic ,", size=2, n=",
                                 input$number_smooth_points, ") +\n")
    } else {geom_smooth_text <- ""}

    # Optional hline
    if (input$add_hline == T){
      geom_hline_text <- paste0("  geom_hline(aes(yintercept=", input$hline_y ,")) +\n")
    } else {geom_hline_text <- ""}



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
      "  function(x){x<- lubridate::parse_date_time(",
      "x, orders=c('YmdHMSz', 'YmdHMS','Ymd','dmY'), tz='Australia/Perth')}\n",
      ")\n",
      "names(df) <- capitalize(names(df))\n\n",

      "pdf('", input$output_filename,".pdf', height = 5, width = 7)\n\n",

      "ggplot(df, ", aesthetic, ") +\n",
      geom_point_text,
      geom_line_text,
      "  labs(title='",input$title,"', x='",input$x_label,"', y='",input$y_label,"') +\n",
      "  scale_x_datetime(labels=date_format('%Y-%m'),\n",
      "    limits=c(as.POSIXct('", date_range[1], "'), as.POSIXct('", date_range[2], "')),\n",
      "    breaks='1 year', minor_breaks='3 months') +\n",
      geom_smooth_text,
      geom_hline_text,
      mpa_theme_text(),

      "\ndev.off()\n"
    ) # /paste
  }) #/reactive

  # output object: rendered R code
  output$rcode <- renderText({plot_code()})

  # output object: download PDF
  output$downloadPdf <- downloadHandler(
    filename = function() {paste0(input$output_filename, '.pdf')},
    content = function(file) {
      pdf(file, height = 5, width = 7);
      print(plot_ggplot());
      dev.off()
    }
  )

  # output object: download R code
  output$downloadCode <- downloadHandler(
    filename = function() {paste0(input$output_filename, '.txt')},
    content = function(file) {writeLines(plot_code(), file)}
  )

  output$save2disk <- renderUI({
    wellPanel(
      h4("Save to disk"),
      textInput("output_filename", "File name", value="figure"),
      downloadButton("downloadPdf", "Download PDF"),
      downloadButton("downloadCode", "Download R Code")
    )
  })

})
