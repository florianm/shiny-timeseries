source("global.R")
shinyServer(function(input, output) {

  # object: data source - CSV URL, e.g. from CKAN
  data <- reactive({
    if (is.null(input$csv_url)) return(NULL)

    d <- as.data.frame(
      lapply(
        read.csv(input$csv_url, sep=",", header=T, stringsAsFactors=T),
        function(x) {
          if(is.factor(x)){x <- lubridate::parse_date_time(x, ldo, tz=ldz)}
          x
        }
      )
    )

  })


  # UI elements
  output$ycol <- renderUI({
    df <-data()
    if (is.null(df)) return(NULL)
    items=names(df)
    names(items)= items #sapply(items, function(x){paste0(x, " (", class(df[[x]])[[1]], ")")})
    selectInput("ycol",
                "Choose independent variable (y axis, numeric)",
                items,
                selected=items[2])
  })

  output$xcol <- renderUI({
    df <-data()
    if (is.null(df)) return(NULL)
    items=names(df)
    names(items)= items #sapply(items, function(x){paste0(x, " (", class(df[[x]])[[1]], ")")})
    selectInput("xcol",
                "Choose dependent Variable (x axis, date)",
                items)
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
  # End UI elements

  # output object: data summary
  output$summary <- renderPrint({ str(data()) })

  # output object: data table
  output$table <- renderDataTable({ data() })

  #   # output object: a simple plot
  #   output$plot_simple <- renderPlot({
  #     df <-data()
  #     plot(df[[input$ycol]] ~ df[[input$xcol]],
  #          xlab=input$x_label, ylab=input$y_label)
  #   })


  # ggplot object
  plot_ggplot <- reactive({
    df <-data()
    point_size <- 3
    pd <- position_dodge(input$pd)

    ggplot(df, aes_string(x=input$xcol, y=input$ycol)) +
      geom_line(position=pd) +
      geom_point(position=pd, size=point_size) +
      ggtitle(input$title) +
      ylab(input$y_label) +
      xlab(input$x_label) +
      scale_x_datetime(labels=date_format("%Y-%m"),
                       breaks=date_breaks("1 year"),
                       minor_breaks="3 months") +
      mpa_theme

  })

  # output object: rendered plot
  output$plot_ggplot <- renderPlot({
    plot_ggplot()
  })

  # output object: download PDF
  output$downloadPdf <- downloadHandler(
    filename = function() { paste0(input$output_filename, '.pdf') },
    content = function(file) {
      pdf(file, height = 5, width = 7);
      print(plot_ggplot());
      dev.off()
    }
  )

  text_instruction <- reactive({
    if (is.null(input$rcode_url) ||
          input$rcode_url == "" ||
          !url.exists(input$rcode_url)) { return(NULL) }
    paste0("## Reproduce the figure:\n# source('", input$rcode_url, "')\n\n")
  })

  # R code spelled out
  plot_code <- reactive({
    df <-data()

    paste0(
      text_instruction(),
      "df <- as.data.frame(lapply(\n  read.table('",
      input$csv_url, "', sep=',', header=T, stringsAsFactors=T),\n",
      "  function(x) {if(is.factor(x)){x <- lubridate::parse_date_time(",
      "x, c('YmdHMSz', 'YmdHMS','Ymd','dmY'), tz='Australia/Perth')};x}))\n\n",
      "pdf('", input$output_filename,".pdf', height = 5, width = 7);\n",
      "ggplot(df, aes_string(x='", input$xcol, "', y='", input$ycol, "')) +\n",
      "  geom_line(position=position_dodge(", input$pd,")) +\n",
      "  geom_point(position=position_dodge(", input$pd,"), size=3) +\n",
      "  ylab('", input$y_label,"') +\n",
      "  xlab('", input$x_label,"') +\n",
      "  scale_x_date(labels=date_format('%Y-%m'), breaks='1 year', minor_breaks='3 months'),\n",
      mpa_theme_text,
      "\ndev.off()\n"
    ) # /paste

  }) #/reactive

  # output object: rendered R code
  output$rcode <- renderText({ plot_code() })

  # output object: download R code
  output$downloadCode <- downloadHandler(
    filename = function() { paste0(input$output_filename, '.txt') },
    content = function(file) { writeLines(plot_code(), file) }
  )

})
