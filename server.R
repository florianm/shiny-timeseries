shinyServer(function(input, output) {

  # object: data source - CSV URL, e.g. from CKAN
  data <- reactive({
    infile <- input$csv_url
    if (is.null(infile)) { return(NULL) }
    read.table(input$csv_url, sep=",", header=T, stringsAsFactors=F)
  })

  # UI elements
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
  output$summary <- renderPrint({ summary(data()) })

  # output object: data table
  output$table <- renderDataTable({ data() })

  #   # output object: a simple plot
  #   output$plot_simple <- renderPlot({
  #     df <-data()
  #     plot(df[[input$ycol]] ~ df[[input$xcol]],
  #          xlab=input$x_label, ylab=input$y_label)
  #   })

  # ggplot object
  plot_ggplot <- reactive(function() {
    df <-data()

    x_col <- input$xcol
    y_col <- input$ycol
    x_min <- min(df[[input$xcol]])
    x_max <- max(df[[input$xcol]])
    x_limits <- c(x_min-input$x_extra, x_max+input$x_extra)
    no_x_breaks <- length(x_min:x_max)
    if (no_x_breaks < input$max_x_breaks) {
      step_x_breaks <- 1
    } else {
      step_x_breaks <- floor(no_x_breaks / input$max_x_breaks)
    }
    x_breaks <- seq(x_min, x_max, step_x_breaks)
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

  # r code spelled out
  plot_code <- reactive({
    df <-data()

    x_col <- input$xcol
    y_col <- input$ycol
    x_min <- min(df[[input$xcol]])
    x_max <- max(df[[input$xcol]])

    no_x_breaks <- length(x_min:x_max)
    if (no_x_breaks < input$max_x_breaks) {
      step_x_breaks <- 1
    } else {
      step_x_breaks <- floor(no_x_breaks / input$max_x_breaks)
    }

    print(
      paste0(
        text_instruction(),

        "df <- read.table('", input$csv_url,
        "', sep=',', header=T, stringsAsFactors=T)\n\n",

        "pdf('", input$output_filename,".pdf', height = 5, width = 7);\n\n",

        "ggplot(df, aes_string(x='", input$xcol, "', y='", input$ycol, "')) +\n",
        "  geom_line(position=position_dodge(", input$pd,")) +\n",
        "  geom_point(position=position_dodge(", input$pd,"), size=3) +\n",
        "  ylab('", input$y_label,"') +\n",
        "  xlab('", input$x_label,"') +\n",
        "  scale_x_continuous(limits=c(",
        x_min-input$x_extra,",",x_max+input$x_extra,
        "), breaks=seq(", x_min, ",", x_max, ",", step_x_breaks, ")) +\n",
        "  theme(\n",
        "    axis.text.x = element_text(size=14),\n",
        "    axis.text.y = element_text(size=14),\n",
        "    axis.title.x=element_text(size=14), # or element_blank(),\n",
        "    axis.title.y=element_text(size=14),\n",
        "    axis.line=element_line(colour='black'),\n",
        "    panel.grid.minor = element_blank(),\n",
        "    panel.grid.major = element_blank(),\n",
        "    panel.border=element_blank(),\n",
        "    panel.background=element_blank(),\n",
        "    legend.justification=c(1,10),\n",
        "    legend.position=c(1,10), # Position legend in top right\n",
        "    legend.title = element_blank(),\n",
        "    legend.key = element_blank()\n",
        "  )\n\n",

        "dev.off()\n"
      ) # /paste
    ) # /print
  }) #/reactive

  # output object: rendered R code
  output$rcode <- renderText({ print(plot_code()) })

  # output object: download R code
  output$downloadCode <- downloadHandler(
    filename = function() { paste0(input$output_filename, '.txt') },
    content = function(file) {
      writeLines(plot_code(), file)
    }
  )



})
