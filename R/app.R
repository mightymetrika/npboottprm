nonparboot_app <- function(){
  ui <- shiny::fluidPage(
    theme = shinythemes::shinytheme("yeti"),
    shiny::column(width = 4,
                  shiny::h3("Arguments"),
                  shiny::fileInput("file", "Upload CSV File", accept = c(".csv")),
                  shiny::tags$div(title = "Enter the name of the X Variable.",
                                  shiny::textInput("x", "X Variable", placeholder = "Enter X variable name here")),
                  shiny::selectInput("test", "Test Type", c("t", "pt", "F")),
                  shiny::tags$div(title = "For paired t-test ('pt'), enter the name of the Y Variable.",
                                  shiny::textInput("y", "Y Variable (optional)", placeholder = "Enter Y variable name here")),
                  shiny::tags$div(title = "For independent t-test ('t') or F-test ('F'), enter the name of the Grouping Variable.",
                                  shiny::textInput("grp", "Grouping Variable (optional)", placeholder = "Enter Grouping variable name here")),
                  shiny::numericInput("nboot", "Number of Bootstrap Resamples", value = 1000, min = 1),
                  shiny::numericInput("conf.level", "Confidence Level", value = 0.95, min = 0, max = 1),
                  shiny::numericInput("seed", "Random Seed (optional)", value = NA),
                  shiny::actionButton("run", "Run Test")
    ),
    shiny::column(width = 8, align="center",
                  shiny::h3("Nonparametric Bootstrap Test"),
                  shiny::br(),  # Add a line break
                  shiny::br(),  # Add a line break
                  shiny::tableOutput("result_table"),
                  shiny::br(),  # Add a line break
                  shiny::br(),  # Add a line break
                  shiny::uiOutput("histAndSlider")
    )
  )

  # Server logic
  server <- function(input, output, session) {

    # Read data
    data <- shiny::reactive({
      shiny::req(input$file)
      df <- utils::read.csv(input$file$datapath)
      return(df)
    })

    # Run test
    results <- shiny::eventReactive(input$run, {
      shiny::req(data())
      seed <- if (!is.na(input$seed)) input$seed else NULL
      nonparboot(data(), x = input$x, y = if(input$test == 'pt') input$y else NULL,
                 grp = if(input$test != 'pt') input$grp else NULL, nboot = input$nboot,
                 test = input$test, conf.level = input$conf.level, seed = seed)
    })

    # Display result in a table
    output$result_table <- shiny::renderTable({
      shiny::req(results())
      res <- results()
      df_res <- data.frame(
        Effect_Size = res$effect.size,
        CI_Lower = res$ci.effect.size[1],
        CI_Upper = res$ci.effect.size[2],
        P_Value = res$p.value
      )
      names(df_res) <- c("Effect Size", "CI (Lower)", "CI (Upper)", "P-value")
      df_res
    }, row.names = FALSE)

    # Display histogram and slider
    output$histAndSlider <- shiny::renderUI({
      shiny::req(results())
      list(
        shiny::plotOutput("hist"),
        shiny::br(),  # Add a line break
        shiny::sliderInput("bins", "Number of bins:", min = 10, max = 50, value = 30)
      )
    })

    # Display histogram
    output$hist <- shiny::renderPlot({
      shiny::req(results())
      res <- results()
      ggplot2::ggplot(data.frame(x = res$bootstrap.dist), ggplot2::aes(x)) +
        ggplot2::geom_histogram(color = "black", fill = "white", bins = input$bins) +
        ggplot2::geom_vline(xintercept = c(res$ci.effect.size), color = "red", linetype = "dashed") +
        ggplot2::geom_vline(xintercept = res$effect.size, color = "blue") +
        ggplot2::labs(x = "Bootstrap Distribution", y = "Frequency",
                      title = "Histogram of Bootstrap Distribution with Effect Size and Confidence Intervals")
    })
  }

  # Run the application
  shiny::shinyApp(ui = ui, server = server)
}
