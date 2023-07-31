nonparboot_app <- function(){
  ui <- shiny::fluidPage(
    shiny::fileInput("file", "Upload CSV File", accept = c(".csv")),
    shiny::textInput("x", "X Variable"),
    shiny::helpText("Enter the name of the X Variable."),
    shiny::selectInput("test", "Test Type", c("t", "pt", "F")),
    shiny::textInput("y", "Y Variable (optional)"),
    shiny::helpText("For paired t-test ('pt'), enter the name of the Y Variable."),
    shiny::textInput("grp", "Grouping Variable (optional)"),
    shiny::helpText("For independent t-test ('t') or F-test ('F'), enter the name of the Grouping Variable."),
    shiny::numericInput("nboot", "Number of Bootstrap Resamples", value = 1000, min = 1),
    shiny::numericInput("conf.level", "Confidence Level", value = 0.95, min = 0, max = 1),
    shiny::numericInput("seed", "Random Seed (optional)", value = NULL),
    shiny::actionButton("run", "Run Test"),
    shiny::verbatimTextOutput("result"),
    shiny::plotOutput("hist")
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
      nonparboot(data(), x = input$x, y = if(input$test == 'pt') input$y else NULL,
                 grp = if(input$test != 'pt') input$grp else NULL, nboot = input$nboot,
                 test = input$test, conf.level = input$conf.level, seed = input$seed)
    })

    # Display result
    output$result <- shiny::renderPrint({
      shiny::req(results())
      results()
    })

    # Display histogram
    output$hist <- shiny::renderPlot({
      shiny::req(results())
      ggplot2::ggplot(data.frame(x = results()$bootstrap.dist), ggplot2::aes(x)) +
        ggplot2::geom_histogram(color = "black", fill = "white") +
        ggplot2::labs(x = "Bootstrap Distribution", y = "Frequency")
    })
  }

  # Run the application
  shiny::shinyApp(ui = ui, server = server)
}




# nonparboot_app <- function(){
#   ui <- shiny::fluidPage(
#     shiny::fileInput("file", "Upload CSV File", accept = c(".csv")),
#     shiny::selectInput("test", "Test Type", c("t", "pt", "F")),
#     shiny::conditionalPanel(condition = "input.test != 'F'",
#                             shiny::textInput("y", "Y Variable (optional)")),
#     shiny::conditionalPanel(condition = "input.test != 'pt'",
#                             shiny::textInput("grp", "Grouping Variable (optional)")),
#     shiny::numericInput("nboot", "Number of Bootstrap Resamples", value = 1000, min = 1),
#     shiny::numericInput("conf.level", "Confidence Level", value = 0.95, min = 0, max = 1),
#     shiny::numericInput("seed", "Random Seed (optional)", value = NULL),
#     shiny::actionButton("run", "Run Test"),
#     shiny::verbatimTextOutput("result"),
#     shiny::plotOutput("hist")
#   )
#
#   # Server logic
#   server <- function(input, output) {
#
#     # Read data
#     data <- shiny::reactive({
#       shiny::req(input$file)
#       shiny::read.csv(input$file$datapath)
#     })
#
#     # Run test
#     results <- shiny::eventReactive(input$run, {
#       shiny::req(data())
#       nonparboot(data(), x = names(data())[1], y = if(input$test == 'pt') input$y else NULL,
#                  grp = if(input$test != 'pt') input$grp else NULL, nboot = input$nboot,
#                  test = input$test, conf.level = input$conf.level, seed = input$seed)
#     })
#
#     # Display result
#     output$result <- shiny::renderPrint({
#       shiny::req(results())
#       results()
#     })
#
#     # Display histogram
#     output$hist <- shiny::renderPlot({
#       shiny::req(results())
#       ggplot2::ggplot(data.frame(x = results()$bootstrap.dist), ggplot2::aes(x)) +
#         ggplot2::geom_histogram(color = "black", fill = "white") +
#         ggplot2::labs(x = "Bootstrap Distribution", y = "Frequency")
#     })
#   }
#
#   # Run the application
#   shiny::shinyApp(ui = ui, server = server)
#
# }

