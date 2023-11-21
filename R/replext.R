replext <- function() {

  # Define the UI
  ui <- shiny::fluidPage(
    shiny::titlePanel("Replext Simulation"),
    shiny::sidebarLayout(
      shiny::sidebarPanel(
        shiny::selectInput("cellBlock", "Select Cell Block:",
                           choices = getCellBlocks()),
        shiny::uiOutput("paramsUI"),
        shiny::actionButton("runSim", "Run Simulation")
      ),
      shiny::mainPanel(
        DT::DTOutput("resultsTable")
      )
    )
  )

  # Define the server logic
  server <- function(input, output, session) {

    # Render the UI for parameters based on the selected cell block
    output$paramsUI <- shiny::renderUI({
      getUIParams(input$cellBlock)
    })

    # Reactive value to store the results
    results <- shiny::reactiveVal(data.frame())

    # Observe event for the run simulation button
    shiny::observeEvent(input$runSim, {
      # Call the simulation function with both user-provided and default parameters
      simResults <- runSimulation(input)

      # Update the results reactive value
      results(simResults)
    })

    # Output the results table
    output$resultsTable <- DT::renderDT({
      results()
    }, options = list(pageLength = 5))
  }

  # Run the application
  shiny::shinyApp(ui = ui, server = server)
}
