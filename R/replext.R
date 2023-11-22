replext <- function() {

  # Define the UI
  ui <- shiny::fluidPage(
    shiny::titlePanel("Replext Simulation"),
    shiny::sidebarLayout(
      shiny::sidebarPanel(
        shiny::numericInput("seed", "Random Number Seed (Optional):", value = NA, min = 1),
        shiny::selectInput("cellBlock", "Select Cell Block:",
                           choices = getCellBlocks()),
        shiny::uiOutput("paramsUI"),
        shiny::actionButton("runSim", "Run Simulation"),
        shiny::downloadButton("downloadBtn", "Download Data")
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
    results <- shiny::reactiveVal(data.frame())     #For display
    results_exp <- shiny::reactiveVal(data.frame()) #For export

    # Observe event for the run simulation button
    shiny::observeEvent(input$runSim, {
      # Call the simulation function with both user-provided and default parameters
      simResults <- runSimulation(input)

      # Update the results reactive value
      results(simResults)
    })

    #Output the results table
    output$resultsTable <- DT::renderDT({
      results()
    }, options = list(pageLength = 5))

    # Download handler for exporting data
    output$downloadBtn <- shiny::downloadHandler(
      filename = function() {
        paste0("Simulation_Results_", Sys.Date(), ".csv")
      },
      content = function(file) {
        # Ensure there is data to download
        shiny::req(results())

        # Get appended results
        simResults_exp <- appendInputParams(results(), input)

        # Write the data to a CSV file
        utils::write.csv(simResults_exp, file, row.names = FALSE)
      }
    )
  }

  # Run the application
  shiny::shinyApp(ui = ui, server = server)
}
