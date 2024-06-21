#' Replext Simulation Shiny App
#'
#' This application attempts to replicate and extend the simulation results from
#' the paper by Dwivedi et al. (2017). The application includes a user interface
#' for selecting simulation parameters and a server logic to process the simulation
#' and handle user interactions.
#'
#' @details
#' The app's user interface consists of:
#' - A dropdown menu to select a cell block for the simulation, which is
#'   populated using the `getCellBlocks` function.
#' - Dynamic UI elements for inputting simulation parameters, generated
#'   based on the selected cell block.
#' - A button to run the simulation.
#' - A download button to export the simulation results.
#'
#' The server logic of the app handles:
#' - Rendering the dynamic UI elements for simulation parameters.
#' - Observing the simulation run event and processing the simulation
#'   using the `runSimulation` function.
#' - Rendering a table to display the simulation results.
#' - Handling the data download request and exporting the results as a CSV file.
#'
#' @return A Shiny app object which can be run to start the application.
#'
#' @references
#' Dwivedi AK, Mallawaarachchi I, Alvarado LA. Analysis of small sample size studies
#' using nonparametric bootstrap test with pooled resampling method. Stat Med. 2017
#' Jun 30;36(14):2187-2205. doi: 10.1002/sim.7263. Epub 2017 Mar 9. PMID: 28276584.
#'
#' @examples
#' if(interactive()){
#'   replext()
#' }
#'
#' @export
replext <- function() {

  # Define the UI
  ui <- shiny::fluidPage(
    shiny::titlePanel("Replext Simulation"),
    shiny::sidebarLayout(
      shiny::sidebarPanel(
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
