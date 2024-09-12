#' Replext Simulation Shiny App with Database Integration
#'
#' This application replicates and extends the simulation results from
#' the paper by Dwivedi et al. (2017), now with added functionality to
#' interact with a PostgreSQL database. The app includes a user interface
#' for selecting simulation parameters and a server logic to process the
#' simulation and handle user interactions, including saving and retrieving
#' data from a database.
#'
#' @details
#' The app's user interface consists of:
#' - A dropdown menu to select a cell block for the simulation, which is
#'   populated using the `getCellBlocks` function.
#' - Dynamic UI elements for inputting simulation parameters, generated
#'   based on the selected cell block.
#' - Buttons to run the simulation and submit the results to a PostgreSQL database.
#' - A table to display the simulation results and previously saved responses.
#' - A download button to export all responses as a CSV file.
#'
#' The server logic of the app handles:
#' - Rendering the dynamic UI elements for simulation parameters.
#' - Observing the simulation run event and processing the simulation
#'   using the `runSimulation` function.
#' - Rendering a table to display the simulation results.
#' - Handling the submission of results and storing them in a PostgreSQL database.
#' - Loading existing responses from the database.
#' - Downloading responses as a CSV file.
#'
#' @param dbname The name of the PostgreSQL database to connect to.
#' @param datatable The name of the table in the database where the simulation results will be stored and retrieved.
#' @param host The host address of the PostgreSQL database.
#' @param port The port number for the PostgreSQL database connection.
#' @param user The username for accessing the PostgreSQL database.
#' @param password The password for the specified user to access the PostgreSQL database.
#'
#' @return A Shiny app object which can be run to start the application.
#'
#' @references
#' Dwivedi AK, Mallawaarachchi I, Alvarado LA. Analysis of small sample size studies
#' using nonparametric bootstrap test with pooled resampling method. Stat Med. 2017
#' Jun 30;36(14):2187-2205. doi: 10.1002/sim.7263. Epub 2017 Mar 9. PMID: 28276584.
#'
#' @examples
#' if (interactive()) {
#'   replext_pgsql(
#'     dbname = "your_db_name",
#'     datatable = "your_data_table",
#'     host = "localhost",
#'     port = 5432,
#'     user = "your_username",
#'     password = "your_password"
#'   )
#' }
#'
#' @export
replext_pgsql <- function(dbname, datatable, host, port, user, password) {

  # Define the UI
  ui <- shiny::fluidPage(
    shiny::titlePanel("Replext Simulation"),
    shiny::sidebarLayout(
      shiny::sidebarPanel(
        shiny::selectInput("cellBlock", "Select Cell Block:",
                           choices = getCellBlocks()),
        shiny::uiOutput("paramsUI"),
        shiny::actionButton("runSim", "Run Simulation"),
        mmints::postgresUI("postgres")$submit,
        shiny::br(),  # Add a line break
        shiny::br(),  # Add a line break
        mmints::postgresUI("postgres")$download,
        mmints::citationUI("citations")$button
      ),
      shiny::mainPanel(
        # Conditionally display the Simulation Results header and table
        shiny::uiOutput("simulation_results_header"),
        DT::DTOutput("resultsTable"),
        shiny::br(),  # Add a line break
        shiny::br(),  # Add a line break
        # Add a header for the responses table
        shiny::div(
          shiny::h4("All Responses"),
          mmints::postgresUI("postgres")$table,
        ),
        shiny::uiOutput("citation_header"),
        mmints::citationUI("citations")$output
      )
    )
  )

  # Define the server logic
  server <- function(input, output, session) {

    # Render the UI for parameters based on the selected cell block
    output$paramsUI <- shiny::renderUI({
      getUIParams(input$cellBlock)
    })

    # initialize the postgres module
    postgres_module <- mmints::postgresServer("postgres",
                                              dbname = dbname,
                                              datatable = datatable,
                                              host = host,
                                              port = port,
                                              user = user,
                                              password = password,
                                              data = NULL)

    # Reactive value to store the results
    results <- shiny::reactiveVal(data.frame())     #For display
    results_exp <- shiny::reactiveVal(data.frame()) #For export

    # # Load data from the database on app start
    # output$responses <- DT::renderDT({
    #   loadData()
    # }, options = list(pageLength = 5))

    # Observe event for the run simulation button
    shiny::observeEvent(input$runSim, {

      # make sure responses are clear
      results(data.frame())

      # Call the simulation function with both user-provided and default parameters
      simResults <- runSimulation(input)

      # Update the results reactive value
      results(simResults)
      results_exp(appendInputParams(results(), input))

      # submit results to database
      postgres_module$data_to_submit(results_exp())
    })

    #Output the results table
    output$resultsTable <- DT::renderDT({
      results()
    }, options = list(pageLength = 5))

    # Conditionally display the Simulation Results header
    output$simulation_results_header <- shiny::renderUI({
      if (nrow(results()) > 0) {
        shiny::h4("Simulation Results")
      } else {
        NULL
      }
    })

    # build citation list
    citations <- list(
      "Nonparametric Bootstrap Test with Pooled Resampling Method:" = "Dwivedi, A. K., Mallawaarachchi, I., & Alvarado, L. A. (2017). Analysis of small sample size studies using nonparametric bootstrap test with pooled resampling method. Statistics in Medicine, 36(14), 2187-2205. https://doi.org/10.1002/sim.7263",
      "Software Implementing Nonparametric Bootstrap Test with Pooled Resampling:" = function() mmints::format_citation(utils::citation("npboottprm"))
    )

    # create citation for display
    mmints::citationServer("citations", citations)

  }

  # Run the application
  shiny::shinyApp(ui = ui, server = server)
}
