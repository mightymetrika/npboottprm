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

  # Helper function to save data to the database
  saveData <- function(data){

    # Connect to the database
    pool <- pool::dbPool(
      drv = RPostgres::Postgres(),
      dbname = dbname,
      host = host,
      user = user,
      password = password,
      port = port
    )

    # Close pool on stop
    shiny::onStop(function() {
      pool::poolClose(pool)
    })

    # Change name of conf.level to conf
    colnames(data)[which(names(data) == "conf.level")] <- "conf"

    # Convert NA to NaN in the data frame
    data[is.na(data)] <- NaN

    # Construct the update query by looping over the data fields
    query <- sprintf(
      "INSERT INTO %s (%s) VALUES ('%s')",
      datatable,
      paste(names(data), collapse = ", "),
      paste(data, collapse = "', '")
    )

    # Execute the query
    tryCatch({
      pool::dbExecute(pool, query)
    }, error = function(e) {
      print(paste("Error:", e))
    })

  }

  # Helper function to load data from database
  loadData <- function() {

    # Connect to the database
    pool <- pool::dbPool(
      drv = RPostgres::Postgres(),
      dbname = dbname,
      host = host,
      user = user,
      password = password,
      port = port
    )

    # Close pool on stop
    shiny::onStop(function() {
      pool::poolClose(pool)
    })

    # Construct the fetching query
    sql <- sprintf("SELECT * FROM %s", datatable)
    query <- pool::sqlInterpolate(pool, sql)

    # Submit the fetch query and disconnect
    pool::dbGetQuery(pool, query)

  }

  # Define the UI
  ui <- shiny::fluidPage(
    shiny::titlePanel("Replext Simulation"),
    shiny::sidebarLayout(
      shiny::sidebarPanel(
        shiny::selectInput("cellBlock", "Select Cell Block:",
                           choices = getCellBlocks()),
        shiny::uiOutput("paramsUI"),
        shiny::actionButton("runSim", "Run Simulation"),
        shiny::actionButton("submit", "Submit"),
        shiny::br(),  # Add a line break
        shiny::br(),  # Add a line break
        shiny::downloadButton("downloadBtn", "Download Responses")
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
          DT::DTOutput("responses")
        )
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

    # Load data from the database on app start
    output$responses <- DT::renderDT({
      loadData()
    }, options = list(pageLength = 5))

    # Observe event for the run simulation button
    shiny::observeEvent(input$runSim, {

      # make sure responses are clear
      results(data.frame())

      # Call the simulation function with both user-provided and default parameters
      simResults <- runSimulation(input)

      # Update the results reactive value
      results(simResults)
      results_exp(simResults)
    })

    #Output the results table
    output$resultsTable <- DT::renderDT({
      results()
    }, options = list(pageLength = 5))

    # When the Submit button is clicked, save the form data
    shiny::observeEvent(input$submit, {
      # Prevent submitting if results are empty
      if(nrow(results_exp()) == 0) {
        shiny::showModal(shiny::modalDialog(
          title = "Error",
          "No results to submit. Please run the simulation first.",
          easyClose = TRUE,
          footer = NULL
        ))
        return()
      }

      # add additional simulation setting information to the results before
      # exporting to database
      simResults_exp <- appendInputParams(results(), input)
      saveData(simResults_exp)

      # Clear the results after submission
      results_exp(data.frame())

      # Update the responses table with new data
      output$responses <- DT::renderDT({
        loadData()
      }, options = list(pageLength = 5))

    })

    # Conditionally display the Simulation Results header
    output$simulation_results_header <- shiny::renderUI({
      if (nrow(results()) > 0) {
        shiny::h4("Simulation Results")
      } else {
        NULL
      }
    })

    # Download handler for exporting data
    output$downloadBtn <- shiny::downloadHandler(
      filename = function() {
        paste0("Simulation_Results_", Sys.Date(), ".csv")
      },
      content = function(file) {
        # Ensure there is data to download
        #shiny::req(loadData())

        # Write the data to a CSV file
        utils::write.csv(loadData(), file, row.names = FALSE)
      }
    )
  }

  # Run the application
  shiny::shinyApp(ui = ui, server = server)
}
