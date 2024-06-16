replext_pgsql <- function(dbname, datatable, host, port, user, password) {

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
        shiny::numericInput("seed", "Random Number Seed (Optional):", value = NA, min = 1),
        shiny::selectInput("cellBlock", "Select Cell Block:",
                           choices = getCellBlocks()),
        shiny::uiOutput("paramsUI"),
        shiny::actionButton("runSim", "Run Simulation"),
        shiny::actionButton("submit", "Submit")
        #shiny::downloadButton("downloadBtn", "Download Data")
      ),
      shiny::mainPanel(
        # Conditionally display the Simulation Results header and table
        shiny::uiOutput("simulation_results_header"),
        DT::DTOutput("resultsTable"),
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
  }

  # Run the application
  shiny::shinyApp(ui = ui, server = server)
}
