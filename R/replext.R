replext <- function() {

  # Define the UI
  ui <- shiny::fluidPage(
    shiny::titlePanel("Replext Simulation"),
    shiny::sidebarLayout(
      shiny::sidebarPanel(
        shiny::selectInput("cellBlock", "Select Cell Block:",
                           choices = list("T2 Cell Block 1.1" = "replext_t2_c1.1",
                                          "T2 Cell Block 1.2" = "replext_t2_c1.2",
                                          "T2 Cell Block 2.1" = "replext_t2_c2.1",
                                          "T2 Cell Block 2.2" = "replext_t2_c2.2",
                                          "T2 Cell Block 3.1" = "replext_t2_c3.1",
                                          "T2 Cell Block 3.2" = "replext_t2_c3.2",
                                          "T2 Cell Block 4.1" = "replext_t2_c4.1",
                                          "T2 Cell Block 4.2" = "replext_t2_c4.2",
                                          "T3 Cell Block 1.1" = "replext_t3_c1.1",
                                          "T3 Cell Block 1.2" = "replext_t3_c1.2",
                                          "T3 Cell Block 2.1" = "replext_t3_c2.1",
                                          "T3 Cell Block 2.2" = "replext_t3_c2.2",
                                          "T3 Cell Block 3.1" = "replext_t3_c3.1",
                                          "T3 Cell Block 3.2" = "replext_t3_c3.2",
                                          "T3 Cell Block 4.1" = "replext_t3_c4.1",
                                          "T3 Cell Block 4.2" = "replext_t3_c4.2",
                                          "T4 Cell Block 1.1" = "replext_t4_c1.1",
                                          "T4 Cell Block 2.1" = "replext_t4_c2.1",
                                          "T4 Cell Block 3.1" = "replext_t4_c3.1",
                                          "T4 Cell Block 4.1" = "replext_t4_c4.1",
                                          "T4 Cell Block 5.1" = "replext_t4_c5.1",
                                          "T4 Cell Block 6.1" = "replext_t4_c6.1",
                                          "T4 Cell Block 7.1" = "replext_t4_c7.1"
                                          )),
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
