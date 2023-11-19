replext_t2_app <- function() {

  # Define the UI
  ui <- shiny::fluidPage(
    shiny::titlePanel("Replext Table 2"),
    shiny::sidebarLayout(
      shiny::sidebarPanel(
        shiny::selectInput("cellBlock", "Select Cell Block:",
                           choices = list("Cell Block 1.1" = "replext_t2_c1.1",
                                          "Cell Block 1.2" = "replext_t2_c1.2",
                                          "Cell Block 2.1" = "replext_t2_c2.1",
                                          "Cell Block 2.2" = "replext_t2_c2.2",
                                          "Cell Block 3.1" = "replext_t2_c3.1",
                                          "Cell Block 3.2" = "replext_t2_c3.2",
                                          "Cell Block 4.1" = "replext_t2_c4.1",
                                          "Cell Block 4.2" = "replext_t2_c4.2")),
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
      params <- switch(input$cellBlock,
                       "replext_t2_c1.1" = list(shiny::numericInput("M1", "Mean for the first group:", 5),
                                                shiny::numericInput("S1", "Standard deviation for the first group:", 1),
                                                shiny::numericInput("M2", "Mean for the second group:", 5),
                                                shiny::numericInput("S2", "Standard deviation for the second group:", 1),
                                                shiny::numericInput("Sk1", "Skew for the first group:", value = NA),
                                                shiny::numericInput("Sk2", "Skew for the second group:", value = NA),
                                                shiny::textInput("n1", "Sample sizes for the first group:", "3,4,5,6,7,8,9,10,15"),
                                                shiny::textInput("n2", "Sample sizes for the second group:", "3,4,5,6,7,8,9,10,15"),
                                                shiny::numericInput("n_simulations", "Number of simulation iterations:", 10000),
                                                shiny::numericInput("nboot", "Number of bootstrap iterations:", 1000),
                                                shiny::numericInput("conf.level", "Confidence level:", 0.95)),
                       "replext_t2_c1.2" = list(shiny::numericInput("M1", "Mean for the first group:", 5),
                                                shiny::numericInput("S1", "Standard deviation for the first group:", 1),
                                                shiny::numericInput("M2", "Mean for the second group:", 5),
                                                shiny::numericInput("S2", "Standard deviation for the second group:", 3),
                                                shiny::numericInput("Sk1", "Skew for the first group:", value = NA),
                                                shiny::numericInput("Sk2", "Skew for the second group:", value = NA),
                                                shiny::textInput("n1", "Sample sizes for the first group:", "3,4,5,6,7,8,9,10,15"),
                                                shiny::textInput("n2", "Sample sizes for the second group:", "3,4,5,6,7,8,9,10,15"),
                                                shiny::numericInput("n_simulations", "Number of simulation iterations:", 10000),
                                                shiny::numericInput("nboot", "Number of bootstrap iterations:", 1000),
                                                shiny::numericInput("conf.level", "Confidence level:", 0.95)),
                       "replext_t2_c2.1" = list(shiny::numericInput("M1", "Mean for the first group:", 5),
                                                shiny::numericInput("S1", "Standard deviation for the first group:", 1),
                                                shiny::numericInput("M2", "Mean for the second group:", 5),
                                                shiny::numericInput("S2", "Standard deviation for the second group:", 1),
                                                shiny::numericInput("Sk1", "Skew for the first group:", value = 0.8),
                                                shiny::numericInput("Sk2", "Skew for the second group:", value = 0.8),
                                                shiny::textInput("n1", "Sample sizes for the first group:", "3,4,5,6,7,8,9,10,15"),
                                                shiny::textInput("n2", "Sample sizes for the second group:", "3,4,5,6,7,8,9,10,15"),
                                                shiny::numericInput("n_simulations", "Number of simulation iterations:", 10000),
                                                shiny::numericInput("nboot", "Number of bootstrap iterations:", 1000),
                                                shiny::numericInput("conf.level", "Confidence level:", 0.95)),
                       "replext_t2_c2.2" = list(shiny::numericInput("M1", "Mean for the first group:", 5),
                                                shiny::numericInput("S1", "Standard deviation for the first group:", 1),
                                                shiny::numericInput("M2", "Mean for the second group:", 5),
                                                shiny::numericInput("S2", "Standard deviation for the second group:", 3),
                                                shiny::numericInput("Sk1", "Skew for the first group:", value = 0.8),
                                                shiny::numericInput("Sk2", "Skew for the second group:", value = 0.8),
                                                shiny::textInput("n1", "Sample sizes for the first group:", "3,4,5,6,7,8,9,10,15"),
                                                shiny::textInput("n2", "Sample sizes for the second group:", "3,4,5,6,7,8,9,10,15"),
                                                shiny::numericInput("n_simulations", "Number of simulation iterations:", 10000),
                                                shiny::numericInput("nboot", "Number of bootstrap iterations:", 1000),
                                                shiny::numericInput("conf.level", "Confidence level:", 0.95)),
                       "replext_t2_c3.1" = list(shiny::numericInput("M1", "Mean for the first group:", 5),
                                                shiny::numericInput("S1", "Standard deviation for the first group:", 1),
                                                shiny::numericInput("M2", "Mean for the second group:", 5),
                                                shiny::numericInput("S2", "Standard deviation for the second group:", 1),
                                                shiny::numericInput("Sk1", "Skew for the first group:", value = 0.8),
                                                shiny::numericInput("Sk2", "Skew for the second group:", value = 1),
                                                shiny::textInput("n1", "Sample sizes for the first group:", "3,4,5,6,7,8,9,10,15"),
                                                shiny::textInput("n2", "Sample sizes for the second group:", "3,4,5,6,7,8,9,10,15"),
                                                shiny::numericInput("n_simulations", "Number of simulation iterations:", 10000),
                                                shiny::numericInput("nboot", "Number of bootstrap iterations:", 1000),
                                                shiny::numericInput("conf.level", "Confidence level:", 0.95)),
                       "replext_t2_c3.2" = list(shiny::numericInput("M1", "Mean for the first group:", 5),
                                                shiny::numericInput("S1", "Standard deviation for the first group:", 1),
                                                shiny::numericInput("M2", "Mean for the second group:", 5),
                                                shiny::numericInput("S2", "Standard deviation for the second group:", 3),
                                                shiny::numericInput("Sk1", "Skew for the first group:", value = 0.8),
                                                shiny::numericInput("Sk2", "Skew for the second group:", value = 1),
                                                shiny::textInput("n1", "Sample sizes for the first group:", "3,4,5,6,7,8,9,10,15"),
                                                shiny::textInput("n2", "Sample sizes for the second group:", "3,4,5,6,7,8,9,10,15"),
                                                shiny::numericInput("n_simulations", "Number of simulation iterations:", 10000),
                                                shiny::numericInput("nboot", "Number of bootstrap iterations:", 1000),
                                                shiny::numericInput("conf.level", "Confidence level:", 0.95)),
                       "replext_t2_c4.1" = list(shiny::numericInput("M1", "Mean for the first group:", 5),
                                                shiny::numericInput("S1", "Standard deviation for the first group:", 1),
                                                shiny::numericInput("M2", "Mean for the second group:", 5),
                                                shiny::numericInput("S2", "Standard deviation for the second group:", 1),
                                                shiny::numericInput("Sk1", "Skew for the first group:", value = 0.8),
                                                shiny::numericInput("Sk2", "Skew for the second group:", value = 0.8),
                                                shiny::textInput("n1", "Sample sizes for the first group:", "4,3,5,4,6,4,3,4,5,6"),
                                                shiny::textInput("n2", "Sample sizes for the second group:", "2,4,3,5,3,6,7,11,10,9"),
                                                shiny::numericInput("n_simulations", "Number of simulation iterations:", 10000),
                                                shiny::numericInput("nboot", "Number of bootstrap iterations:", 1000),
                                                shiny::numericInput("conf.level", "Confidence level:", 0.95)),
                       "replext_t2_c4.2" = list(shiny::numericInput("M1", "Mean for the first group:", 5),
                                                shiny::numericInput("S1", "Standard deviation for the first group:", 1),
                                                shiny::numericInput("M2", "Mean for the second group:", 5),
                                                shiny::numericInput("S2", "Standard deviation for the second group:", 3),
                                                shiny::numericInput("Sk1", "Skew for the first group:", value = 0.8),
                                                shiny::numericInput("Sk2", "Skew for the second group:", value = 0.8),
                                                shiny::textInput("n1", "Sample sizes for the first group:", "4,3,5,4,6,4,3,4,5,6"),
                                                shiny::textInput("n2", "Sample sizes for the second group:", "2,4,3,5,3,6,7,11,10,9"),
                                                shiny::numericInput("n_simulations", "Number of simulation iterations:", 10000),
                                                shiny::numericInput("nboot", "Number of bootstrap iterations:", 1000),
                                                shiny::numericInput("conf.level", "Confidence level:", 0.95)))

      do.call(shiny::tagList, params)
    })

    # Reactive value to store the results
    results <- shiny::reactiveVal(data.frame())

    # Function to convert text input to numeric vector
    text_to_vector <- function(text_input) {
      as.numeric(unlist(strsplit(text_input, ",")))
    }

    # Observe event for the run simulation button
    shiny::observeEvent(input$runSim, {

      # Define a function to handle NULL or NA skewness
      handle_skew <- function(skew_input) {
        if (is.na(skew_input) || skew_input == "") {
          return(NULL)
        } else {
          return(as.numeric(skew_input))
        }
      }

      # Call the simulation function with both user-provided and default parameters
      simResults <- replext_t2_c1.1(M1 = input$M1, S1 = input$S1, M2 = input$M2,
                                    S2 = input$S2,
                                    Sk1 = handle_skew(input$Sk1),
                                    Sk2 = handle_skew(input$Sk2),
                                    n1 = text_to_vector(input$n1),
                                    n2 = text_to_vector(input$n2),
                                    n_simulations = input$n_simulations,
                                    nboot = input$nboot,
                                    conf.level = input$conf.level)

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
