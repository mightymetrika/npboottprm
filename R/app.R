#' Shiny App for Nonparametric Bootstrap Tests with Pooled Resampling
#'
#' This function creates a Shiny app for performing nonparametric bootstrap tests
#' with pooled resampling. The app allows you to conduct an independent t-test,
#' a paired t-test, or a one-way ANOVA, depending on your input.
#'
#' @export
#'
#' @return An interactive Shiny app.
#'
#' @examples
#' if(interactive()){
#'   nonparboot_app()
#' }
#'
#' @references
#' Dwivedi AK, Mallawaarachchi I, Alvarado LA (2017). 'Analysis of small sample
#' size studies using nonparametric bootstrap test with pooled resampling method.'
#' Statistics in Medicine, 36 (14), 2187-2205. https://doi.org/10.1002/sim.7263
nonparboot_app <- function(){
  ui <- shiny::fluidPage(
    theme = shinythemes::shinytheme("yeti"),
    shiny::column(width = 4,
                  shiny::h3("Arguments"),
                  shiny::fileInput("file", "Upload CSV File", accept = c(".csv")),
                  shiny::textInput("x", "X Variable", placeholder = "Enter X variable name here"),
                  shiny::selectInput("test", "Test Type", c("t", "pt", "F")),
                  shiny::textInput("y", "Y Variable (optional)", placeholder = "Enter Y variable name here"),
                  shiny::textInput("grp", "Grouping Variable (optional)", placeholder = "Enter Grouping variable name here"),
                  shiny::numericInput("nboot", "Number of Bootstrap Resamples", value = 1000, min = 1),
                  shiny::numericInput("conf.level", "Confidence Level", value = 0.95, min = 0, max = 1),
                  shiny::numericInput("seed", "Random Seed (optional)", value = NA),
                  shiny::checkboxInput("na_rm", "Remove observations with missing values", value = FALSE),
                  shiny::actionButton("run", "Run Test"),
                  mmints::citationUI("citations")$button
    ),
    shiny::column(width = 8, align="center",
                  shiny::h3("Nonparametric Bootstrap Test with Pooled Resampling"),
                  shiny::uiOutput("resultUI"),
                  shiny::uiOutput("histAndSliderUI"),
                  mmints::citationUI("citations")$output
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
                 test = input$test, conf.level = input$conf.level, seed = seed,
                 na_rm = input$na_rm)
    })

    # Generate UI for results
    output$resultUI <- shiny::renderUI({
      shiny::req(results())
      list(
        shiny::h3("Test Statistic Results"),
        shiny::tableOutput("result_table_stat"),
        shiny::h3("Bootstrap Distribution of Test Statistic"),
        shiny::plotOutput("hist_stat"),
        shiny::sliderInput("bins_stat", "Number of bins:", min = 10, max = 50, value = 30),
        shiny::br(),  # Add a line break
        shiny::br(),  # Add a line break
        shiny::h3("Effect Size Results"),
        shiny::tableOutput("result_table_effect"),
        shiny::h3("Bootstrap Distribution of Effect Size"),
        shiny::plotOutput("hist_effect"),
        shiny::sliderInput("bins_effect", "Number of bins:", min = 10, max = 50, value = 30),
        shiny::br(),  # Add a line break
        shiny::br(),  # Add a line break
        shiny::h3("Method Explanation"),
        shiny::uiOutput("method_explanation")
      )
    })

    # Display result in a table for test statistic
    output$result_table_stat <- shiny::renderTable({
      shiny::req(results())
      res <- results()
      df_res <- data.frame(
        Test_Statistic = res$orig.stat,
        CI_Lower = res$ci.stat[1],
        CI_Upper = res$ci.stat[2],
        P_Value = res$p.value
      )
      names(df_res) <- c("Test Statistic", "CI (Lower)", "CI (Upper)", "P-value")
      df_res
    }, row.names = FALSE)

    # Display histogram for test statistic
    output$hist_stat <- shiny::renderPlot({
      shiny::req(results())
      res <- results()
      ggplot2::ggplot(data.frame(x = res$bootstrap.stat.dist), ggplot2::aes(x)) +
        ggplot2::geom_histogram(color = "black", fill = "white", bins = input$bins_stat) +
        ggplot2::geom_vline(xintercept = c(res$ci.stat), color = "red", linetype = "dashed") +
        ggplot2::geom_vline(xintercept = res$orig.stat, color = "blue") +
        ggplot2::labs(x = "Bootstrap Distribution", y = "Frequency",
                      title = "Histogram of Bootstrap Test Statistic Distribution: Original (Observed) Statistic and Bootstrap Confidence Intervals Indicated")
    })

    # Display result in a table for effect size
    output$result_table_effect <- shiny::renderTable({
      shiny::req(results())
      res <- results()
      df_res <- data.frame(
        Effect_Size = res$effect.size,
        CI_Lower = res$ci.effect.size[1],
        CI_Upper = res$ci.effect.size[2]
      )
      names(df_res) <- c("Effect Size", "CI (Lower)", "CI (Upper)")
      df_res
    }, row.names = FALSE)

    # Display histogram for effect size
    output$hist_effect <- shiny::renderPlot({
      shiny::req(results())
      res <- results()
      ggplot2::ggplot(data.frame(x = res$bootstrap.effect.dist), ggplot2::aes(x)) +
        ggplot2::geom_histogram(color = "black", fill = "white", bins = input$bins_effect) +
        ggplot2::geom_vline(xintercept = c(res$ci.effect.size), color = "red", linetype = "dashed") +
        ggplot2::geom_vline(xintercept = res$effect.size, color = "blue") +
        ggplot2::labs(x = "Bootstrap Distribution", y = "Frequency",
                      title = "Histogram of Bootstrap Effect Size Distribution: Original (Observed) Effect Size and Bootstrap Confidence Intervals Indicated")
    })

    output$method_explanation <- shiny::renderUI({
      if (input$test == "t") {
        explanation_text <- paste0(
          "<p>This analysis applies a nonparametric bootstrap independent t-test with pooled resampling, as described in Dwivedi et al. (2017). ",
          "Bootstrap resampling is used to approximate the sampling distribution of the t-statistic. ",
          "The 'Bootstrap Distribution' plots depict the distribution of the bootstrap-resampled t-statistic and the bootstrap-resampled effect size. ",
          "The vertical lines in these plots represent the statistic or effect size calculated from the original sample (blue) and the bootstrap confidence intervals (red). ",
          "The p-value is calculated as the proportion of bootstrap-resampled statistics that are as extreme or more extreme than the observed statistic. ",
          "The 'Effect Size' is a measure of the strength of the observed effect. For the independent t-test, it is the raw difference in means.</p>",
          "<p>Dwivedi AK, Mallawaarachchi I, Alvarado LA (2017). 'Analysis of small sample size studies using nonparametric bootstrap test with pooled resampling method.' Statistics ",
          "in Medicine, 36 (14), 2187-2205. https://doi.org/10.1002/sim.7263</p>"
        )
      } else if (input$test == "pt") {
        explanation_text <- paste0(
          "<p>This analysis applies a nonparametric bootstrap paired t-test with pooled resampling, as described in Dwivedi et al. (2017). ",
          "Bootstrap resampling is used to approximate the sampling distribution of the t-statistic. ",
          "The 'Bootstrap Distribution' plots depict the distribution of the bootstrap-resampled t-statistic and the bootstrap-resampled effect size. ",
          "The vertical lines in these plots represent the statistic or effect size calculated from the original sample (blue) and the bootstrap confidence intervals (red). ",
          "The p-value is calculated as the proportion of bootstrap-resampled statistics that are as extreme or more extreme than the observed statistic. ",
          "The 'Effect Size' is a measure of the strength of the observed effect. For the paired t-test, it is the raw mean difference of pairs.</p>",
          "<p>Dwivedi AK, Mallawaarachchi I, Alvarado LA (2017). 'Analysis of small sample size studies using nonparametric bootstrap test with pooled resampling method.' Statistics ",
          "in Medicine, 36 (14), 2187-2205. https://doi.org/10.1002/sim.7263</p>"
        )
      } else {  # input$test == "F"
        explanation_text <- paste0(
          "<p>This analysis applies a nonparametric bootstrap F-test with pooled resampling, as described in Dwivedi et al. (2017). ",
          "Bootstrap resampling is used to approximate the sampling distribution of the F-statistic. ",
          "The 'Bootstrap Distribution' plots depict the distribution of the bootstrap-resampled F-statistic and the bootstrap-resampled effect size. ",
          "The vertical lines in these plots represent the statistic or effect size calculated from the original sample (blue) and the bootstrap confidence intervals (red). ",
          "The p-value is calculated as the proportion of bootstrap-resampled statistics that are as extreme or more extreme than the observed statistic. ",
          "The 'Effect Size' is a measure of the strength of the observed effect. For ANOVA, the effect size is eta-squared, which represents the proportion of total variance in the dependent variable that can be attributed to the independent variable.</p>",
          "<p>Dwivedi AK, Mallawaarachchi I, Alvarado LA (2017). 'Analysis of small sample size studies using nonparametric bootstrap test with pooled resampling method.' Statistics ",
          "in Medicine, 36 (14), 2187-2205. https://doi.org/10.1002/sim.7263</p>"
        )
      }
      shiny::HTML(explanation_text)
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
