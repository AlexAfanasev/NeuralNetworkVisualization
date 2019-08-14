#' Returns the about panel.
#'
#' @import shiny
#' @import shinyWidgets
#' @importFrom plotly renderPlotly
#' @keywords internal
server <- function (session, input, output) {
    variables <- shiny::reactiveValues(neuralnet = NULL)

    Dataset <- shiny::reactive({
      infile <- input$datafile
      if (is.null(infile)) {
        return(NULL)
      } else {
        neural_network <- readRDS(infile$datapath)
        variables$neuralnet <- neural_network

        return(list(data = neural_network$neural_network$data,
                    response = neural_network$dependent))
      }
    })

    output$networkplotting <- renderUI({
      if (identical(Dataset()$data, '') ||
          identical(Dataset()$data, data.frame())) {
        return(NULL)
      }
      columns <- names(Dataset()$data)
      columns <- columns[columns != Dataset()$response]
      tagList(
      h4("Select predictors", style = "color:blue"),
      pickerInput(
          inputId = "plotting",
          label = "Select variables for plotting",
          choices = columns,
          options = list(
            `actions-box` = TRUE,
            size = 10,
            `selected-text-format` = "count > 3"
          ),
          multiple = TRUE
        ),
      br(),
      h4("Customize bootstrap confidence interval",
         style = "color:blue"),
      fluidRow(
      column(width = 6,
      numericInput("lower", "Select lower quantile for
                    bootstrap confidence interval", value = 0.1,
                    min = 0, max = 1, step = 0.01 )),
      column(width = 6,
      numericInput("upper", "Select upper quantile for
                    bootstrap confidence interval", value = 0.9,
                    min = 0, max = 1, step = 0.01))),
      br(),
      numericInput("nrepetitions", "Select number of repetitions for
                     bootstrap confidence interval", value = 50))
    })

    plot <- eventReactive(
        input$go,{
            if (!is.null(input$plotting) & !is.null(variables$neuralnet)) {
                print("start plotting")
                start <- Sys.time()
                my_plot <- plot_partial_dependencies(
                    variables$neuralnet, input$plotting, type = "ggplotly",
                    probs = c(input$lower, input$upper),
                    nrepetitions = input$nrepetitions)
                end <- Sys.time()
                print(paste("finished plotting, Duration: ", end - start))
                return(my_plot)
            } else {
                print("empty plot")
                return()
            }
        }
    )

    output$plot <- renderPlotly({
        plot()
    })
}

