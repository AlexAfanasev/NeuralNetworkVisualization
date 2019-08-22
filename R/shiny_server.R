#' Returns the shiny app server.
#'
#' @import shiny
#' @import shinyWidgets
#' @importFrom plotly renderPlotly
#' @keywords internal
server <- function (session, input, output) {
    variables <- reactiveValues(model = NULL)

    Dataset <- reactive({
        infile <- input$datafile

        if (is.null(infile)) {
            return(NULL)
        } else {
            model <- readRDS(infile$datapath)
            variables$model <- model
            return(list(data = model$neural_network$data,
                        variables =  model$neural_network$covariate))
        }
    })

    output$networkplotting <- renderUI({
        if (identical(Dataset()$data, '') ||
            identical(Dataset()$data, data.frame())) {
            return(NULL)
        }

        columns <- colnames(Dataset()$variables)

        tagList(
            h4("Select predictors", style = "color:blue"),
            pickerInput(
                inputId = "plotting",
                label = "Select variables for plotting",
                choices = columns,
                options = list(
                    `actions-box` = TRUE,
                    size = 10,
                    `selected-text-format` = "count > 3"),
                multiple = TRUE),
            br(),
            h4("Customize bootstrap confidence interval", style = "color:blue"),
            fluidRow(
                column(width = 6, numericInput(
                    "lower",
                    "Select lower quantile for bootstrap confidence interval",
                    value = 0.05,min = 0, max = 1,  step = 0.01)),
                column(width = 6, numericInput(
                    "upper",
                    "Select upper quantile for bootstrap confidence interval",
                    value = 0.95, min = 0, max = 1, step = 0.01))),
            br(),
            numericInput("nrepetitions",
                         "Select number of repetitions for bootstrap confidence
                         interval", value = 50))
    })

    plot <- eventReactive(input$go, {
        if (!is.null(input$plotting) && !is.null(variables$model)) {
            print("start plotting")
            start <- Sys.time()

            my_plot <- plot_partial_dependencies(
                variables$model, input$plotting, type = "ggplotly",
                probs = c(input$lower, input$upper),
                nrepetitions = input$nrepetitions)

            end <- Sys.time()
            print(paste("Finished plotting, Duration: ", end - start))
            return(my_plot)
        } else {
            print("empty plot")
            return()
        }
    })

    output$plot <- renderPlotly({
        plot()
    })
}

