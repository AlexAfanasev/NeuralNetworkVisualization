library(shiny)

server <- shinyServer(
    function (session, input, output) {
        variables <- reactiveValues(neuralnet = NULL)

        Dataset <- reactive({
            infile <- input$datafile
            if (is.null(infile)) {
                return(NULL)
            } else {
                neural_network <- readRDS(infile$datapath)
                variables$neuralnet <- neural_network

                return(neural_network$neural_network$data)
            }
        })

        output$networkplotting <- renderUI({
            if (identical(Dataset(), '') ||
                identical(Dataset(), data.frame())) {
                return(NULL)
            }
            columns <- names(Dataset())
            checkboxGroupInput(
                "plotting", "Variables for plotting",
                choices = c("all",columns))
        })

        output$plot <- plotly::renderPlotly({
            if (!is.null(input$plotting) & !is.null(variables$neuralnet)) {
                print("start plotting")
                my_plot <- plot_partial_dependencies(
                    variables$neuralnet, input$plotting, type = "ggplotly",
                    probs = c(0.05, 0.95))
                print("finished plotting")
                return(my_plot)
            } else {
                print("empty plot")
                return()
            }
        })
    }
)
