library(NeuralNetworkVisualization)
library(shiny)
library(shinyWidgets)
library(plotly)
library(waiter)
library(tools)

function(session, input, output){
    variables <- reactiveValues(model = NULL)

    Dataset <- reactive({
        validate(
            need(input$datafile != "",
                 "Please select a NeuralNetwork.rds file!"))
        validate(
            need(file_ext(input$datafile) == "rds",
                 "Please select a .rds file!"))

        infile <- input$datafile
        model <- readRDS(infile$datapath)
        variables$model <- model
        return(list(data = model$neural_network$data,
                    variables =  model$neural_network$covariate))
    })

    output$networkplotting <- renderUI({
        data_set_data <- Dataset()$data
        if (identical(data_set_data, '') || is.null(data_set_data) ||
            identical(data_set_data, data.frame())) {
            return(NULL)
        } else {
            columns <- colnames(Dataset()$variables)

            tagList(
                h3("2. Visualization Settings", style = "color:blue"),
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
                checkboxInput(inputId = "bootstrap",
                              label = "Add Confidence Intervals"),
                br(),
                uiOutput("networkbootstrap"),
                actionBttn("go", "Press to plot!"))
        }
    })

    output$networkbootstrap <- renderUI({
        if (input$bootstrap) {
            return(
                tagList(
                    h4("Customize bootstrap confidence interval",
                       style = "color:blue"),
                    fluidRow(
                        column(width = 6, numericInput(
                            "lower",
                            "Select lower quantile for bootstrap
                            confidence interval", value = 0.05, min = 0,
                            max = 1, step = 0.01)),
                        column(width = 6, numericInput(
                            "upper",
                            "Select upper quantile for bootstrap
                            confidence interval", value = 0.95, min = 0,
                            max = 1, step = 0.01))),
                    br(),
                    numericInput("nrepetitions",
                                 "Select number of repetitions for
                                 bootstrap confidence interval", value = 50)))
        }
    })

    plot <- observeEvent(input$go, {
        if (is.character(input$plotting)) {
            show_waiter(
                tagList(
                    spin_three_bounce(),
                    span("Loading ... Might take a while ...",
                         style = "color:white;"))
            )

            print("start plotting")
            start <- Sys.time()

            if (input$bootstrap) {
                my_plot <- plot_partial_dependencies(
                    variables$model, input$plotting, type = "ggplotly",
                    probs = c(input$lower, input$upper),
                    nrepetitions = input$nrepetitions)
            } else {
                my_plot <- plot_partial_dependencies(
                    variables$model, input$plotting, type = "ggplotly")
            }

            output$plot <- renderPlotly({
                my_plot
            })

            end <- Sys.time()

            print(paste("Finished plotting, Duration: ", end - start))

            updateTabsetPanel(session, "inTabset", "panel3")
            hide_waiter()
        } else {
            showNotification("Please specify variables for plotting!",
                             type = "error")
            return()
        }
    })
}
