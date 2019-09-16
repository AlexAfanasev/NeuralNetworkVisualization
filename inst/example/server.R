library(NeuralNetworkVisualization)
library(shiny)
library(shinyWidgets)
library(plotly)
library(waiter)
library(tools)

function(session, input, output){
    variables <- reactiveValues(model = NULL, is_file_selected = NULL)

    output$networkupload <- renderUI({
        variables$model <- NULL
        if (input$model_setting == "Upload .rds file") {
            fileInput("datafile", "Choose NeuralNetwork File",
                      multiple = FALSE, accept = ".rds",
                      placeholder = "Nothing uploaded")
        } else if (input$model_setting == "Use model from R global env.") {
            global_env_variables <- ls(envir = globalenv())
            check_for_model_variables <- sapply(
                global_env_variables, function(x){is(get(x), "NeuralNetwork")})
            if (any(check_for_model_variables)) {
                pickerInput(
                    inputId = "model_choice",
                    label = "Select NeuralNetwork from global environment",
                    choices = global_env_variables[
                        check_for_model_variables],
                    options = list(
                        `actions-box` = TRUE,
                        size = 10,
                        `selected-text-format` = "count > 3",
                        title = "Select a NeuralNetwork"),
                    multiple = FALSE)
            } else {
                sendSweetAlert(
                    session = session,
                    title = "Error",
                    text = "There is no NeuralNetwork in the global
                    environment!", type = "error"
                )
            }
        } else if (input$model_setting == "Use example model") {
            pickerInput(
                inputId = "model_example",
                label = "Select NeuralNetwork from examples",
                choices = c("categorical", "binary", "numerical"),
                options = list(
                    `actions-box` = TRUE,
                    size = 10,
                    `selected-text-format` = "count > 3",
                    title = "Select a NeuralNetwork"),
                multiple = FALSE)
        }
    })

    observeEvent(input$model_choice, {
        if (!is.null(input$model_choice) & !all(input$model_choice == "")) {
            variables$model <- get(input$model_choice)
        }
    })

    observeEvent(input$datafile, {
        infile <- input$datafile
        model <- readRDS(infile$datapath)
        if (is(model, "NeuralNetwork")) {
            variables$model <- model
        } else {
            sendSweetAlert(
                session = session,
                title = "Error",
                text = "The .rds file does not contain a NeuralNetwork!",
                type = "error"
            )
        }
    })

    observeEvent(input$model_example, {
        if (!is.null(input$model_example) & !all(input$model_example == "")) {
            variables$model <- example_nn_model(input$model_example)
        }
    })

    output$networkplotting1 <- renderUI({
        data_set_data <- variables$model$neural_network$data
        if (identical(data_set_data, '') || is.null(data_set_data) ||
            identical(data_set_data, data.frame())) {
            return(NULL)
        } else {
            columns <- colnames(variables$model$neural_network$covariate)

            tagList(
                h3("2. Visualization Settings", style = "color:blue"),
                prettySwitch(
                    inputId = "stored_data", label = "Use stored data",
                    status = "info", fill = TRUE
                ),
                uiOutput("networkplotting2"))
        }
    })

    output$networkplotting2 <- renderUI({
        if (is.null(variables$model)) {
            return(NULL)
        } else {
            model <- variables$model
            columns <- colnames(variables$model$neural_network$covariate)
            if (isTRUE(input$stored_data)) {
                if (is.null(model$stored_data)) {
                    sendSweetAlert(
                        session = session,
                        title = "Error",
                        text = "The NeuralNetwork has no stored data!",
                        type = "error"
                    )
                } else {
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
                        tags$div(
                            tags$b(tags$p(
                                paste("Number of bootstrap repetitions: ",
                                      model$options$nrepetitions, sep = "")
                                )), br(),
                            tags$b(tags$p(
                                paste("Lower quantile: ",
                                      model$options$probs[1], sep = ""))), br(),
                            tags$b(tags$p(
                                paste("Upper quantile: ",
                                      model$options$probs[2], sep = ""))), br()
                        ),
                        actionBttn("go", "Press to plot!")
                    )
                }
            } else {
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
                    prettySwitch(
                        inputId = "bootstrap",
                        label = "Add Confidence Intervals",
                        status = "info", fill = TRUE),
                    uiOutput("networkbootstrap"),
                    actionBttn("go", "Press to plot!")
                )
            }
        }
    })

    output$networkbootstrap <- renderUI({
        if (input$bootstrap) {
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

                numericInput("nrepetitions",
                             "Select number of repetitions for
                             bootstrap confidence interval", value = 50),

                awesomeCheckbox(
                    inputId = "parallel",
                    label = "Use parallel computation",
                    value = FALSE)
                )
        }
    })

    observeEvent(input$go, {
        if (is.character(input$plotting)) {
            if (input$bootstrap) {
                if (!(sum(c(input$lower, input$upper)) == 1) || any(
                    c(input$lower, input$upper) <= 0)) {
                    sendSweetAlert(
                        session = session,
                        title = "Error",
                        text = "The lower and upper levels have to sum up to 1
                        and each one has to be bigger than 0!", type = "error"
                    )
                    return(NULL)
                }
                if (input$nrepetitions < 2) {
                    sendSweetAlert(
                        session = session,
                        title = "Error",
                        text = "The number of repetitions has to be at
                        least 2!", type = "error"
                    )
                    return(NULL)
                }
            }

            show_waiter(
                tagList(
                    spin_three_bounce(),
                    span("Loading ... Might take a while ...",
                         style = "color:white;"))
            )

            if (input$bootstrap) {
                my_plot <- plot_partial_dependencies(
                    variables$model, input$plotting, type = "ggplotly",
                    probs = c(input$lower, input$upper),
                    nrepetitions = input$nrepetitions,
                    parallel = input$parallel)
            } else if (input$stored_data) {
                my_plot <- plot_partial_dependencies(
                    variables$model, input$plotting, type = "ggplotly",
                    use_stored_data = TRUE)
            } else {
                my_plot <- plot_partial_dependencies(
                    variables$model, input$plotting, type = "ggplotly")
            }

            output$plot <- renderPlotly({
                my_plot
            })

            updateTabsetPanel(session, "inTabset", "panel3")
            hide_waiter()
        } else {
            sendSweetAlert(
                session = session,
                title = "Error",
                text = "Please specify variables for plotting!", type = "error"
            )
        }
    })
}
