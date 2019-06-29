library(shiny)
library(NeuralNetworkVisualization)

### ui#######
ui <- shinyUI(
    pageWithSidebar(

        headerPanel("CSV Data explorer"),

        sidebarPanel(
            fileInput("datafile", "Choose CSV File",
                      multiple = FALSE,
                      accept = c("text/csv", "text/plain")),
            checkboxInput("header", "Header", TRUE),
            radioButtons("sep", "Separator",
                         choices = c(Comma = ",",
                                     Semicolon = ";",
                                     Tab = "\t"),
                         selected = ",")
        ),

        mainPanel(
            tabsetPanel(type = "tabs",
                        tabPanel("Neural Network Settings",
                                 br(),
                                 uiOutput("networktraining")),
                        tabPanel("Plot",
                                 br(),
                                 uiOutput("networkplotting"),
                                 plotly::plotlyOutput("plot"))
            )
        )
    )
)

### server ####
server <- shinyServer(
    function (session,input, output) {
        variables <- reactiveValues(neuralnet = NULL)

        Dataset <- reactive({
            infile <- input$datafile
            if (is.null(infile)) {
                return(NULL)
            } else {
                return(read.csv(infile$datapath, header = input$header,
                                sep = input$sep, stringsAsFactors = TRUE))
            }
        })


        output$networktraining <- renderUI({
            if (identical(Dataset(), '') ||
                identical(Dataset(), data.frame())) {
                return(NULL)
            }

            columns <- names(Dataset())
            tagList(
                selectInput("dependent", "Select a  dependent variable:",
                            choices = columns),
                checkboxGroupInput(
                    "predictor",
                    "Independent variables for training the Neural Network",
                    choices = c("all",columns)),
                numericInput("layer1", "Neural network layer 1",
                             value = 1, min = 1, max = NA),
                numericInput("layer2", "Neural network layer 2",
                             value = 1, min = 1, max = NA),
                radioButtons("scale", "Scale neural network",
                             choices = c(True = T, False = F), selected = T),
                actionButton("fit", "Train Neural Network!")
            )
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

        observeEvent(input$fit, {
            if (input$predictor == "all") {
                independent <- names(Dataset())[
                    names(Dataset()) != input$dependent]
            } else {
                independent <- input$predictor
            }
            formula <- paste(input$dependent,
                             paste(independent, collapse = " + "),
                             sep = " ~ ")
            print(formula)
            set.seed(1)
            variables$neuralnet <- NeuralNetwork(
                f = formula, data = Dataset(),
                layers = c(5, 3), scale = TRUE,
                linear.output = TRUE)
        })

        # neuralnet <- reactive({
        #     if (input$classreg == "categorical") {
        #         regressand  = paste(levels(Dataset()[[input$dependent]]),
        #                             collapse = "+")
        #     } else {
        #         regressand = input$dependent
        #     }
        #
        #     # alternative to paste, did not yield success
        #     predictors <- paste(input$independent, collapse = "+")
        #     fml <- as.formula(sprintf('%s ~ %s', regressand, predictors))
        #     NeuralNetwork(fml, Dataset(),
        #                   layers = c(input$layer1, input$layer2), scale = T)
        # })

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

#### app ##########
shinyApp(ui, server)
