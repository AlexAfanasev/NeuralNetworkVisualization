library(shiny)
library(shinyWidgets)
library(NeuralNetworkVisualization)

### ui  #######
ui <- fluidPage(
    titlePanel("Marginal Effects for Neural Networks"),
    br(),
    sidebarPanel(
### upload file 
    h4("Upload Data", style = "color:blue"),
    fileInput("datafile", "Choose CSV File",
                multiple = TRUE,
                accept = c("text/csv",
                           "text/comma-separated-values, text/plain",
                           ".csv")),
## accept header
    h4("Customize Data", style = "color:blue"),
    fluidRow(
    column(4,
    awesomeCheckbox( "header", "Header", value = T)),
### choose separator 
    column(4,
    radioButtons("sep", "Separator",
                   choices = c(Comma = ",",
                               Semicolon = ";",
                               Tab = "\t"),
                   selected = ",")),
### choose type of future neural network 
    column(4,
    radioButtons("classreg", "Classification or Regression",
                   choices = c(Classification = "categorical",
                               Regression = "numerical" ),
                   selected = "numerical"))),
    br(),
### UIs as helper for reactive selection
    h4("Select variables for the neural network", style = "color:blue"),
    fluidRow(
    column(5, overwrite = 2,
    uiOutput("select_dependent")),
    column(5,
    uiOutput("select_independent"))),
    br(),
    h4("Customize neural network", style = "color:blue"),
    uiOutput("select_nn")
    ),
### output plot    
    mainPanel(
    plotOutput("plot")
    )
  )


### server ####
server <- shinyServer(function(session,input, output) {
### reactive data  
    Dataset <- reactive ({
    infile <- input$datafile
    if (is.null(infile)) {
      return(NULL)
    }
    read.csv(infile$datapath, header = input$header, sep = input$sep)
    })
### select dependent variable based on variables contained in the data 
    output$select_dependent <- renderUI ({
    if (identical(Dataset(), '') || identical(Dataset(),data.frame())) {
        return(NULL)
    }
    cols <- names(Dataset())
    selectInput("dependent", "Select a  dependent variable:", choices = cols, selected = NULL)
    })
### reactive independent variable  
    independent_var <- reactive({
    cols <- names(Dataset())
    select_independent <- cols[cols != input$dependent]
    return(select_independent)
    })
### select independent variables for neural network based on dependent variable 
    output$select_independent <- renderUI({
    pickerInput(
      inputId = "independent", 
      label = "Select independent variables", 
      choices = independent_var(), 
      options = list(
        `actions-box` = TRUE, 
        size = 10,
        `selected-text-format` = "count > 3"
      ), 
      multiple = TRUE
      )})
    
### reactive predictor for marginal effect based on available independent variables    
    predictor_var <- reactive({
    select_predictor <- input$independent 
     return(select_predictor)
    })
### select parameters for neural network based on predictors    
    output$select_nn <- renderUI({
### select the predictors 
    fluidRow(
    column(6, overwrite = 1,
    pickerInput(
      inputId = "predictor", 
      label = "Select predictor variables", 
      choices = predictor_var(), 
      options = list(
        `actions-box` = TRUE, 
        size = 10,
        `selected-text-format` = "count > 3"
      ), 
      multiple = TRUE
      )),
    column(5,
    tagList(
### select layers for neural network    
    numericInput("layer1", "Neural network layer 1", value = 1, min = 1, max = NA),
    numericInput("layer2", "Neural network layer 2", value = 1, min = 1, max = NA),
### option to scale neural network    
    radioButtons("scale", "Scale neural network", choices = c(True = T, False = F),
                 selected = T))))
})
### create formula for neural network   
    neuralnet <- reactive ({
    if (input$classreg == "categorical") {
       regressand  = paste(levels(Dataset()[[input$dependent]]), collapse = "+")
    } else {
     regressand = input$dependent
    }
    predictors = paste(input$independent, collapse = "+")
    fml = as.formula(sprintf('%s ~ %s', regressand, predictors))
    NeuralNetwork(fml, Dataset(), layers = c(input$layer1, input$layer2), scale = input$scale)
    })
### output plot
output$plot <- renderPlot ({
    plot <- plot_partial_dependencies(neural_net(), input$predictor)  
    return(plot)
  })
})

### app ####                   

shinyApp(ui, server)