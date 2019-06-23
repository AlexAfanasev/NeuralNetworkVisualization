library(shiny)
library(NeuralNetworkVisualization)

### ui#######
ui <- shinyUI(pageWithSidebar(
    headerPanel("CSV Data explorer"),
    
    sidebarPanel(
 
    fileInput("datafile", "Choose CSV File",
                multiple = TRUE,
                accept = c("text/csv",
                           "text/comma-separated-values,text/plain",
                           ".csv")),
      
    checkboxInput("header", "Header", TRUE),
      
      
    radioButtons("sep", "Separator",
                   choices = c(Comma = ",",
                               Semicolon = ";",
                               Tab = "\t"),
                   selected = ","),
    
    radioButtons("classreg", "Classification or Regression",
                   choices = c(Classification = "categorical",
                               Regression = "numerical" ),
                   selected = "numerical"),

    uiOutput("varselect"),
      
    br()  
      
      
    ),
    
    
    mainPanel(
      
   
    plotOutput("plot")
      
    )
    
  )
)
### server ####

server <- shinyServer(function(session,input, output) {
    Dataset <- reactive ({
    infile <- input$datafile
    if (is.null(infile)) {
      
      return(NULL)
    }
    read.csv(infile$datapath)
    })
  
    output$varselect <- renderUI ({
    
          if (identical(Dataset(), '') || identical(Dataset(),data.frame())) {
              return(NULL)
          }
    cols <- names(Dataset())
    
    tagList(
    
    selectInput("dependent", "Select a  dependent variable:",choices = cols, selected = NULL),
    
    checkboxGroupInput("independent", "Select independent variables" , choices = cols,
             selected = NULL),
    
    checkboxGroupInput("predictor", "Independent variables for training the Neural Network",
            choices = c("all",cols)),
    
    numericInput("layer1", "Neural network layer 1", value = 1, min = 1, max = NA),
    
    numericInput("layer2", "Neural network layer 2", value = 1, min = 1, max = NA),
    
    radioButtons("scale", "Scale neural network", choices = c(True = T, False = F),
       selected = T)
    )
    })
        
  
  
neuralnet <- reactive ({
    if (input$classreg == "categorical") {
       regressand  = paste(levels(Dataset()[[input$dependent]]),collapse = "+")
    } else {
     regressand = input$dependent
    }
# alternative to paste, did not yield success    
    predictors = paste(input$independent, collapse = "+")
    fml = as.formula(sprintf('%s ~ %s', regressand, predictors))
    NeuralNetwork(fml, Dataset(), layers = c(input$layer1, input$layer2), scale =T)
    })

  
output$table <- renderPlot ({
    plot = plot_partial_dependencies(neural_net(), input$predictor)
    return(plot)
  })
  
  
})

#### app ##########                       

shinyApp(ui, server)