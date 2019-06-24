#Example copied from the internet
#https://stackoverflow.com/questions/18762962/passing-variable-names-to-model-in-shiny-app

library(shiny)

ui <- shinyUI(pageWithSidebar(
  headerPanel("Test Shiny App"),
  
  sidebarPanel(
    selectInput("dependent", "Dependent Variable:", c("x","y","z")),
    uiOutput("independent")
  ),
  
  mainPanel(tableOutput("regTab"))
))

dat <- data.frame(x=rnorm(100),y=rnorm(100),z=rnorm(100))

server <- shinyServer(function(input, output, session) {
  
  output$independent <- renderUI({
    checkboxGroupInput("independent", "Independent Variables:",names(dat)[!names(dat) %in% input$dependent],names(dat)[!names(dat) %in% input$dependent])
  })
  
  runRegression <- reactive({
    lm(as.formula(paste(input$dependent," ~ ",paste(input$independent,collapse="+"))))#,data=dat, #layers = c(1,1)
    # works with lm, but not with NeuralNetwork
  })
  
  output$regTab <- renderTable({
    if(!is.null(input$independent)){
      summary(runRegression())$coef #response for nn
    } else {
      print(data.frame(Warning="Please select Model Parameters."))
    }
  })
  
})

shinyApp(ui,server)