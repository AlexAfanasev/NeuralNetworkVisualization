#' Returns the about panel.
#'
#' @keywords internal
#' @import shiny
#' @import shinyWidgets

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
                    variables =  model$neural_network$covariate))
      }
    })
    
    output$networkplotting <- shiny::renderUI({
      if (identical(Dataset()$data, '') ||
          identical(Dataset()$data, data.frame())) {
        return(NULL)
      }
      columns <- colnames(Dataset()$variables)
      shiny::tagList(
      shiny::h4("Select predictors", style = "color:blue"),
      shinyWidgets::pickerInput(
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
      shiny::br(),
      shiny::h4("Customize bootstrap confidence interval", style = "color:blue"),
      shiny::fluidRow(
      column(width = 6,
      shiny::numericInput("lower", "Select lower quantile for 
                    bootstrap confidence interval", value = 0.1, 
                    min = 0, max = 1, step = 0.01 )),
      column(width = 6,
      shiny::numericInput("upper", "Select upper quantile for 
                    bootstrap confidence interval", value = 0.9, 
                    min = 0, max = 1, step = 0.01))),
      shiny::br(),
      shiny::numericInput("nrepetitions", "Select number of repetitions for 
                     bootstrap confidence interval", value = 300)
     
        )
    })
    
    plot <- shiny::eventReactive(
          input$go,{
          if (!is.null(input$plotting) & !is.null(variables$neuralnet)) {
          print("start plotting")
          my_plot <- plot_partial_dependencies(
            variables$neuralnet, input$plotting, type = "ggplotly",
            probs = c(input$lower, input$upper), 
            nrepetitions = input$nrepetitions)
          print("finished plotting")
          return(my_plot)
        } else {
          print("empty plot")
          return()
        }
          }
    )
    output$plot <- plotly::renderPlotly({
      plot()
    })
  }

